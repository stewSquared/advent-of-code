package synacor

import synacor.numbers.*

enum Opcode:
  case HALT, SET
  case PUSH, POP
  case EQ, GT
  case JMP, JT, JF
  case ADD, MULT, MOD
  case AND, OR, NOT
  case RMEM, WMEM
  case CALL, RET
  case OUT, IN
  case NOOP

  def numParams: Int = this match
    case EQ | GT | ADD | MULT | MOD | AND | OR => 3
    case SET | JT | JF | NOT | RMEM | WMEM => 2
    case PUSH | POP | JMP | CALL | OUT | IN => 1
    case _ => 0

case class Registers(underlying: IArray[U15]):
  require(underlying.sizeIs == 8)
  def apply(r: Reg): U15 = underlying(r.toIndex)

  def updated(r: Reg, u15: U15): Registers =
    Registers(underlying.updated(r.toIndex, u15))

object Registers:
  def apply(
    r0: U15,
    r1: U15,
    r2: U15,
    r3: U15,
    r4: U15,
    r5: U15,
    r6: U15,
    r7: U15
  ): Registers = Registers.apply(IArray(r0, r1, r2, r3, r4, r5, r6, r7))
  def init: Registers =
    Registers.apply(IArray.fill(8)(Word.fromInt(0).u15))

type Stack = List[Word]
// type Memory = Map[Adr, Word]
case class Memory(underlying: Vector[Word]):
  def apply(a: Adr): Word = underlying(a.toIndex)
  def updated(a: Adr, w: Word): Memory =
    Memory(underlying.updated(a.toIndex, w))

sealed trait Phase
class Ready extends Phase
class Updated extends Phase
class Moved extends Phase

type IsReady[P <: Phase] = P <:< Ready
type CanMove[P <: Phase] = P <:< (Ready | Updated)
type HasMoved[P <: Phase] = P <:< Moved

enum Tick:
  case Halt(code: ExitCode)
  case Output(c: Char, state: VMState[Ready])
  case Input(f: Char => VMState[Ready])
  case Continue(state: VMState[Ready])

  def isBlocked = this match
    case _: (Halt | Input) => true
    case _: (Output | Continue) => false

enum ExitCode:
  case Success, EmptyStack

case class VMState[P <: Phase](pc: Adr, registers: Registers, stack: Stack, memory: Memory):
  import Opcode.*
  private given Registers = this.registers

  private def unsafeSetReady: VMState[Ready] = this.asInstanceOf[VMState[Ready]]
  def ready(using HasMoved[P]): VMState[Ready] = this.asInstanceOf[VMState[Ready]]

  def updateAgain(f: VMState[Ready] => VMState[Updated])(using P =:= Updated): VMState[Updated] =
    f(this.unsafeSetReady)

  def noOutput(using HasMoved[P]): Tick =
    Tick.Continue(this.ready)

  def output(n: Lit)(using HasMoved[P]): Tick =
    Tick.Output(n.asChar, this.ready)

  def input(using ev: IsReady[P]): Tick = Tick.Input:
    ch => this.store(aReg, ch.toLit).progress.ready

  def halt: Tick = Tick.Halt(code = ExitCode.Success)

  def op: Opcode = memory(pc).op
  def aArg[A <: U15 : Cast]: Arg[A] = Arg.fromWord[A](memory(pc.inc1))
  def bArg[A <: U15 : Cast]: Arg[A] = Arg.fromWord[A](memory(pc.inc2))
  def cArg[A <: U15 : Cast]: Arg[A] = Arg.fromWord[A](memory(pc.inc3))

  def aReg: Reg = memory(pc.inc1).reg
  def bReg: Reg = memory(pc.inc2).reg
  def cReg: Reg = memory(pc.inc3).reg

  def a[A <: U15 : Cast]: A = aArg.value
  def b[A <: U15 : Cast]: A = bArg.value
  def c[A <: U15 : Cast]: A = cArg.value

  def nextInstruction: Adr = op.numParams match
    case 0 => pc.inc1
    case 1 => pc.inc2
    case 2 => pc.inc3
    case 3 => pc.inc4

  def progress(using CanMove[P]): VMState[Moved] = this.copy(pc = nextInstruction)
  def jump(a: Adr)(using CanMove[P]): VMState[Moved] = this.copy(pc = a)

  def store(r: Reg, v: Lit)(using IsReady[P]): VMState[Updated] = this.copy(registers = registers.updated(r, v))

  def push(w: U15)(using IsReady[P]): VMState[Updated] = this.copy(stack = w :: stack)
  def pop(using IsReady[P]): Option[(Word, VMState[Updated])] =
    stack.headOption.map(_ -> this.copy(stack = stack.tail))

  def read(a: Adr): Word = memory(a)
  def write(a: Adr, v: Lit)(using IsReady[P]): VMState[Updated] = this.copy(memory = memory.updated(a, v))

  def showInst(using IsReady[P]): String =
    val args = op match
      case PUSH       => s"${aArg[Lit].show}"
      case POP | IN   => s"${aReg.name}"
      case SET        => s"${aReg.name} ${aArg[Lit].show}"
      case JMP | CALL => s"${aArg[Adr].show}"
      case JT | JF    => s"${aArg[Lit].show} ${bArg[Adr].show}"
      case NOT | RMEM => s"${aReg.name} ${bArg[Lit].show}"
      case WMEM       => s"${aArg[Adr].show} ${bArg[Lit].show}"
      case OUT        => aArg[Lit] match
        case Arg.RegRef(reg) => s"${reg.name}(${cast[Char](registers(reg))})"
        case Arg.Const(v) => v.asChar
      case GT | EQ | ADD | MULT | MOD | AND | OR =>
        s"${aReg.name} ${bArg[Lit].show} ${cArg[Lit].show}"
      case NOOP | RET | HALT => ""
    s"@${pc.hex}: $op $args"

  def tick(using IsReady[P]): Tick = op match
    case HALT => this.halt
    case SET => store(aReg, b).progress.noOutput
    case PUSH => push(a[U15]).progress.noOutput // TODO: can we dereference a?
    case POP => this.pop match
      case Some(word -> state) => state.updateAgain: state =>
          state.store(aReg, Arg.fromWord(word).value)
        .progress.noOutput
      case None => Tick.Halt(ExitCode.EmptyStack)
    case EQ =>
      val x: Lit = if b[Lit] == c[Lit] then 1.toLit else 0.toLit // boolean tolit?
      store(aReg, x).progress.noOutput
    case GT =>
      val x: Lit = if b[Lit] > c[Lit] then 1.toLit else 0.toLit
      store(aReg, x).progress.noOutput
    case JMP => jump(a).noOutput
    case JT =>
      if a[Lit] != 0.toLit then this.jump(b).noOutput
      else this.progress.noOutput
    case JF =>
      if a[Lit] == 0.toLit then this.jump(b).noOutput
      else this.progress.noOutput
    case ADD => this.store(aReg, b[Lit] + c[Lit]).progress.noOutput
    case MULT => this.store(aReg, b[Lit] * c[Lit]).progress.noOutput
    case MOD => this.store(aReg, b[Lit] % c[Lit]).progress.noOutput
    case AND => this.store(aReg, b[Lit] & c[Lit]).progress.noOutput
    case OR => this.store(aReg, b[Lit] | c[Lit]).progress.noOutput
    case NOT => this.store(aReg, ~b[Lit]).progress.noOutput
    case RMEM => this.store(aReg, read(b).lit).progress.noOutput
    case WMEM => this.write(a, b).progress.noOutput
    case CALL => this
      .push(nextInstruction)
      .jump(a).noOutput
    case RET => this.pop match
      case Some((w, s)) => s.jump(Arg.fromWord(w).value).noOutput
      case None         => this.halt
    case OUT => this.progress.output(a)
    case IN => this.input
    case NOOP => this.progress.noOutput
