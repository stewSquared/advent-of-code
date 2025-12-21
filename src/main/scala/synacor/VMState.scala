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

case class Registers(underlying: IArray[Word]):
  require(underlying.sizeIs == 8)
  def apply(r: Reg): Word = underlying(r.toIndex)

  def updated(r: Reg, w: Word): Registers =
    Registers(underlying.updated(r.toIndex, w))

object Registers:
  def apply(
    r0: Word,
    r1: Word,
    r2: Word,
    r3: Word,
    r4: Word,
    r5: Word,
    r6: Word,
    r7: Word
  ): Registers = Registers.apply(IArray(r0, r1, r2, r3, r4, r5, r6, r7))
  def init: Registers =
    Registers.apply(IArray.fill(8)(Word.fromInt(0)))

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

  private def unsafeSetReady: VMState[Ready] = this.asInstanceOf[VMState[Ready]]
  def ready(using HasMoved[P]): VMState[Ready] = this.asInstanceOf[VMState[Ready]]

  def updateAgain(f: VMState[Ready] => VMState[Updated])(using P =:= Updated): VMState[Updated] =
    f(this.unsafeSetReady)

  def noOutput(using HasMoved[P]): Tick =
    Tick.Continue(this.ready)

  def output(w: Word)(using HasMoved[P]): Tick =
    Tick.Output(deref(w).asChar, this.ready)

  def input(using ev: IsReady[P]): Tick = Tick.Input:
    ch => this.store(a.toReg, ch.toLit).progress.ready

  def halt: Tick = Tick.Halt(code = ExitCode.Success)

  def op: Opcode = memory(pc).op
  def a: Arg = Arg.fromWord(memory(pc.inc1))
  def b: Arg = Arg.fromWord(memory(pc.inc2))
  def c: Arg = Arg.fromWord(memory(pc.inc3))

  def nextInstruction: Adr = op.numParams match
    case 0 => pc.inc1
    case 1 => pc.inc2
    case 2 => pc.inc3
    case 3 => pc.inc4

  def progress(using CanMove[P]): VMState[Moved] = this.copy(pc = nextInstruction)
  def jump(a: Adr)(using CanMove[P]): VMState[Moved] = this.copy(pc = a)

  def store(r: Reg, v: Lit)(using IsReady[P]): VMState[Updated] = this.copy(registers = registers.updated(r, v))

  extension (a: Arg)
    def value: Lit = a match
      case r: Arg.RegRef => r.deref(registers).lit
      case Arg.Const(v) => v.lit

    def address: Adr = a match
      case Arg.RegRef(reg) => registers(reg).adr
      case Arg.Const(v) => v.adr

  def push(w: Adr | Lit)(using IsReady[P]): VMState[Updated] = this.copy(stack = w :: stack)
  def pop(using IsReady[P]): Option[(Word, VMState[Updated])] =
    stack.headOption.map(_ -> this.copy(stack = stack.tail))

  def read(a: Adr): Word = memory(a)
  def write(a: Adr, v: Lit)(using IsReady[P]): VMState[Updated] = this.copy(memory = memory.updated(a, v))

  def showArg(regOrLit: Word): String =
    if regOrLit.fitsU15 then regOrLit.lit.hex // 0x0002
    else s"${regOrLit.reg.name}(${deref(regOrLit).lit.hex})" // R2(0x0002)

  def showChar(regOrLit: Word): String =
    if regOrLit.fitsU15 then regOrLit.asChar.toString // 0x0002
    else s"${regOrLit.reg.name}(${deref(regOrLit).asChar.toString})" // R2(0x0002)

  def showAdr(regOrAdr: Word): String =
    if regOrAdr.fitsU15 then s"@${regOrAdr.adr.hex}" // @0x0002
    else s"${regOrAdr.reg.name}(@${deref(regOrAdr).adr.hex})" // R2(0x0002)

  def showArgs(args: String*): String =
    s"@${pc.hex}: $op ${args.mkString(" ")}"

  def show(using IsReady[P]): String = op match // todo rename show instruction
    case PUSH => showArgs(showArg(a))
    case POP | IN => showArgs(a.reg.name)
    case SET => showArgs(a.reg.name, showArg(b))
    case JMP | CALL => showArgs(showAdr(a))
    case JT | JF => showArgs(showArg(a), showAdr(b))
    case GT | EQ | ADD | MULT | MOD | AND | OR =>
      showArgs(a.reg.name, showArg(b), showArg(c))
    case NOT | RMEM => showArgs(a.reg.name, showArg(b))
    case WMEM => showArgs(showAdr(a), showArg(b))
    case OUT => showArgs(showChar(a))
    case NOOP | RET | HALT => s"@${pc.hex}: $op"

  def tick(using IsReady[P]): Tick = op match
    case HALT => this.halt
    case SET => store(a.toReg, b.value).progress.noOutput
    case PUSH => push(a.value).progress.noOutput // TODO: can we dereference a?
    case POP => this.pop match
      case Some(word -> state) => state.updateAgain: state =>
          state.store(a.toReg, Arg.fromWord(word).value)
        .progress.noOutput
      case None => Tick.Halt(ExitCode.EmptyStack)
    case EQ =>
      val x: Lit = if b.value == c.value then 1.toLit else 0.toLit // boolean tolit?
      store(a.toReg, x).progress.noOutput
    case GT =>
      val x: Lit = if b.value > c.value then 1.toLit else 0.toLit
      store(a.toReg, x).progress.noOutput
    case JMP => jump(a.address).noOutput
    case JT =>
      if a.value != 0.toLit then this.jump(b.address).noOutput
      else this.progress.noOutput
    case JF =>
      if a.value == 0.toLit then this.jump(b.address).noOutput
      else this.progress.noOutput
    case ADD => this.store(a.toReg, b.value + c.value).progress.noOutput
    case MULT => this.store(a.toReg, b.value * c.value).progress.noOutput
    case MOD => this.store(a.toReg, b.value % c.value).progress.noOutput
    case AND => this.store(a.toReg, b.value & c.value).progress.noOutput
    case OR => this.store(a.toReg, b.value | c.value).progress.noOutput
    case NOT => this.store(a.toReg, ~(b.value)).progress.noOutput
    case RMEM => this.store(a.toReg, read(b.address).lit).progress.noOutput
    case WMEM => this.write(a.address, b.value).progress.noOutput
    case CALL => this
      .push(nextInstruction)
      .jump(a.address).noOutput
    case RET => this.pop match
      case Some((w, s)) => s.jump(Arg.fromWord(w).address).noOutput
      case None         => this.halt
    case OUT => this.progress.output(a.value)
    case IN => this.input
    case NOOP => this.progress.noOutput
