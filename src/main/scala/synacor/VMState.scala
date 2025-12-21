package synacor

import synacor.numbers.*

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
  import Inst.*
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
    ch => this.store(a.reg, ch.toLit).progress.ready

  def halt: Tick = Tick.Halt(code = ExitCode.Success)

  def op: Opcode = memory(pc).op
  def a: Word = memory(pc.inc1)
  def b: Word = memory(pc.inc2)
  def c: Word = memory(pc.inc3)

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
    s"@${pc.hex}: ${inst.show}"

  def inst = Inst.currentInstruction(this)

  import Inst.*
  def tick(using IsReady[P]): Tick = inst match
    case HALT => halt
    case SET(a, b) => store(a.reg, b.value).progress.noOutput
    case PUSH(a) => push(a.value).progress.noOutput
    case POP(a) => pop match
      case Some(word -> state) => state.updateAgain: state =>
          state.store(a.reg, LitArg.fromWord(word).value)
        .progress.noOutput
      case None => Tick.Halt(ExitCode.EmptyStack)
    case EQ(a, b, c) =>
      val x: Lit = if b.value == c.value then 1.toLit else 0.toLit // boolean tolit?
      store(a.reg, x).progress.noOutput
    case GT(a, b, c) =>
      val x: Lit = if b.value > c.value then 1.toLit else 0.toLit
      store(a.reg, x).progress.noOutput
    case JMP(a) => jump(a.value).noOutput
    case JT(a, b) =>
      if a.value != 0.toLit then this.jump(b.value).noOutput
      else this.progress.noOutput
    case JF(a, b) =>
      if a.value == 0.toLit then this.jump(b.value).noOutput
      else this.progress.noOutput
    case ADD(a, b, c) => this.store(a.reg, b.value + c.value).progress.noOutput
    case MULT(a, b, c) => this.store(a.reg, b.value * c.value).progress.noOutput
    case MOD(a, b, c) => this.store(a.reg, b.value % c.value).progress.noOutput
    case AND(a, b, c) => this.store(a.reg, b.value & c.value).progress.noOutput
    case OR(a, b, c) => this.store(a.reg, b.value | c.value).progress.noOutput
    case NOT(a, b) => this.store(a.reg, ~b.value).progress.noOutput
    case RMEM(a, b) => this.store(a.reg, read(b.value).lit).progress.noOutput
    case WMEM(a, b) => this.write(a.value, b.value).progress.noOutput
    case CALL(a) => this.push(nextInstruction).jump(a.value).noOutput
    case RET => this.pop match
      case Some((w, s)) => s.jump(AdrArg.fromWord(w).value).noOutput
      case None         => this.halt
    case OUT(a) => this.progress.output(a.value)
    case IN(a) => this.input
    case NOOP => this.progress.noOutput
