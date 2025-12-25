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
  def apply(r: Reg): Word = underlying(r.toIndex)

  def updated(r: Reg, w: U15): Registers =
    Registers(underlying.updated(r.toIndex, w))

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
    Registers.apply(IArray.fill(8)(U15.fromInt(0)))

type Stack = List[U15]

case class Memory(underlying: Vector[Word]):
  def apply(a: Adr): Word = underlying(a.toIndex)
  def updated(a: Adr, w: Word): Memory =
    Memory(underlying.updated(a.toIndex, w))

sealed trait Phase
class Ready extends Phase
class Updated extends Phase
class Moved extends Phase

type IsReady[P <: Phase] = P <:< Ready
type IsUpdated[P <: Phase] = P <:< Updated
type CanMove[P <: Phase] = P <:< (Ready | Updated)
type HasMoved[P <: Phase] = P <:< Moved

enum Tick:
  // Output and Continue.state both refer to state *after* executing the opcode
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

  private def unsafeSetReady: VMState[Ready] = this.asInstanceOf[VMState[Ready]]
  def ready(using HasMoved[P]): VMState[Ready] = this.asInstanceOf[VMState[Ready]]

  def updateAgain(f: VMState[Ready] => VMState[Updated])(using IsUpdated[P]): VMState[Updated] =
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

  def store(r: Reg, v: Lit)(using IsReady[P]): VMState[Updated] =
    this.copy(registers = registers.updated(r, v))

  def push(w: Adr | Lit)(using IsReady[P]): VMState[Updated] = this.copy(stack = w :: stack)
  def pop(using IsReady[P]): Option[(U15, VMState[Updated])] =
    stack.headOption.map(_ -> this.copy(stack = stack.tail))

  def read(a: Adr): Word = memory(a)
  def write(a: Adr, v: Lit)(using IsReady[P]): VMState[Updated] = this.copy(memory = memory.updated(a, v))

  def showInst(using IsReady[P]): String =
    s"@${pc.hex}: ${inst.show(using registers, memory)}"

  def inst: Inst = Inst.parse(op, a, b, c)

  given Registers = this.registers

  def tick(using IsReady[P]): Tick = inst match
    case HALT => halt
    case SET(a, b) => store(a.reg, b.value).progress.noOutput
    case PUSH(a) => push(a.value).progress.noOutput
    case POP(a) => pop match
      case Some(w -> s) => s.updateAgain(_.store(a.reg, Arg.parse(w).value)).progress.noOutput
      case None => Tick.Halt(code = ExitCode.EmptyStack)
    case EQ(a, b, c) =>
      val x: Lit = if b.value == c.value then 1.toLit else 0.toLit // boolean tolit?
      store(a.reg, x).progress.noOutput
    case GT(a, b, c) =>
      val x: Lit = if b.value > c.value then 1.toLit else 0.toLit
      store(a.reg, x).progress.noOutput
    case JMP(a) => jump(a.value).noOutput
    case JT(a, b) =>
      if a.value != 0.toLit then this.jump(b.value).noOutput
      else progress.noOutput
    case JF(a, b) =>
      if a.value == 0.toLit then this.jump(b.value).noOutput
      else progress.noOutput
    case ADD(a, b, c) => store(a.reg, b.value + c.value).progress.noOutput
    case MULT(a, b, c) => store(a.reg, b.value * c.value).progress.noOutput
    case MOD(a, b, c) => store(a.reg, b.value % c.value).progress.noOutput
    case AND(a, b, c) => store(a.reg, b.value & c.value).progress.noOutput
    case OR(a, b, c) => store(a.reg, b.value | c.value).progress.noOutput
    case NOT(a, b) => store(a.reg, ~(b.value)).progress.noOutput
    case RMEM(a, b) => store(a.reg, read(b.value).lit).progress.noOutput
    case WMEM(a, b) => write(a.value, b.value).progress.noOutput
    case CALL(a) if a == Arg.Const(0x178B.toAdr) =>
      println("HACKERMANN enabled")
      this.store(Reg.R1, 6.toLit).progress.noOutput // hack to set R1 to 6 before calling 0x178B
    case CALL(a) => push(nextInstruction).jump(a.value).noOutput
    case RET => pop match
      case Some((w, s)) => s.jump(Arg.parse(w).value).noOutput
      case None         => halt
    case OUT(a) => this.progress.output(a.value)
    case IN(a) => this.input
    case NOOP => this.progress.noOutput

object VMState:
  extension (vm: VMState[Ready])
    def modifyPC(f: Adr => Adr)(using Emulator.HackPerm): VMState[Ready] =
      vm.copy(pc = f(vm.pc))
    def modifyRegisters(f: Registers => Registers)(using Emulator.HackPerm): VMState[Ready] =
      vm.copy(registers = f(vm.registers))
    def modifyStack(f: Stack => Stack)(using Emulator.HackPerm): VMState[Ready] =
      vm.copy(stack = f(vm.stack))
    def modifyMemory(f: Memory => Memory)(using Emulator.HackPerm): VMState[Ready] =
      vm.copy(memory = f(vm.memory))
