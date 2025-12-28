package synacor

import synacor.numbers.*

case class Registers(underlying: IArray[U15]):
  require(underlying.sizeIs == 8)
  def apply(r: Reg): U15 = underlying(r.toIndex)

  def updated(r: Reg, w: U15): Registers =
    Registers(underlying.updated(r.toIndex, w))

object Registers:
  def init: Registers =
    Registers.apply(IArray.fill(8)(0.toU15))

case class Stack(underlying: List[U15]):
  def push(v: U15): Stack = Stack(v :: underlying)
  def pop: Option[(U15, Stack)] = underlying match
    case v :: vs => Some(v -> Stack(vs))
    case Nil => None

object Stack:
  def init: Stack = Stack(Nil)

case class Memory(underlying: Vector[Word]):
  def apply(a: Adr): Word = underlying(a.toIndex)
  def updated(a: Adr, v: U15): Memory =
    Memory(underlying.updated(a.toIndex, Word.fromU15(v)))

sealed trait Phase
final class Ready extends Phase
final class Finished extends Phase
final class Moved extends Phase

type IsReady[P <: Phase] = P <:< Ready
type IsFinished[P <: Phase] = P <:< Finished
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

  inline def ready(using HasMoved[P]): VMState[Ready] = this.asInstanceOf[VMState[Ready]]

  inline def noUpdate(using IsReady[P]): VMState[Finished] = this.asInstanceOf[VMState[Finished]]

  def noOutput(using HasMoved[P]): Tick =
    Tick.Continue(this.ready)

  def output(n: Lit)(using HasMoved[P]): Tick =
    Tick.Output(n.toChar, this.ready)

  def input(reg: Reg)(using ev: IsReady[P]): Tick = Tick.Input:
    ch => this.store(reg, ch.toLit).progress.ready

  def halt(code: ExitCode): Tick = Tick.Halt(code)

  val op: Opcode = Opcode.parse(memory(pc))

  val inst: Inst = Inst.parse(op,
    a = memory(pc.inc1),
    b = memory(pc.inc2),
    c = memory(pc.inc3)
  )

  def nextInstruction: Adr = op.numParams match
    case 0 => pc.inc1
    case 1 => pc.inc2
    case 2 => pc.inc3
    case 3 => pc.inc4

  def progress(using IsFinished[P]): VMState[Moved] = this.copy[Moved](pc = nextInstruction)
  def jump(a: Adr)(using IsFinished[P]): VMState[Moved] = this.copy[Moved](pc = a)

  def store(r: Reg, v: U15)(using IsReady[P]): VMState[Finished] =
    this.copy[Finished](registers = registers.updated(r, v))

  def push(v: U15)(using IsReady[P]): VMState[Finished] =
    this.copy[Finished](stack = stack.push(v))

  def popAdr(using IsReady[P]): Option[(Adr, VMState[Finished])] =
    stack.pop.map((v, s) => v.asAdr -> this.copy[Finished](stack = s))

  def popToReg(r: Reg)(using IsReady[P]): Option[VMState[Finished]] =
    stack.pop.map((v, s) => this.copy[Finished](registers = registers.updated(r, v), stack = s))

  def read(a: Adr): U15 = U15.parse(memory(a))
  def write(a: Adr, v: Lit)(using IsReady[P]): VMState[Finished] = this.copy[Finished](memory = memory.updated(a, v))

  def showInst(using IsReady[P]): String =
    s"@${pc.hex}: ${inst.show(using registers, memory)}"

  given Registers = this.registers

  def checkR8(inst: Inst): Unit =
    val instStr = inst.show(using registers, memory)
    if instStr.contains("R8") then
      println(s"R8 accessed @${pc.hex}: $instStr")

  import util.chaining.*

  def tick(using IsReady[P]): Tick = inst.tap(checkR8) match
    case HALT => halt(ExitCode.Success)
    case SET(a, b) => store(a, b.lit).progress.noOutput
    case PUSH(a) => push(a.lit).progress.noOutput
    case POP(a) => popToReg(a) match
      case Some(s) => s.progress.noOutput
      case None => halt(code = ExitCode.EmptyStack)
    case EQ(a, b, c) =>
      val x: Lit = if b.lit == c.lit then 1.toLit else 0.toLit // boolean tolit?
      store(a, x).progress.noOutput
    case GT(a, b, c) =>
      val x: Lit = if b.lit > c.lit then 1.toLit else 0.toLit
      store(a, x).progress.noOutput
    case JMP(a) => noUpdate.jump(a.adr).noOutput
    case JT(a, b) =>
      if a.lit != 0.toLit then noUpdate.jump(b.adr).noOutput
      else noUpdate.progress.noOutput
    case JF(a, b) =>
      if a.lit == 0.toLit then noUpdate.jump(b.adr).noOutput
      else noUpdate.progress.noOutput
    case ADD(a, b, c) => store(a, b.lit + c.lit).progress.noOutput
    case MULT(a, b, c) => store(a, b.lit * c.lit).progress.noOutput
    case MOD(a, b, c) => store(a, b.lit % c.lit).progress.noOutput
    case AND(a, b, c) => store(a, b.lit & c.lit).progress.noOutput
    case OR(a, b, c) => store(a, b.lit | c.lit).progress.noOutput
    case NOT(a, b) => store(a, ~(b.lit)).progress.noOutput
    case RMEM(a, b) => store(a, read(b.adr)).progress.noOutput
    case WMEM(a, b) => write(a.adr, b.lit).progress.noOutput
    case CALL(a) if a == Arg.Const(0x178B.toAdr) =>
      println("HACKERMANN enabled")
      this.store(Reg.R1, 6.toLit).progress.noOutput // hack to set R1 to 6 before calling 0x178B
    case CALL(a) => push(nextInstruction).jump(a.adr).noOutput
    case RET => popAdr match
      case Some((a, s)) => s.jump(a).noOutput
      case None         => halt(ExitCode.Success)
    case OUT(a) => noUpdate.progress.output(a.lit)
    case IN(a) => input(a)
    case NOOP => noUpdate.progress.noOutput

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
