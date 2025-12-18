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

type Tick = Option[(Option[Char], VMState[Ready])]

case class VMState[P <: Phase](pc: Adr, registers: Registers, stack: Stack, memory: Memory, input: List[Char] = Nil):
  import Opcode.*

  private def unsafeSetReady: VMState[Ready] = this.asInstanceOf[VMState[Ready]]
  def updateAgain(f: VMState[Ready] => VMState[Updated])(using P =:= Updated): VMState[Updated] =
    f(this.unsafeSetReady)

  def noOutput(using HasMoved[P]): Tick = Some(None -> this.unsafeSetReady)
  def output(w: Word)(using HasMoved[P]): Tick =
    Some(Some(deref(w).asChar) -> this.unsafeSetReady)

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
  def deref(w: Word): Word = if w.fitsU15 then w else registers(w.reg)

  extension (w: Word) def value: Lit = deref(w).lit
  extension (w: Word) def address: Adr = deref(w).adr

  def push(w: Adr | Lit)(using IsReady[P]): VMState[Updated] = this.copy(stack = w :: stack)
  def pop(using IsReady[P]): Option[(Word, VMState[Updated])] =
    stack.headOption.map(_ -> this.copy(stack = stack.tail))

  def read(a: Adr): Word = memory(a)
  def write(a: Adr, v: Lit)(using IsReady[P]): VMState[Updated] = this.copy(memory = memory.updated(a, v))

  def step(using IsReady[P]): Tick = op match
    case HALT => None
    case SET => store(a.reg, b.value).progress.noOutput
    case PUSH => push(a.value).progress.noOutput // TODO: can we dereference a?
    case POP => this.pop match
      case Some(w -> s) => s.updateAgain(_.store(a.reg, w.value)).progress.noOutput
      case None => ???
    case EQ =>
      val x: Lit = if b.value == c.value then 1.toLit else 0.toLit // boolean tolit?
      store(a.reg, x).progress.noOutput
    case GT =>
      val x: Lit = if b.value > c.value then 1.toLit else 0.toLit
      store(a.reg, x).progress.noOutput
    case JMP => jump(a.address).noOutput
    case JT =>
      if a.value != 0.toLit then this.jump(b.address).noOutput
      else this.progress.noOutput
    case JF =>
      if a.value == 0.toLit then this.jump(b.address).noOutput
      else this.progress.noOutput
    case ADD => this.store(a.reg, b.value + c.value).progress.noOutput
    case MULT => this.store(a.reg, b.value * c.value).progress.noOutput
    case MOD => this.store(a.reg, b.value % c.value).progress.noOutput
    case AND => this.store(a.reg, b.value & c.value).progress.noOutput
    case OR => this.store(a.reg, b.value | c.value).progress.noOutput
    case NOT => this.store(a.reg, ~(b.value)).progress.noOutput
    case RMEM => this.store(a.reg, read(b.address).lit).progress.noOutput
    case WMEM => this.write(a.address, b.value).progress.noOutput
    case CALL => this
      .push(nextInstruction)
      .jump(a.address).noOutput
    case RET => this.pop.flatMap:
      case (w, s) => s.jump(w.address).noOutput
    case OUT => this.progress.output(a)
    case IN =>
      input match
        case c :: cs =>
          print(c)
          this.store(a.reg, c.toLit).copy(input = cs).progress.noOutput
        case Nil =>
          val nextLine = io.StdIn.readLine("input: ") + "\n"
          this.copy[Ready](input = nextLine.toList).step
    case NOOP => this.progress.noOutput
