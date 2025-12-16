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

type Tick = Option[(Option[Char], VMState)]

case class VMState(pc: Adr, registers: Registers, stack: Stack, memory: Memory):
  import Opcode.*

  def noOutput: Tick = Some(None -> this)
  def output(w: Word): Tick = Some(Some(w.asChar) -> this)

  def op: Opcode = memory(pc).op
  def a: Word = memory(pc.inc1)
  def b: Word = memory(pc.inc2)
  def c: Word = memory(pc.inc3)

  def nextInstruction: Adr = op.numParams match
    case 0 => pc.inc1
    case 1 => pc.inc2
    case 2 => pc.inc3
    case 3 => pc.inc4

  def progress: VMState = this.copy(pc = nextInstruction)
  def jump(a: Adr): VMState = this.copy(pc = a)

  def store(r: Reg, v: Lit): VMState = this.copy(registers = registers.updated(r, v))
  def deref(r: Reg): Lit = registers(r).lit
  extension (w: Word)
    def value: Lit = if w.fitsU15 then deref(w.reg) else w.lit

  def push(w: Adr | Lit): VMState = this.copy(stack = w :: stack)
  def pop: Option[(Word, VMState)] =
    stack.headOption.map(_ -> this.copy(stack = stack.tail))

  def read(a: Adr): Word = memory(a)
  def write(a: Adr, v: Lit) = this.copy(memory = memory.updated(a, v))

  def step: Tick = op match
    case HALT => None
    case SET => store(a.reg, b.value).progress.noOutput
    case PUSH => push(a.value).progress.noOutput // TODO: can we dereference a?
    case POP => this.pop match
      case Some(w -> s) => s.store(a.reg, w.value).progress.noOutput
      case None => ???
    case EQ =>
      val x: Lit = if b.value == c.value then 1.toLit else 0.toLit
      store(a.reg, x).noOutput
    case GT =>
      val x: Lit = if b.value > c.value then 1.toLit else 0.toLit
      store(a.reg, x).noOutput
    case JMP => jump(a.adr).noOutput
    case JT =>
      if a.value != 0.toLit then this.copy(pc = b.adr).noOutput
      else this.progress.noOutput
    case JF =>
      if a.value == 0.toLit then this.copy(pc = b.adr).noOutput
      else this.progress.noOutput
    case ADD => this.store(a.reg, b.value + c.value).progress.noOutput
    case MULT => this.store(a.reg, b.value * c.value).progress.noOutput
    case MOD => this.store(a.reg, b.value % c.value).progress.noOutput
    case AND => this.store(a.reg, b.value & c.value).progress.noOutput
    case OR => this.store(a.reg, b.value | c.value).progress.noOutput
    case NOT => this.store(a.reg, ~(b.value)).progress.noOutput
    case RMEM => this.store(b.reg, read(a.adr).lit).progress.noOutput
    case WMEM => this.write(a.adr, b.value).progress.noOutput
    case CALL => this.push(nextInstruction).jump(a.adr).noOutput
    case RET => this.pop.flatMap:
      case (w, s) => s.jump(w.adr).noOutput
    case OUT => this.progress.output(a)
    case IN => ???
    case NOOP => this.progress.noOutput
