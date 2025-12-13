"Hello, Synacor!"

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

// val x = 0xFF
// 0b100
// 1234L

case class Num(low: Byte, high: Byte):
  def asShort: Short = ((high << 8) + low).toShort
  def asInt: Int = asShort.toInt
  def asChar: Char = ((high << 8) + low).toChar
  def increment: Num = copy(low = (low + 1).toByte) // TODO overflow

  infix def +(n: Num): Num = Num((n.low + low).toByte, (n.high + high).toByte) // TODO carry
  infix def +(n: Int): Num = Num((n & 0xFF + low).toByte, (n & 0xFF00).toByte) // TODO carry

  infix def *(n: Num): Num = Num.fromInt:
    (asInt * n.asInt) // TODO verify signed works

  infix def %(n: Num): Num = Num.fromInt:
    (asInt % n.asInt) // TODO verify signed works


  infix def &(n: Num): Num =
    Num((n.low & low).toByte, (n.high & high).toByte)

  infix def |(n: Num): Num =
    Num((n.low | low).toByte, (n.high | high).toByte)

  infix def >(n: Num): Boolean = asInt > n.asInt

  def not: Num = Num.fromInt(~asInt)

object Num: // TODO Integral typeclass
  // implicit conversions?
  def fromShort(s: Short): Num =
    Num(low = (s & 0xFF).toByte, high = (s & 0xFF00).toByte)

  def fromInt(n: Int): Num =
    Num(low = (n & 0xFF).toByte, high = (n & 0xFF00).toByte)


  // infix def not(n: Num): Num =
  //   Num(n.low | low, n.high | high)


  // TODO: implicit conversions?

// type Num = Int

extension (n: Int)
  def toNum: Num = Num.fromInt(n)

case class Registers(
  a: Num,
  b: Num,
  c: Num,
  d: Num,
  e: Num,
  f: Num,
  g: Num,
  h: Num
):
  val values = Array(a,b,c,d,e,f,g,h)
  def apply(i: Int): Num = values(i)

type Stack = List[Num]
type Memory = Map[Num, Num]

case class State(pc: Num, registers: Registers, stack: Stack, memory: Memory):

  def progress(op: Opcode): State = copy(pc = pc + 1 + op.numParams)

  def noOutput = Some(None -> this)
  def output(n: Num) = Some(Some(n.asChar) -> this)

  // register? constant? address?
  // TODO memory/register safety
  def a: Num = memory(pc + 1)
  def b: Num = memory(pc + 2)
  def c: Num = memory(pc + 3)

  def store(pos: Num, v: Num) = this.copy(memory = memory.updated(pos, v))

  def step(op: Opcode): Option[(Option[Char], State)] = op match
    case Opcode.HALT => None
    case Opcode.SET => this.noOutput
    case Opcode.PUSH => this.noOutput
    case Opcode.POP => this.noOutput
    case Opcode.EQ =>
      val x = if b == c then 1.toNum else 0.toNum
      store(a, x).noOutput
    case Opcode.GT =>
      val x = if b > c then 1.toNum else 0.toNum
      store(a, x).noOutput
    case Opcode.JMP => this.copy(pc = a).noOutput
    case Opcode.JT =>
      if a != 0.toNum then this.copy(pc = b).noOutput
      else this.progress(op).noOutput
    case Opcode.JF =>
      if a == 0.toNum then this.copy(pc = b).noOutput
      else this.progress(op).noOutput
    case Opcode.ADD => this.store(a, b + c).progress(op).noOutput
    case Opcode.MULT => this.store(a, b * c).progress(op).noOutput
    case Opcode.MOD => this.store(a, b % c).progress(op).noOutput
    case Opcode.AND => this.store(a, b & c).progress(op).noOutput
    case Opcode.OR => this.store(a, b | c).progress(op).noOutput
    case Opcode.NOT => this.store(a, b.not).progress(op).noOutput
    case Opcode.RMEM => this.store(b, a).progress(op).noOutput // TODO register check?
    case Opcode.WMEM => this.store(a, b).progress(op).noOutput // TODO register check?
    case Opcode.CALL => this.noOutput
    case Opcode.RET => this.noOutput
    case Opcode.OUT => this.output(a) // TODO progress
    case Opcode.IN => this.noOutput
    case Opcode.NOOP => this.progress(op).noOutput


// TODO test overflows

// val binary = io.source


//
