val input = io.Source.fromResource("2019/day-11.txt").getLines().next()
val intcode = input.split(',').map(_.toLong).toVector

enum Opcode:
  case Add, Mul, Save, Out, JumpTrue, JumpFalse, LT, EQ, Rel, Halt

  def numParams: Int = this match
    case Add | Mul | LT | EQ  => 3
    case Save | Out | Rel     => 1
    case JumpTrue | JumpFalse => 2
    case Halt                 => 0

import Opcode.*

enum Mode:
  case Pos, Imm, Rel

object Mode:
  def apply(int: Int) = Mode.fromOrdinal(int)

object Opcode:
  def fromInst(inst: Long) =
    if inst.toInt == 99 then Opcode.Halt
    else Opcode.fromOrdinal(inst.toInt % 100 - 1)

import collection.immutable.IntMap

case class State(memory: Map[Int, Long], ip: Int, relativeBase: Int = 0):
  lazy val inst = memory(ip)

  def parameterMode(pos: Int): Mode =
    Mode((inst / 100).toString.reverse.padTo(3, '0')(pos).asDigit)

  def param(pos: Int): Long = memory(ip + 1 + pos)

  def value(pos: Int): Long = parameterMode(pos) match
    case Mode.Pos => memory(param(pos).toInt)
    case Mode.Imm => param(pos)
    case Mode.Rel => memory(param(pos).toInt + relativeBase)

  def writeAddress(op: Opcode): Int =
    val addrParam = param(op.numParams - 1).toInt
    parameterMode(op.numParams - 1) match
      case Mode.Pos => addrParam
      case Mode.Imm =>
        ??? // Parameters that an instruction writes to will never be in immediate mode.
      case Mode.Rel => addrParam + relativeBase

  def progress(op: Opcode): State = copy(ip = ip + 1 + op.numParams)
  def write(value: Long, op: Opcode): State = copy(
    memory.updated(writeAddress(op), value)
  )
  def jump(addr: Int): State = copy(ip = addr)
  def shiftBase(by: Int): State = copy(relativeBase = relativeBase + by)

  def withOutput(output: Long) = Some(Some(output) -> this)
  def noOutput = Some(None -> this)

  def step(input: Int): Option[(Option[Long], State)] =
    val op = Opcode.fromInst(inst)
    op match
      case Add =>
        write(value(0) + value(1), op).progress(op).noOutput
      case Mul =>
        write(value(0) * value(1), op).progress(op).noOutput
      case Save =>
        write(input, op).progress(op).noOutput
      case Out =>
        progress(op).withOutput(value(0))
      case JumpTrue =>
        if value(0) != 0 then jump(value(1).toInt).noOutput
        else progress(op).noOutput
      case JumpFalse =>
        if value(0) == 0 then jump(value(1).toInt).noOutput
        else progress(op).noOutput
      case LT =>
        val result = if value(0) < value(1) then 1 else 0
        write(result, op).progress(op).noOutput
      case EQ =>
        val result = if value(0) == value(1) then 1 else 0
        write(result, op).progress(op).noOutput
      case Rel =>
        shiftBase(value(0).toInt).progress(op).noOutput
      case Halt => None

  def nextOutput(input: Int): Option[(Long, State)] =
    step(input).flatMap { case (output, nextState) =>
      output.map(_ -> nextState).orElse {
        nextState.nextOutput(input)
      }
    }

object State:
  def apply(longs: Long*): State =
    val memory = IntMap[Long](longs.zipWithIndex.map(_.swap)*)
    apply(memory)

  def apply(memory: Map[Int, Long]): State =
    new State(memory.withDefaultValue(0L), ip = 0, relativeBase = 0)

case class Point(x: Int, y: Int):
  def up = copy(y = y + 1)
  def down = copy(y = y - 1)
  def right = copy(x = x + 1)
  def left = copy(x = x - 1)

// State(intcode*).step(0)
enum Dir:
  case N, S, E, W
  def right = this match
    case N => E
    case E => S
    case S => W
    case W => N

  def left = this match
    case E => N
    case S => E
    case W => S
    case N => W

  def apply(p: Point): Point = this match
    case N => p.up
    case S => p.down
    case E => p.right
    case W => p.left

case class PaintState(
    pos: Point,
    dir: Dir,
    white: Set[Point],
    intcodeState: State
):
  def paintStep(input: Int): Option[PaintState] =
    for (colorOutput, nextState) <- intcodeState.nextOutput(input)
    yield
      val nextPaint =
        if colorOutput == 1L then white.incl(pos)
        else white.excl(pos)
      copy(white = nextPaint, intcodeState = nextState)

  def moveStep(input: Int): Option[PaintState] =
    for (dirOutput, nextState) <-  intcodeState.nextOutput(input)
    yield
      val nextDir =
        if dirOutput == 1L then dir.right
        else dir.left
      copy(pos = nextDir(pos), dir = nextDir, intcodeState = nextState)

  def step: Option[PaintState] =
    val input = if white(pos) then 1 else 0
    val paint = paintStep(input)
    paint.flatMap(_.moveStep(input)).orElse {
      assert(paint.forall(_.step.isEmpty))
      paint
    }

val start = PaintState(Point(0, 0), Dir.N, white = Set.empty, State(intcode*))

def positions = LazyList.unfold(start)(s => s.step.map(s => s.pos -> s))

val ans1 = positions.toSet.size

val start2 = PaintState(
  Point(0, 0),
  Dir.N,
  white = Set(Point(0,0)),
  State(intcode*)
)

val finalState = LazyList.unfold(start2)(s => s.step.map(s => s -> s)).last

val painted = finalState.white

val xValues = painted.map(_.x)
val yValues = painted.map(_.y)

val xRange = xValues.min to xValues.max
val yRange = yValues.min to yValues.max

for y <- yRange.reverse do
  for x <- xRange do
    val p = Point(x, y)
    if painted(p) then print('#') else print('.')
  println()

val ans2 = "LBJHEKLH"
