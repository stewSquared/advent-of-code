val input = io.Source.fromResource("2019/day-13.txt").getLines().next()
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

enum Tile:
  case Empty, Wall, Block, Paddle, Ball

case class GameState(grid: Map[Point, Tile], score: Int, pcState: State, ballX: Option[Int] = None, paddleX: Option[Int] = None):
  def step(input: Int): Option[GameState] =
    for
      (x, s1) <- pcState.nextOutput(input)
      (y, s2) <- s1.nextOutput(input)
      (id, s3) <- s2.nextOutput(input)
    yield (x.toInt, y.toInt) match
      case (-1, 0) =>
        val score = id.toInt
        copy(score = score, pcState = s3)
      case (x, y) =>
        val p = Point(x, y)
        val tile = Tile.fromOrdinal(id.toInt)
        tile match
          case Tile.Ball => copy(grid = grid.updated(p, tile), pcState = s3, ballX = Some(x))
          case Tile.Paddle => copy(grid = grid.updated(p, tile), pcState = s3, paddleX = Some(x))
          case _ => copy(grid = grid.updated(p, tile), pcState = s3)

val start = GameState(Map.empty, 0, State(intcode*))

val finalGrid = LazyList.unfold(start)(_.step(0).map(s => s.grid -> s)).last

val ans = finalGrid.toList.collect {
  case (point, Tile.Block) => point
}.distinct.size

val withQuarters = intcode.updated(0, 2L)

val gameStart = GameState(Map.empty, 0, State(withQuarters*))

def scores = LazyList.unfold(gameStart) {
  case gs@GameState(grid, score, pcState, bx, px) =>
    val direction = for
      paddleX <- px
      ballX <- bx
    yield (ballX - paddleX).sign
    gs.step(direction.getOrElse(0)).map(s => s.score -> s)
}

val ans2 = scores.last
