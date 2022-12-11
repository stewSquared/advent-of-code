val input = io.Source.fromResource("2019/day-15.txt").getLines().next()
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

enum Dir:
  case N, S, W, E

enum Output:
  case Wall, Moved, Found

case class Point(x: Int, y: Int):
  def move(dir: Dir): Point = dir match
    case Dir.N => copy(y = y + 1)
    case Dir.S => copy(y = y - 1)
    case Dir.W => copy(x = x - 1)
    case Dir.E => copy(x = x + 1)

  def adj = Dir.values.map(move)

// TODO: walls map may be unnecessary
case class DroidState(
    pos: Point,
    walls: Map[Point, Boolean],
    oxy: Option[Point],
    pc: State
):
  def step(dir: Dir): DroidState =
    val (pcOut, nextPc) = pc.nextOutput(dir.ordinal + 1).get
    val out = Output.fromOrdinal(pcOut.toInt)
    val ahead = pos.move(dir)
    out match
      case Output.Wall =>
        copy(walls = walls.updated(ahead, true), pc = nextPc)
      case Output.Moved =>
        copy(pos = ahead, walls = walls.updated(ahead, false), pc = nextPc)
      case Output.Found =>
        copy(
          pos = ahead,
          walls = walls.updated(ahead, false),
          oxy = Some(ahead),
          pc = nextPc
        )

def move(state: State, dir: Dir): (Output, State) =
  val (pcOut, next) = state.nextOutput(dir.ordinal + 1).get
  val output = Output.fromOrdinal(pcOut.toInt)
  output -> next

import collection.mutable.PriorityQueue

val cost = collection.mutable.Map[Point, Int](Point(0, 0) -> 0)
val toVisit = PriorityQueue.empty[Point](Ordering.by(cost))
val state = collection.mutable.Map[Point, State](Point(0, 0) -> State(intcode*))

var visiting = Point(0, 0)
var oxy: Option[Point] = None

while oxy.isEmpty do
  for
    dir <- Dir.values
    ahead = visiting.move(dir)
    if !cost.contains(ahead)
    (output, next) = move(state(visiting), dir)
  do output match
    case Output.Wall =>
      cost(ahead) = Int.MaxValue
    case Output.Moved =>
      cost(ahead) = cost(visiting) + 1
      state(ahead) = next
      toVisit.enqueue(ahead)
    case Output.Found =>
      cost(ahead) = cost(visiting) + 1
      state(ahead) = next
      oxy = Some(ahead)

  visiting = toVisit.dequeue()

val ans1 = oxy.map(cost)

val xRange = cost.keys.map(_.x).min to cost.keys.map(_.x).max
val yRange = cost.keys.map(_.y).min to cost.keys.map(_.y).max

for y <- yRange do
  for x <- xRange do
    val p = Point(x, y)
    if p == Point(0, 0) then print('S')
    else if oxy.contains(p) then print('O')
    else
      cost.get(p) match
        case Some(Int.MaxValue) => print('#')
        case Some(_)            => print('.')
        case None               => print(' ')
  println()

val ans2 =
  var minutes = 0
  var filledLast = Set(oxy.get)
  var filled = Set(oxy.get)
  while filledLast.nonEmpty do
    val fillingNow = collection.mutable.Set.empty[Point]
    for
      fillingFrom <- filledLast
      dir <- Dir.values
      ahead = fillingFrom.move(dir)
      if !cost.get(ahead).exists(_ == Int.MaxValue)
      if !filled(ahead)
      (output, next) = move(state(fillingFrom), dir)
    do output match
      case Output.Wall =>
        cost(ahead) = Int.MaxValue
      case Output.Moved | Output.Found =>
        state(ahead) = next
        filled += ahead
        fillingNow += ahead

    filledLast = fillingNow.toSet
    minutes += 1 // TODO: fix later, counts one extra

  minutes

//
