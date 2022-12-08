val input = io.Source.fromResource("2019/day-09.txt").getLines().next()
val intcode = input.split(',').map(_.toInt).toVector.map(_.toLong)

enum Opcode:
  case Add, Mul, Save, Out, JumpTrue, JumpFalse, LT, EQ, Rel, Halt

  def numParams: Int = this match
    case Add | Mul | LT | EQ => 3
    case Save | Out | Rel => 1
    case JumpTrue | JumpFalse => 2
    case Halt => 0

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
      case Mode.Imm => ??? // Parameters that an instruction writes to will never be in immediate mode.
      case Mode.Rel => addrParam + relativeBase

  def progress(op: Opcode): State = copy(ip = ip + 1 + op.numParams)
  def write(value: Long, op: Opcode): State = copy(memory.updated(writeAddress(op), value))
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

object State:
  def apply(longs: Long*): State =
    val memory = IntMap[Long](longs.zipWithIndex.map(_.swap)*)
    apply(memory)

  def apply(memory: Map[Int, Long]): State =
    new State(memory.withDefaultValue(0L), ip = 0, relativeBase = 0)

def run(initialMemory: Vector[Long], input: Int): Vector[Option[Long]] =
  run(State(initialMemory*), input)

def run(initialState: State, input: Int): Vector[Option[Long]] =
  LazyList
    .unfold(initialState)(_.step(input))
    .toVector

intcode
intcode.indexOf(2107L)

val ans1 = run(intcode, input = 1).last.get
val ans2 = run(intcode, input = 2).last.get

// tests

run(
  Vector[Long](109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006,
    101, 0, 99),
  0
).flatten

val s = State(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101,
  0, 99)

s
s.step(0).get._2
s.step(0).get._2.step(0).get._2
s.step(0).get._2.step(0).get._2.step(0).get._2
s.step(0).get._2.step(0).get._2.step(0).get._2.step(0).get._2

run(Vector[Long](1102, 34915192, 34915192, 7, 4, 7, 99, 0), 0)
run(Vector(104, 1125899906842624L, 99), 0)

run(Vector[Long](3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8), 8)
run(Vector[Long](3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8), 9)

run(Vector[Long](3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8), 7)
run(Vector[Long](3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8), 8)

run(Vector[Long](3, 3, 1108, -1, 8, 3, 4, 3, 99), 8)
run(Vector[Long](3, 3, 1108, -1, 8, 3, 4, 3, 99), 9)

run(Vector[Long](3, 3, 1107, -1, 8, 3, 4, 3, 99), 7)
run(Vector[Long](3, 3, 1107, -1, 8, 3, 4, 3, 99), 8)

run(Vector[Long](3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9), 0)
run(Vector[Long](3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9), 1)
