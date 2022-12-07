val input = io.Source.fromResource("2019/day-05.txt").getLines().next()
val intcode = input.split(',').map(_.toInt).toVector

enum Ops(val numParams: Int):
  case Add extends Ops(3)
  case Mul extends Ops(3)
  case Save extends Ops(1)
  case Out extends Ops(1)
  case JumpTrue extends Ops(2)
  case JumpFalse extends Ops(2)
  case LT extends Ops(3)
  case EQ extends Ops(3)
  case Halt extends Ops(0)

import Ops.*

object Ops:
  def fromInst(inst: Int) =
    if inst == 99 then Ops.Halt else Ops.fromOrdinal(inst % 100 - 1)

case class State(memory: Vector[Int], ip: Int):
  lazy val inst =  memory(ip)

  def value(pos: Int): Int =
    val param = memory(ip + 1 + pos)
    val mode = (inst / 100).toString.reverse.padTo(3, '0')(pos)
    if mode == '0' then memory(param) else param
  def addr(op: Ops): Int = memory(ip + op.numParams)

  def progress(op: Ops): State = copy(ip = ip + 1 + op.numParams)
  def jump(addr: Int): State = copy(ip = addr)
  def write(value: Int, op: Ops): State =
    // Note: Parameters that an instruction writes to will never be in immediate mode.
    copy(memory.updated(addr(op), value))

  def withOutput(output: Int) = Some(Some(output) -> this)
  def noOutput = Some(None -> this)

  def step(input: Int): Option[(Option[Int], State)] =
    println(memory.drop(ip).take(4))
    val op = Ops.fromInst(inst)
    op match
      case Add =>
        write(value(0) + value(1), op).progress(op).noOutput
      case Mul =>
        write(value(0) * value(1), op).progress(op).noOutput
      case Save =>
        write(input, op).progress(op).noOutput
      case Out =>
        progress(op).withOutput(memory(addr(op)))
      case JumpTrue =>
        if value(0) != 0 then jump(value(1)).noOutput
        else progress(op).noOutput
      case JumpFalse =>
        if value(0) == 0 then jump(value(1)).noOutput
        else progress(op).noOutput
      case LT =>
        val result = if value(0) < value(1) then 1 else 0
        write(result, op).progress(op).noOutput
      case EQ =>
        val result = if value(0) == value(1) then 1 else 0
        write(result, op).progress(op).noOutput
      case Halt => None

def run(initialMemory: Vector[Int], input: Int): Vector[Option[Int]] =
  LazyList
    .unfold(State(initialMemory, ip = 0))(_.step(input))
    .toVector

run(Vector(3,9,8,9,10,9,4,9,99,-1,8), 8)
run(Vector(3,9,8,9,10,9,4,9,99,-1,8), 9)

run(Vector(3,9,7,9,10,9,4,9,99,-1,8), 7)
run(Vector(3,9,7,9,10,9,4,9,99,-1,8), 8)

run(Vector(3,3,1108,-1,8,3,4,3,99), 8)
run(Vector(3,3,1108,-1,8,3,4,3,99), 9)

run(Vector(3,3,1107,-1,8,3,4,3,99), 7)
run(Vector(3,3,1107,-1,8,3,4,3,99), 8)

Vector(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9).size
Vector(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9).indexOf(4)

run(Vector(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), 0)
run(Vector(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), 1)

val ans1 = run(intcode, input = 1).last.get
val ans2 = run(intcode, input = 5).last.get
