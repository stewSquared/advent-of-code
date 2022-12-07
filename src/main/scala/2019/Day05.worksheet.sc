val input = io.Source.fromResource("2019/day-05.txt").getLines().next()
val intcode = input.split(',').map(_.toInt).toVector

enum Ops:
  case Add, Mul, Save, Out, JumpTrue, JumpFalse, LT, EQ, Halt

import Ops.*

object Ops:
  def fromInst(inst: Int) =
    if inst == 99 then Ops.Halt else Ops.fromOrdinal(inst % 100 - 1)

case class State(memory: Vector[Int], ip: Int):
  def step(input: Int): Option[(Option[Int], State)] =
    val inst =  memory(ip)
    val opcode = memory(ip) % 100
    def value(pos: Int): Int =
      val param = memory(ip + 1 + pos)
      val mode = (inst / 100).toString.reverse.padTo(3, '0')(pos)
      if mode == '0' then memory(param) else param

    Ops.fromInst(inst) match
      case Add =>
        val x = value(0)
        val y = value(1)
        val addr = memory(ip + 3)
        Some(
          None -> copy(
            memory = memory.updated(addr, x + y),
            ip = ip + 4
          )
        )
      case Mul =>
        val x = value(0)
        val y = value(1)
        val addr = memory(ip + 3)
        Some(
          None -> copy(
            memory = memory.updated(addr, x * y),
            ip = ip + 4
          )
        )
      case Save =>
        val addr = memory(ip + 1)
        Some(
          None -> copy(
            memory = memory.updated(addr, input),
            ip = ip + 2
          )
        )
      case Out =>
        val addr = memory(ip + 1)
        val output: Option[Int] = Some(memory(addr))
        Some(output -> copy(ip = ip + 2))
      case JumpTrue =>
        val x = value(0)
        val addr = value(1)
        if x != 0 then Some(None -> copy(ip = addr))
        else Some(None -> copy(ip = ip + 3))
      case JumpFalse =>
        val x = value(0)
        val addr = value(1)
        if x == 0 then Some(None -> copy(ip = addr))
        else Some(None -> copy(ip = ip + 3))
      case LT =>
        val x = value(0)
        val y = value(1)
        val addr = memory(ip + 3)
        val result = if x < y then 1 else 0
        Some(None -> copy(memory = memory.updated(addr, result), ip + 4))
      case EQ =>
        val x = value(0)
        val y = value(1)
        val addr = memory(ip + 3)
        val result = if x == y then 1 else 0
        Some(None -> copy(memory = memory.updated(addr, result), ip + 4))
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
run(Vector(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), 1) // LOOOP

// run(intcode, input = 5)

val ans1 = run(intcode, input = 1).last.get
val ans2 = run(intcode, input = 5).last.get

// State(Vector(1002,4,3,4,33), 0).step

// run(Vector(1002,4,3,4,33))
