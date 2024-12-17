import aoc.{Point, Area, Dir}

val input = io.Source.fromResource("2024/day-17.txt").getLines.toList

enum Opcode:
  case adv, bxl, bst, jnz, bxc, out, bdv, cdv

case class CPU(a: Long, b: Long, c: Long, ip: Int, program: Vector[Int]):
  def running: Boolean = program.indices contains ip

  def combo: Int = program(ip + 1) match
    case 0 => 0
    case 1 => 1
    case 2 => 2
    case 3 => 3
    case 4 => (a%8).toInt
    case 5 => (b%8).toInt
    case 6 => (c%8).toInt
    case 7 => ???

  def literal = program(ip + 1)
  def opcode = Opcode.fromOrdinal(program(ip))
  def advance = this.copy(ip = ip + 2)

  def next: (Option[Int], CPU) =
    val stateOut =
      opcode match
        case Opcode.adv =>
          copy(a = a / math.pow(2, combo).toInt).advance -> None
        case Opcode.bxl => copy(b = b^literal).advance -> None
        case Opcode.bst => copy(b = combo % 8).advance -> None
        case Opcode.jnz =>
          val state = if a == 0 then
            this.advance
          else copy(ip = literal)
          state -> None
        case Opcode.bxc => copy(b = b^c).advance -> None
        case Opcode.out => this.advance -> Some(combo % 8)
        case Opcode.bdv =>
          copy(b = a / math.pow(2, combo).toInt).advance -> None
        case Opcode.cdv =>
          copy(c = a / math.pow(2, combo).toInt).advance -> None
    stateOut.swap

val startState: CPU =
  val s"Register A: $a" = input(0)
  val s"Register B: $b" = input(1)
  val s"Register C: $c" = input(2)

  val s"Program: $program" = input(4)
  val programValue = program.split(",").mkString.map(_.asDigit).toVector

  CPU(a.toInt, b.toInt, c.toInt, 0, programValue)

def output(state: CPU): List[Int] =
  Iterator.unfold(state): cpu =>
    Option.when(cpu.running)(cpu.next)
  .toList.flatten

def finalState(startState: CPU): CPU =
  LazyList
    .iterate(startState)(_.next._2)
    .dropWhile(_.running)
    .head

val ans1 = output(startState).mkString(",")

CPU(a=0b110010, b=0, c=0, ip=0, startState.program)

output:
  CPU(a=0b110010, b=0, c=0, ip=0, startState.program)

output:
  CPU(a=0b000100, b=0, c=0, ip=0, startState.program)


(0 until 8).find: b =>
  output(CPU(a=b+(0<<3), b=0, c=0, ip=0, startState.program)) == List(0)

(0 until 8).map: b =>
  output(CPU(a=b+(4<<3), b=0, c=0, ip=0, startState.program))

(0 until 8).filter: b =>
  output(CPU(a=b+(4<<3), b=0, c=0, ip=0, startState.program)) == List(3, 0)

val foo1 = (0 until 8).map: b =>
  output(CPU(a=b+(5<<3)+(4<<6), b=0, c=0, ip=0, startState.program))

val foo2 = (0 until 8).map: b =>
  output(CPU(a=b+(5<<3), b=0, c=0, ip=0, startState.program))

foo1.zip(foo2) foreach println

// 4<<6 + 5<<7

def backsolve(prefix: Long, expected: Vector[Int]): List[Long] =
  (0 until 8).map(i => (i+(prefix<<3)) -> i).filter: (a, i) =>
    output(CPU(a=a, b=0, c=0, ip=0, startState.program)) == expected
  .toList.map(_._1)

backsolve(0, Vector(0))
backsolve(4, Vector(3,0))
backsolve(37, Vector(5,3,0))
backsolve(39, Vector(5,3,0))


val anss = startState.program.tails.toList.reverse.tail
  .foldLeft(List(0L)):
    (possiblePrefixes, expected) =>
      possiblePrefixes.flatMap(backsolve(_, expected)).distinct

val foo = output:
  CPU(a=anss.head, b=0, c=0, ip=0, startState.program)

val ans2 = anss.min


(0 until 8).map: b =>
  output(CPU(a=b+(7<<3)+(4<<6), b=0, c=0, ip=0, startState.program))



startState.program

// candidates.indexWhere(_.head == 0)
// 7

(0 until 8).map: a =>
  a + 0b100<<3
  output:
    CPU(a=a , b=0, c=0, ip=0, startState.program)

output:
  /// next b should be 0
  // output
  CPU(a=0b100 , b=0, c=0, ip=0, startState.program)

output:
  // next b should be 100
  CPU(a=0b100111 , b=0, c=0, ip=0, startState.program)

(0 until 8).map: a =>
  a + 0b100111<<3
  output:
    CPU(a=0b100111, b=0, c=0, ip=0, startState.program)



startState.program

// startState.program.reverse.scanLeft(0):
//   case (c, d) =>
//     (0 until 8).find: b =>
//       output(CPU(a=b+(c<<3), b=0, c=0, ip=0, startState.program)).head == d
//     .get





startState.a.toBinaryString

val states = LazyList.iterate(startState)(_.next._2)

states.takeWhile(_.running).map(_.opcode).toList

states.map(_.a).take(72).toList

// 0,7,2,6,4,2,7,2,5 wrong

startState.copy(
  c = 9,
  program = Vector(2, 6)
).next._2.b

output:
  startState.copy( a = 10, program = Vector(5,0,5,1,5,4) )

output:
  startState.copy(a = 2024, program = Vector(0,1,5,4,3,0))

finalState(startState.copy(a = 2024, program = Vector(0,1,5,4,3,0)))
  .a

finalState(startState.copy(b = 29, program = Vector(1,7)))
  .b

finalState(startState.copy(
  b = 2024,
  c = 43690,
  program = Vector(4,0)
)).b

startState.program
  .sliding(2)
  .foreach:
    case Vector(op, arg) =>
      println(s"${Opcode.fromOrdinal(op)} $arg")
