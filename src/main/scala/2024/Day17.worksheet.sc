val input = io.Source.fromResource("2024/day-17.txt").getLines.toList

enum Opcode:
  case adv, bxl, bst, jnz, bxc, out, bdv, cdv

case class CPU(a: Long, b: Long, c: Long, ip: Int, program: Vector[Int]):
  def running: Boolean = program.indices contains ip

  def combo: Int = program(ip + 1) match
    case n@(0|1|2|3) => n
    case 4 => (a % 8).toInt
    case 5 => (b % 8).toInt
    case 6 => (c % 8).toInt
    case 7 => ???

  def literal = program(ip + 1)
  def opcode = Opcode.fromOrdinal(program(ip))

  def advance = this.copy(ip = ip + 2)

  def noOutput = None -> this
  def withOutput(n: Int) = Some(n) -> this

  private def div = a / math.pow(2, combo).toInt

  def next: (Option[Int], CPU) =
    opcode match
      case Opcode.adv => copy(a = div).advance.noOutput
      case Opcode.bxl => copy(b = b^literal).advance.noOutput
      case Opcode.bst => copy(b = combo % 8).advance.noOutput
      case Opcode.jnz =>
        if a == 0 then this.advance.noOutput
        else copy(ip = literal).noOutput
      case Opcode.bxc => copy(b = b^c).advance.noOutput
      case Opcode.out => this.advance.withOutput(combo % 8)
      case Opcode.bdv => copy(b = div).advance.noOutput
      case Opcode.cdv => copy(c = div).advance.noOutput

  def run: List[Int] =
    Iterator.unfold[Option[Int], CPU](this): cpu =>
      Option.when(cpu.running)(cpu.next)
    .flatten.toList

  def load(program: Int*): CPU =
    this.copy(program = program.toVector)

  def runWith(program: Vector[Int]): List[Int] =
    load(program*).run

  def endState: CPU =
    Iterator.iterate(this)(_.next._2)
      .dropWhile(_.running).next

  def debug: LazyList[(output: Option[Int], state: CPU)] =
    LazyList.unfold(this): cpu =>
      Option.when(cpu.running):
        val n = cpu.next
        (n, n._2)

object CPU:
  def init(a: Long = 0, b: Long = 0, c: Long = 0, ip: Int = 0, program: Vector[Int] = Vector.empty) =
    CPU(a=a, b=b, c=c, ip=ip, program=program)

@annotation.nowarn("msg=StringContext")
val startState: CPU =
  val s"Register A: $a" = input(0)
  val s"Register B: $b" = input(1)
  val s"Register C: $c" = input(2)

  val s"Program: $program" = input(4)
  val programValue = program.split(",").mkString.map(_.asDigit).toVector

  CPU(a.toInt, b.toInt, c.toInt, 0, programValue)

val ans1 = startState.run.mkString(",")

def backsolve(prefix: Long, expected: Vector[Int]): List[Long] =
  (0 until 8).map(i => i+(prefix<<3)).filter: a =>
    startState.copy(a = a).run == expected
  .toList

val anss = startState.program.tails.toList.reverse.tail
  .foldLeft(List(0L)):
    (possiblePrefixes, expected) =>
      possiblePrefixes.flatMap(backsolve(_, expected)).distinct

val ans2 = anss.min

CPU.init(c=9).load(2, 6).endState.b == 1
CPU.init(a=10).load(5,0,5,1,5,4).run == List(0,1,2)
CPU.init(a=2024).load(0,1,5,4,3,0).run == List(4,2,5,6,7,7,7,7,3,1,0)
CPU.init(a=2024).load(0,1,5,4,3,0).endState.a == 0
CPU.init(b=29).load(1,7).endState.b == 26
CPU.init(b=2024, c=43690).load(4,0).endState.b == 44354
