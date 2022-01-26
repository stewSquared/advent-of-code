val input = io.Source.fromResource("2020/day-08.txt").getLines.toIndexedSeq

case class State(pointer: Int, acc: Int, visited: List[Int], instructions: IndexedSeq[String]):
  def next: State = instructions(pointer) match
    case s"nop $arg" => copy(pointer + 1, acc, pointer :: visited)
    case s"acc $n" => copy(pointer + 1, acc + n.toInt, pointer :: visited)
    case s"jmp $o" => copy(pointer + o.toInt, acc, pointer :: visited)

def execute(instructions: IndexedSeq[String]): Iterator[State] =
  val start = State(pointer = 0, acc = 0, visited = Nil, instructions)
  Iterator.iterate(start)(_.next)

val ans = execute(input).takeWhile(s => ! s.visited.contains(s.pointer)).toSeq.last.acc

val ans2 =
  val instructionSets = input.zipWithIndex.collect {
    case (s"nop $arg", i) => input.updated(i, s"jmp $arg")
    case (s"jmp $arg", i) => input.updated(i, s"nop $arg")
  }

  val corrected = instructionSets.flatMap { instructions =>
    execute(instructions)
      .takeWhile(s => ! s.visited.contains(s.pointer))
      .find(s => s.pointer == instructions.size)
  }

  corrected.head.acc

println(ans2)

// instructions.count(_.startsWith("nop"))
// instructions.count(_.startsWith("jmp"))
