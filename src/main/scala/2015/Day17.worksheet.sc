val input = io.Source.fromResource("2015/day-17.txt").getLines().toList

val containers = input.map(_.toInt).sorted

def count2(goal: Int, containers: List[Int], used: List[Int]): List[List[Int]] =
  if goal < 0 then Nil
  else if goal == 0 then List(used)
  else containers match
    case c::cs => count2(goal - c, cs, c::used) ::: count2(goal, cs, used)
    case Nil => Nil

val solutions = count2(150, containers, Nil)

val ans1 = solutions.size

val min = solutions.map(_.size).min
val ans2 = solutions.count(_.size == min)
