val input = io.Source.fromResource("2024/day-07.txt").getLines.toList

val equations: List[(Long, List[Long])] =
  input.collect:
    case s"$lhs: $rhs" =>
      lhs.toLong -> rhs.split(" ").map(_.toLong).toList.reverse

def solvable(lhs: Long, rhs: List[Long]): Boolean =
  rhs match
    case Nil => ???
    case head :: Nil => head == lhs
    case n :: ns =>
      solvable(lhs - n, ns)
        || ((lhs % n == 0) && solvable(lhs / n, ns))
        || (lhs > n && (lhs.toString.endsWith(n.toString)) && solvable(lhs.toString.dropRight(n.toString.length).toLong, ns))

val ans = equations
  .filter(solvable)
  .map(_._1)
  .sum
