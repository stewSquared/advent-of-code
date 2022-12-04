val input = io.Source.fromResource("2022/day-04.txt").getLines().collect {
  case s"$a-$b,$c-$d" =>
    (a.toInt to b.toInt, c.toInt to d.toInt)
}

val ans1 = input.count { case (left, right) =>
  left.containsSlice(right) || right.containsSlice(left)
}

val ans2 = input.count(_.intersect(_).nonEmpty)
