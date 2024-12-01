val input = io.Source.fromResource("2024/day-01.txt").getLines().toList

val List(left, right) = input.map(_.split(" +").map(_.toInt).toList).transpose

val ans1 = left.sorted.zip(right.sorted).map(_ - _).map(_.abs).sum

val counts = right.groupMapReduce(identity)(_ => 1)(_ + _).withDefaultValue(0)

val ans2 = left.map(n => counts(n) * n).sum
