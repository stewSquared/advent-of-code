val input = io.Source.fromResource("2025/day-05.txt").getLines().toList

import aoc.Interval

val fresh = input.collect:
  case s"$a-$b" => Interval(a.toLong, b.toLong)
.foldLeft(List.empty[Interval[Long]]):
  case (acc, n) => n.union(acc)

val available = input.flatMap(_.toLongOption)

val ans1 = available.count(id => fresh.exists(_.contains(id)))
val ans2 = fresh.map(_.size).sum
