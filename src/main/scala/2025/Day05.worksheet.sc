val input = io.Source.fromResource("2025/day-05.txt").getLines().toList

import aoc.Interval

val fresh = input.collect:
  case s"$a-$b" => Interval(a.toLong, b.toLong)

val available = input.flatMap(_.toLongOption)

val ans1 = available.count(id => fresh.exists(r => r.contains(id)))

val union = fresh.foldLeft(List.empty[Interval[Long]]):
  case (acc, n) => n.union(acc)

val ans2 = union.map(_.size).sum
