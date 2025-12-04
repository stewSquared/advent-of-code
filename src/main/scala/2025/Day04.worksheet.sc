val input = io.Source.fromResource("2025/day-04.txt").getLines().toVector

import aoc.*

val rolls = Area(input).pointsIterator.filter(input(_) == '@').toSet

def accessible(rolls: Set[Point]): Set[Point] =
  rolls.filter(_.surrounding.count(rolls) < 4)

val ans1 = accessible(rolls).size

def remove(rolls: Set[Point]): Set[Point] =
  rolls.diff(accessible(rolls))

val steps = LazyList.iterate(rolls)(remove)
lazy val pairs = steps.zip(steps.tail)

val remaining = pairs.collectFirst:
  case (s, t) if s.size == t.size => s.size
.get

val ans2 = rolls.size - remaining
