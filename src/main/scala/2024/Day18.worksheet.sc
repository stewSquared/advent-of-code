val input = io.Source.fromResource("2024/day-18.txt").getLines.toList

import aoc.{Area, Point}

val area = Area(0 to 70, 0 to 70)

val bytes: List[Point] = input.collect:
  case s"$x,$y" => Point(x.toInt, y.toInt)

val grid1024: Set[Point] = bytes.take(1024).toSet

val start = area.topLeft
val end = area.botRight

def floodFill(blocked: Set[Point]): Iterator[Set[Point]] =
  Iterator.unfold(Set.empty[Point] -> Set(start)):
    case (prev, curr) => Option.when(curr.nonEmpty):
      val next = curr.flatMap(_.adjacent)
        .diff(prev)
        .diff(blocked)
        .filter(_.inBounds(area))

      curr -> (curr -> next)

val ans1 = floodFill(grid1024).indexWhere(_.contains(end))

def reachable(n: Int): Boolean =
  val blocked = bytes.take(n + 1).toSet
  floodFill(blocked).exists(_.contains(end))

def bs(range: Range): Point =
  if range.size == 1 then bytes(range.start) else
    val (left, right) = range.splitAt(range.size / 2)
    if reachable(left.last) then bs(right) else bs(left)

val ans2 = bs(bytes.indices.drop(1024))
