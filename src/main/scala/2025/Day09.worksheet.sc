val input = io.Source.fromResource("2025/day-09.txt").getLines().toList

import aoc.{Area, Point, Line}

val tiles = input.collect:
  case s"$x,$y" => Point(x.toInt, y.toInt)

val rects = tiles.combinations(2).map:
  case Seq(p, q) => Area.bounding(p, q)
.toList

val ans1 = rects.map(_.size[Long]).max

val lines = tiles.zip(tiles.last :: tiles).map:
  case (p, q) => Line(p, q)

def uncrossed(a: Area): Boolean =
  val inner = a.expand(-1)
  lines.forall: l =>
    util.Try:
      Area.bounding(l.p, l.q).intersect(inner).isEmpty
    .getOrElse(false)

val ans2 = rects.filter(uncrossed).map(_.size[Long]).max
