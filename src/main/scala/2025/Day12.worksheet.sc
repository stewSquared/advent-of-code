val input = io.Source.fromResource("2025/day-12.txt").getLines().toVector

import aoc.{Point, Area}

case class Shape(points: Set[Point]):
  def n = points.map(_.n)
  def s = points.map(_.s)
  def e = points.map(_.e)
  def w = points.map(_.w)
  def cw = points.map: p =>
    Point(x = p.y, y = -p.x)
  def ccw = points.map: p =>
    Point(x = -p.y, y = p.x)

val baseShapes = Iterator.unfold(input):
  lines => lines.head match
    case s"$n:" => Some:
      val grid = lines.tail.take(3) //.takeWhile(_.nonEmpty)
      val points = Area(grid).pointsIterator.filter(grid(_) == '#').toSet
      println(Area(grid).draw(p => (if points(p) then '#' else '.')))
      val shape = Shape(points)

      shape -> lines.drop(5)
    case _ => None
.toVector

val regions: Vector[(Area, List[Int])] = input.collect:
  case s"${w}x${l}: $counts" =>
    Area(0 until w.toInt, 0 until l.toInt) -> counts.split(' ').map(_.toInt).toList

regions.size

val lowerBound = regions.count:
  case (area, counts) =>
    val presentVolume = counts.zip(baseShapes).map:
      case (c, s) => s.points.size.toLong * c
    .sum

    presentVolume <= area.size[Long]


//
