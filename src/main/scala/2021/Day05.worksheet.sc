import io.Source
import util.Using

val lines = Using(Source.fromResource("2021/day-05-1.txt")) {
  _.getLines.map { case s"$x1,$y1 -> $x2,$y2" =>
    Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
  }.toSeq
}.get

case class Point(x: Int, y: Int)

case class Line(p: Point, q: Point):
  val xDist = q.x - p.x
  val yDist = q.y - p.y

  def horizontal = yDist == 0
  def vertical = xDist == 0

  def xRange = (p.x to q.x by xDist.sign)
  def yRange = (p.y to q.y by yDist.sign)

  def points: Seq[Point] =
    if horizontal then xRange.map(Point(_, p.y))
    else if vertical then yRange.map(Point(p.x, _))
    else (xRange zip yRange).map(Point(_, _))

val ans1 = lines
  .filter(l => l.horizontal || l.vertical)
  .flatMap(_.points)
  .groupBy(identity)
  .count(_._2.size > 1)

val ans2 = lines
  .flatMap(_.points)
  .groupBy(identity)
  .count(_._2.size > 1)
