val input = io.Source.fromResource("2022/day-15.txt").getLines().toList

case class Point(x: Int, y: Int):
  def dist(other: Point) =
    (other.x - x).abs + (other.y - y).abs

val radii = input.collect {
  case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
    val sensor = Point(sx.toInt, sy.toInt)
    val beacon = Point(bx.toInt, by.toInt)
    Radius(sensor, sensor.dist(beacon))
}

case class Interval(min: Int, max: Int):
  override def toString = s"$min..$max"

  def size = max + 1 - min
  def contains(n: Int) = min <= n && n <= max
  def iterator = scala.Range.inclusive(min, max).iterator

  def diff(n: Interval): List[Interval] =
    if min < n.min && n.max < max then
      List(copy(max = n.min - 1), copy(min = n.max + 1))
    else if n.min <= min && max <= n.max then Nil
    else if n contains max then List(copy(max = n.min - 1))
    else if n contains min then List(copy(min = n.max + 1))
    else List(this)

  def diff(intervals: Seq[Interval]): List[Interval] =
    intervals.foldLeft(List(this)) { (disjoint, n) =>
      disjoint.flatMap(_.diff(n))
    }

object Interval:
  def apply(n1: Int, n2: Int): Interval = new Interval(n1 min n2, n1 max n2)

case class Radius(s: Point, d: Int):
  def atRow(y: Int): Option[Interval] =
    val h = d - (s.y - y).abs
    Option.when(h >= 0)(Interval(s.x - h, s.x + h))

def disjointUnion(ranges: Seq[Interval]): List[Interval] =
  ranges.foldLeft[List[Interval]](Nil) { (disjoint, r) =>
    r :: disjoint.flatMap(_.diff(r))
  }

val xNonRanges = radii.flatMap(_.atRow(y = 2000000))
val ans1 = disjointUnion(xNonRanges).map(_.size).sum

val MAX = 4000000
val possibleRange = Interval(0, MAX)

val possibleBeacons: Iterator[Point] =
  for
    y <- possibleRange.iterator
    xNonRanges = radii.flatMap(_.atRow(y))
    xRange <- possibleRange.diff(xNonRanges)
    x <- xRange.iterator
  yield Point(x, y)

val distress = possibleBeacons.next()
val ans2 = distress.x.toLong * MAX + distress.y
