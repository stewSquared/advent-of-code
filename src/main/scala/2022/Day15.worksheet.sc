val input = io.Source.fromResource("2022/day-15.txt").getLines().toList

case class Point(x: Int, y: Int):
  def dist(other: Point) =
    (other.x - x).abs + (other.y - y).abs

val sbs = input.collect {
  case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
    Point(sx.toInt, sy.toInt) -> Point(bx.toInt, by.toInt)
}

Point(0,0).dist(Point(-2,1))

case class Range(min: Int, max: Int):
  override def toString = s"$min..$max"

  def size = max + 1 - min
  def contains(n: Int) = min <= n && n <= max
  def supersetOf(r: Range) = min <= r.min && r.max <= max

  def intersect(r: Range): Option[Range] =
    if supersetOf(r) then Some(r)
    else if r contains min then Some(copy(max = max.min(r.max)))
    else if r contains max then Some(copy(min = min.max(r.min)))
    else None

  def diff(r: Range): List[Range] =
    intersect(r).fold(List(this)) { o =>
      List(
        Option.when(min < o.min)(min to (o.min - 1)),
        Option.when(o.max < max)((o.max + 1) to max)
      ).flatten
    }

  def union(r: Range): List[Range] = r :: this.diff(r)

  // def union(ranges: Range): List[Range] = ???


def unionSize(areas: Seq[Range]): Long =
  val union = areas.sortBy(_.size).foldLeft[List[Range]](Nil) {
    (disjoint, a) => a :: disjoint.flatMap(_ diff a)
  }
  union.map(_.size).sum

unionSize(List(1 to 7, 3 to 10))
unionSize(List(1 to 3, 7 to 10))

object Range:
  def apply(n1: Int, n2: Int): Range =
    new Range(n1 min n2, n1 max n2)

  def unapply(s: String) = s match
    case s"$n1..$n2" => n1.toIntOption.zip(n2.toIntOption).map(apply)

extension (n1: Int) def to(n2: Int): Range = Range(n1, n2)

Radius(Point(0,0), 3).atRow(y = 0)
Radius(Point(0,0), 3).atRow(y = 1)
Radius(Point(0,0), 3).atRow(y = 2)
Radius(Point(0,0), 3).atRow(y = 3)
Radius(Point(0,0), 3).atRow(y = 4)

Radius(Point(1,1), 3).atRow(y = 1)
Radius(Point(1,1), 3).atRow(y = 2)
Radius(Point(1,1), 3).atRow(y = 3)
Radius(Point(1,1), 3).atRow(y = 4)
Radius(Point(1,1), 3).atRow(y = 5)

case class Radius(s: Point, d: Int):
  def contains(p: Point): Boolean = s.dist(p) <= d
  def atRow(y: Int): Option[Range] =
    val h = d - (s.y - y).abs
    val min = s.x - h
    val max = s.x + h
    Option.when(min <= max)(min to max)

val radii = sbs.map((s,b) => Radius(s, s.dist(b)))

val rowRanges = radii.flatMap(_.atRow(y=2000000))
val ans1 = unionSize(rowRanges)

val MAX = 4000000

radii
  .flatMap(_.atRow(0))

radii
  .flatMap(_.atRow(0))
  .flatMap((0 to MAX).diff)

unionSize(radii
  .flatMap(_.atRow(0))
  .flatMap((0 to MAX).diff))

unionSize(radii
  .flatMap(_.atRow(1))
  .flatMap((0 to MAX).diff))

val possibleBeacons: Iterator[Point] =
  for
    y <- scala.Range(0, MAX + 1).iterator
    xNonRanges = radii.flatMap(_.atRow(y))
    xRanges = xNonRanges.foldLeft(List(Range(0, MAX + 1))) {
      (possibleRanges, nonRange) => possibleRanges.flatMap(_.diff(nonRange))
    }
    if xRanges.size == 1
    xRange <- xRanges
    if xRange.size == 1
  yield Point(xRange.min, y)

val distress: Point = possibleBeacons.next()

val ans2 = distress.x.toLong * 4000000L + distress.y.toLong
