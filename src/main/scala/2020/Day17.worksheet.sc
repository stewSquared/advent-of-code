val input = io.Source.fromResource("2020/day-17.txt").getLines.toList

// val input = List(
//   ".#.",
//   "..#",
//   "###"
// )

(1 to 10).toList

case class Point(x: Int, y: Int, z: Int, w: Int):
  def adjacent = for
    xd <- x-1 to x+1
    yd <- y-1 to y+1
    zd <- z-1 to z+1
    wd <- w-1 to w+1
    if !(xd == x && yd == y && zd == z && wd == w)
  yield Point(xd, yd, zd, wd)

Point(0,0,0,0).adjacent

extension (r: Range)
  def expand: Range = (r.min - 1) to (r.max + 1)

case class Area(xRange: Range, yRange: Range, zRange: Range, wRange: Range):
  def expand: Area = Area(xRange.expand, yRange.expand, zRange.expand, wRange.expand)

  def points = for
    x <- xRange
    y <- yRange
    z <- zRange
    w <- wRange
  yield Point(x, y, z, w)

case class Grid(area: Area, active: Set[Point]):
  def activeNext(p: Point): Boolean =
    val activeNeighbors = p.adjacent.count(active) // TODO optimize
    activeNeighbors == 3 || active(p) && activeNeighbors == 2

  def next: Grid =
    val expanded = area.expand
    val nextPoints: Set[Point] =
      expanded.points.filter(activeNext).toSet
    Grid(expanded, nextPoints)

input foreach println

val start =
  val xRange = 0 until input(0).length
  val yRange = 0 until input.length
  val area = Area(xRange, yRange, 0 to 0, 0 to 0)
  val active = area.points.filter {
    case Point(x, y, z, w) => input(y)(x) == '#'
  }.toSet
  Grid(area, active)

val ans = LazyList.iterate(start)(_.next)(6).active.size
