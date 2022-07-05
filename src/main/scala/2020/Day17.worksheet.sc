val input = io.Source.fromResource("2020/day-17.txt").getLines.toList

case class Point(x: Int, y: Int, z: Int, w: Int):
  def neighbors: Seq[Point] = for
    xd <- x - 1 to x + 1
    yd <- y - 1 to y + 1
    zd <- z - 1 to z + 1
    wd <- w - 1 to w + 1
    if x != xd || y != yd || z != zd || w != wd
  yield Point(xd, yd, zd, wd)

extension (r: Range) def expand: Range = r.min - 1 to r.max + 1

case class Area(xRange: Range, yRange: Range, zRange: Range, wRange: Range):
  def expand: Area =
    copy(xRange.expand, yRange.expand, zRange.expand, wRange.expand)

  def points: IndexedSeq[Point] = for
    xd <- xRange
    yd <- yRange
    zd <- zRange
    wd <- wRange
  yield Point(xd, yd, zd, wd)

case class Grid(area: Area, active: Set[Point]):
  def activeNext(p: Point): Boolean =
    val activeNeighbors = p.neighbors.count(active)
    active(p) && activeNeighbors == 2 ||
    activeNeighbors == 3

  def next: Grid =
    val expanded = area.expand
    Grid(expanded, expanded.points.filter(activeNext).toSet)

  def next3d: Grid =
    val expanded = area.expand.copy(wRange = 0 to 0)
    Grid(expanded, expanded.points.filter(activeNext).toSet)

val start: Grid =
  val xRange = 0 until input(0).length
  val yRange = 0 until input.length
  val initialPoints = for
    x <- xRange
    y <- yRange
    if input(y)(x) == '#'
  yield Point(x, y, 0, 0)
  Grid(Area(xRange, yRange, 0 to 0, 0 to 0), initialPoints.toSet)

val ans1 = LazyList.iterate(start)(_.next3d)(6).active.size

val ans2 = LazyList.iterate(start)(_.next)(6).active.size
