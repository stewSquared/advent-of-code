val trees: Vector[Vector[Int]] = io.Source.fromResource("2022/day-08.txt")
  .getLines()
  .toVector
  .map(_.map(_.asDigit).toVector)

case class Point(x: Int, y: Int):
  def up = copy(y = y - 1)
  def down = copy(y = y + 1)
  def left = copy(x = x - 1)
  def right = copy(x = x + 1)

val xRange = trees.head.indices
val yRange = trees.indices

def inBounds(p: Point) =
  xRange.contains(p.x) && yRange.contains(p.y)

def height(p: Point) = trees(p.y)(p.x)

def lineOfSight(p: Point, d: Point => Point): Iterator[Point] =
  Iterator.iterate(p)(d).takeWhile(inBounds).drop(1)

def visible(p: Point): Boolean =
  val h = height(p)
  lineOfSight(p, _.up).forall(height(_) < h) ||
  lineOfSight(p, _.down).forall(height(_) < h) ||
  lineOfSight(p, _.right).forall(height(_) < h) ||
  lineOfSight(p, _.left).forall(height(_) < h)

def scenicScore(p: Point): Int =
  val h = height(p)
  def treesVisible(it: Iterator[Point]): Int =
    val (low, blocking) = it.span(height(_) < h)
    low.size + (if blocking.hasNext then 1 else 0)

  val north = treesVisible(lineOfSight(p, _.up))
  val south = treesVisible(lineOfSight(p, _.down))
  val east = treesVisible(lineOfSight(p, _.right))
  val west = treesVisible(lineOfSight(p, _.left))
  north * south * east * west

val points = for
  x <- xRange
  y <- yRange
yield Point(x, y)

val ans1 = points.count(visible)
val ans2 = points.map(scenicScore).max
