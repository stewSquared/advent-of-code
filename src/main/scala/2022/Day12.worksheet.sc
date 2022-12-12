val grid = io.Source.fromResource("2022/day-12.txt").getLines().toVector

val xRange = grid(0).indices
val yRange = grid.indices

case class Point(x: Int, y: Int):
  def u = copy(y = y + 1)
  def d = copy(y = y - 1)
  def l = copy(x = x + 1)
  def r = copy(x = x - 1)

  def inBounds = xRange.contains(x) && yRange.contains(y)
  def adj = Set(u, d, l, r).filter(_.inBounds)

val points = for
  y <- yRange
  x <- xRange
yield Point(x, y)

def elevation(p: Point) = grid(p.y)(p.x) match
  case 'S' => 'a'
  case 'E' => 'z'
  case c   => c

def floodFill(source: Set[Point]): LazyList[Set[Point]] =
  LazyList.unfold(Set.empty[Point] -> source) { case (visitedLast, visiting) =>
    Option.when(visiting.nonEmpty) {
      val next = visiting
        .flatMap(p => p.adj.filter(elevation(_) <= elevation(p) + 1))
        .filterNot(visitedLast)
      visiting -> (visiting, next)
    }
  }

val start = points.find(p => grid(p.y)(p.x) == 'S').get
val end = points.find(p => grid(p.y)(p.x) == 'E').get
val lowPoints = points.filter(elevation(_) == 'a')

val ans1 = floodFill(Set(start)).indexWhere(_.contains(end))
val ans2 = floodFill(Set(lowPoints*)).indexWhere(_.contains(end))
