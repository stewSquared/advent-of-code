import io.Source
import util.Using

case class Point(x: Int, y: Int):
  def u = copy(y = y - 1)
  def d = copy(y = y + 1)
  def l = copy(x = x - 1)
  def r = copy(x = x + 1)

extension (grid: Vector[Vector[Int]]) def apply(p: Point) = grid(p.y)(p.x)

val height = Using(Source.fromResource("2021/day-09-1.txt")) {
  _.getLines.map(_.map(_.asDigit).toVector).toVector
}.get

val xRange = height(0).indices
val yRange = height.indices

def adjacent(p: Point) = Set(p.u, p.d, p.l, p.r).filter { case Point(x, y) =>
  xRange.contains(x) && yRange.contains(y)
}

val lowPoints = for
  y <- yRange
  x <- xRange
  p = Point(x, y)
  if adjacent(p).forall(height(_) > height(p))
yield p

def basin(low: Point): Set[Point] =
  lazy val points: LazyList[Set[Point]] =
    Set.empty #:: Set(low) #::
      points.zip(points.tail).map { (previous, toVisit) =>
        toVisit.flatMap(adjacent).filter(height(_) < 9).diff(previous)
      }
  points.tail.takeWhile(_.nonEmpty).reduce(_ union _)

val ans1 = lowPoints.map(height(_) + 1).sum

val basinSizes = lowPoints.map(basin(_).size)
val ans2 = basinSizes.sorted.reverse.take(3).product
