import io.Source
import util.Using
import util.chaining.*

type Grid = Vector[Vector[Int]]

case class Point(x: Int, y: Int):
  def up = copy(y = y - 1)
  def down = copy(y = y + 1)
  def left = copy(x = x - 1)
  def right = copy(x = x + 1)

val heightMap: Grid = Using(Source.fromResource("2021/day-09-1.txt")) {
  _.getLines.map(_.map(_.asDigit).toVector).toVector
}.get

extension (rows: Grid)
  def heightAt(p: Point): Int = rows(p.y)(p.x)

  def inBounds(p: Point) = p match
    case Point(x, y) =>
      0 <= y && y < rows.length
        && 0 <= x && x < rows(0).length

  def adjacent(p: Point): Set[Point] =
    Set(p.up, p.down, p.left, p.right).filter(inBounds)

  def lowPoints: Seq[Point] = for
    y <- 0 until rows.length
    x <- 0 until rows(0).length
    p = Point(x, y)
    if adjacent(p).forall(q => heightAt(q) > heightAt(p))
  yield p

  def basin(low: Point): Set[Point] = {
    lazy val points: LazyList[Set[Point]] = Set.empty #:: Set(low)
      #:: points.zip(points.tail).map((previous, toVisit) =>
          toVisit.flatMap(adjacent).filterNot(previous).filter(heightAt(_) < 9)
        )
    points.tail.takeWhile(_.nonEmpty).reduce(_ union _)
  }

val ans1 = heightMap.lowPoints.map(p => heightMap.heightAt(p) + 1).sum

val ans2 = heightMap.lowPoints
  .map(heightMap.basin(_).size)
  .sorted.reverse.take(3).product
