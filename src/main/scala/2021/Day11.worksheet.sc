import io.Source
import util.chaining.*
import util.Using

import collection.mutable.ArraySeq

case class Point(x: Int, y: Int):
  def up = copy(y = y - 1)
  def down = copy(y = y + 1)
  def left = copy(x = x - 1)
  def right = copy(x = x + 1)

def input(): Grid = Using(Source.fromResource("2021/day-11-1.txt")) { source =>
  Grid(ArraySeq.from(source.getLines.map(line => ArraySeq.from(line.map(_.asDigit)))))
}.get

class Grid (rows: ArraySeq[ArraySeq[Int]]):
  def apply(p: Point): Int = rows(p.y)(p.x)
  def update(p: Point, n: Int): Unit = rows(p.y)(p.x) = n

  def xRange = 0 until rows(0).length
  def yRange = 0 until rows.length

  def inBounds(p: Point): Boolean = p match
    case Point(x, y) => (xRange contains x) && (yRange contains y)

  def adjacent(p: Point): Seq[Point] =
    List(p.up.left, p.up, p.up.right, p.left, p.right, p.down.left, p.down, p.down.right)
      .filter(inBounds)

  def allPoints: Seq[Point] = for
    x <- xRange
    y <- yRange
  yield Point(x, y)

  def increment(points: Seq[Point]): Unit =
    for p <- points do this(p) += 1
    val flashing = points.distinct.filter(this(_) > 9)
    for p <- flashing do this(p) = 0
    val flashed = flashing.flatMap(p => adjacent(p).filter(this(_) != 0))
    if flashed.nonEmpty then increment(flashed)

  def step() = increment(allPoints)

def flashCounts(grid: Grid = input()) = Iterator.continually {
  grid.step()
  grid.allPoints.count(grid(_) == 0)
}

val ans1 = flashCounts().take(100).sum

val count = input().allPoints.size
val ans2 = flashCounts().indexWhere(_ == count) + 1
