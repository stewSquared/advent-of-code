import io.Source
import util.chaining.*
import util.Using

import collection.mutable.ArraySeq

case class Point(x: Int, y: Int)

def input(): Grid = Using(Source.fromResource("2021/day-11-1.txt")) { source =>
  Grid(source.getLines.map(_.map(_.asDigit).to(ArraySeq)).to(ArraySeq))
}.get

class Grid(rows: ArraySeq[ArraySeq[Int]]):
  def apply(p: Point): Int = rows(p.y)(p.x)
  def update(p: Point, n: Int): Unit = rows(p.y)(p.x) = n

  def xRange = 0 until rows(0).length
  def yRange = 0 until rows.length

  def inBounds(p: Point): Boolean = xRange.contains(p.x) && yRange.contains(p.y)

  def adjacent(p: Point): Iterable[Point] = for
    dx <- -1 to 1
    dy <- -1 to 1
    q = p.copy(x = p.x + dx, y = p.y + dy)
    if inBounds(q)
  yield q

  def allPoints: Seq[Point] = for
    x <- xRange
    y <- yRange
  yield Point(x, y)

  def increment(p: Point): Unit = if this(p) < 10 then
    this(p) += 1
    if this(p) == 10 then adjacent(p).foreach(increment)

  def step() =
    allPoints.foreach(increment)
    allPoints.filter(this(_) == 10).foreach(this(_) = 0)

def flashCounts(grid: Grid = input()) = Iterator.continually {
  grid.step()
  grid.allPoints.count(grid(_) == 0)
}

val ans1 = flashCounts().take(100).sum

val count = input().allPoints.size
val ans2 = flashCounts().indexWhere(_ == count) + 1
