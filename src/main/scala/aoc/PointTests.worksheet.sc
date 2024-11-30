import java.util.NoSuchElementException
import aoc.*

Point(3,4).xy
aoc.xy(Point(3,4))

val area = aoc.Area(0 to 2, 0 to 2)

import collection.immutable.NumericRange

NumericRange(0L, 4L, 1L)

def foo[N : Integral](n: N) =
  NumericRange(n, n, n)

area.expand == aoc.Area(-1 to 3, -1 to 3)
area.size
area.expand(1).size
area.topLeft
area.botRight
area.topBorder

area.topLeft.d.d.r.r
area.topLeft.d.r.u.l
area.topLeft.r.r.d.d
area.botRight.u.u.l.l

Area.bounding(Point(0,3), Point(4, 3)).size
Area.bounding(Point(0,3), Point(4, 3)).xRange.size
Area.bounding(Point(0,3), Point(4, 3)).yRange.size

assert(Point(4, 1).cw == Point(-1, 4))

val p = Point(3,4)

p.xy

assert(p.cw.cw.cw.cw == p)
assert(p.ccw.ccw.ccw.ccw == p)
assert(p.cw.ccw == p)

assert(p.u == Point(3,3))

assert(p.move(Dir.N) == p.n)
assert(p.move(Dir.S) == p.s)
assert(p.move(Dir.E) == p.e)
assert(p.move(Dir.W) == p.w)

assert(p.u == p.n)
assert(p.d == p.s)
assert(p.l == p.w)
assert(p.r == p.e)

assert(p.n.adjacentTo(p))
assert(p.nw.surrounds(p))

assert(area.topLeft.dist(area.botRight) == 4)

assert(p.plus(p) == Point(6,8))
assert(p.minus(p) == Point.origin)

assert(area.contains(Point.origin))
assert(!area.contains(area.topLeft.n))

area.topBorder
area.topBorder.xRange
area.topBorder.yRange

3 to 3
0.sign

List.empty.indices
(1 until 1) == (0 until 0)
(1 to 1) == (0 to 0)

"aoeu".indices

Area(Vector(Vector(3,4,5)))

val grid = Vector(
  Vector(1,2,3),
  Vector(4,5,6),
  Vector(7,8,9)
)

assert(Area(grid) == Area(0 to 2, 0 to 2))
assert(grid(Point(1,1)) == 5)

val stringGrid = Vector(
  "abc",
  "def",
  "ghi"
)

assert(Area(stringGrid) == Area(0 to 2, 0 to 2))
assert(stringGrid(Point(1,1)) == 'e')

val emptyArea = Area(0 until 0, 0 until 0)

assert(emptyArea.size == 0)
assert(!emptyArea.contains(Point.origin))

assertThrows[NoSuchElementException](emptyArea.topLeft)
assertThrows[NoSuchElementException](emptyArea.left)
assertThrows[NoSuchElementException](emptyArea.expand(1))

import annotation.nowarn

extension (any: Any)
  @nowarn("msg=discarded") def discard(): Unit = ()

def assertThrows[T](body: => Any)(using m: scala.reflect.ClassTag[T]) =
  try
    body.discard()
    assert(false)
  catch
    case e: Throwable =>
      assert(m.runtimeClass.isInstance(e))

// Area intersection

val area1 = Area(0 to 2, 0 to 2)
val area2 = Area(1 to 3, 2 to 4)
val area3 = Area(3 to 4, 3 to 4)
area1.intersect(area2).get
area1.intersect(area2).get.size
area1.intersect(area3).isEmpty

// Area diff

val largeArea = Area(0 to 10, 0 to 10)
val midArea = Area(3 to 7, 3 to 7)
val topLeftArea = Area(0 to 7, 0 to 7)
val botRightArea = Area(3 to 10, 3 to 10)

midArea.diff(largeArea).isEmpty
largeArea.size - midArea.size
largeArea.diff(midArea).map(_.size).sum
largeArea.diff(midArea) foreach println
largeArea.diff(botRightArea).size == 3
largeArea.size - botRightArea.size
largeArea.diff(botRightArea).map(_.size).sum

botRightArea.diff(topLeftArea).size == 3
botRightArea.diff(topLeftArea).map(_.size).sum
botRightArea.diff(topLeftArea.intersect(botRightArea).get).map(_.size).sum
botRightArea.size - botRightArea.intersect(topLeftArea).get.size
botRightArea.intersect(topLeftArea).get.size
botRightArea.size
64 - 25
botRightArea.diff(topLeftArea) foreach println

// Area union

topLeftArea.size + botRightArea.size - topLeftArea.intersect(botRightArea).get.size
topLeftArea.union(botRightArea).map(_.size).sum
topLeftArea.union(botRightArea) foreach println
val expectedAreas = List(
  Area(0 to 2, 0 to 2),
  Area(0 to 2, 3 to 7),
  // Area(0 to 2, 8 to 10),
  Area(3 to 7, 0 to 2),
  Area(3 to 7, 3 to 7),
  Area(3 to 7, 8 to 10),
  // Area(8 to 10, 0 to 2),
  Area(8 to 10, 3 to 7),
  Area(8 to 10, 8 to 10)
)
expectedAreas.toSet == topLeftArea.union(botRightArea).toSet
// TODO fix this!^

topLeftArea.diff(botRightArea) foreach println
botRightArea.diff(topLeftArea) foreach println
// Interval tests

val range = Interval(1, 10)
range.intersect(Interval(8, 12))
range.intersect(Interval(-2, 2))
range.intersect(Interval(2, 8))
range.intersect(Interval(-2, 12))

Interval(1L, 10L).diff(Interval(8L, 12L))
Interval(1L, 10L).diff(Interval(3L, 5L))
Interval(1L, 10L).diff(Interval(-5L, 5L))
Interval(1L, 10L).diff(Interval(-5L, 15L))
Interval(1L, 10L).diff(Interval(15L, 25L))
