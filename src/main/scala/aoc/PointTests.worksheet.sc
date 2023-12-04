import java.util.NoSuchElementException
import aoc.*

Point(3,4).xy
aoc.xy(Point(3,4))

val area = aoc.Area(0 to 2, 0 to 2)

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


def assertThrows[T](body: => Unit)(using m: scala.reflect.ClassTag[T]) =
  try
    body
    assert(false)
  catch
    case e: Throwable =>
      assert(m.runtimeClass.isInstance(e))




// Area.foo
