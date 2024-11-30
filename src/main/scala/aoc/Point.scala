package aoc

import collection.immutable.NumericRange
import math.Integral.Implicits.infixIntegralOps
import math.Ordering.Implicits.infixOrderingOps

case class Point(x: Int, y: Int):
  def n = copy(y = y - 1)
  def s = copy(y = y + 1)
  def e = copy(x = x + 1)
  def w = copy(x = x - 1)

  def ne = Point(x + 1, y - 1)
  def se = Point(x + 1, y + 1)
  def nw = Point(x - 1, y - 1)
  def sw = Point(x - 1, y + 1)

  def nRepeat(n: Int) = copy(y = y - n)
  def sRepeat(n: Int) = copy(y = y + n)
  def eRepeat(n: Int) = copy(x = x + n)
  def wRepeat(n: Int) = copy(x = x - n)

  def u = n
  def d = s
  def l = w
  def r = e

  def swap = Point(y, x)

  // TODO: consider Move objects that represent movement functions

  def move(dir: Dir, times: Int = 1) = dir match
    case Dir.N => nRepeat(times)
    case Dir.S => sRepeat(times)
    case Dir.E => eRepeat(times)
    case Dir.W => wRepeat(times)

  def adjacent = Set(n, s, e, w)
  def surrounding = Set(n, s, e, w, ne, se, nw, sw)

  def adjacentTo(p: Point) = dist(p) == 1
  def surrounds(p: Point) =
    p != this && locally:
      val dx = (p.x - x).abs
      val dy = (p.y - y).abs
      dx <= 1 && dy <= 1

  def inBounds(a: Area) = a.contains(this)

  def dist(p: Point) =
    (p.x - x).abs + (p.y - y).abs

  def plus(p: Point) = Point(x + p.x, y + p.y)
  def minus(p: Point) = Point(x - p.x, y - p.y)

  def cw: Point = Point(-y, x)
  def ccw: Point = Point(y, -x)

object Point:
  val origin = Point(0, 0)

  extension [T](grid: IndexedSeq[IndexedSeq[T]])
    def apply(p: Point): T = grid(p.y)(p.x)

  extension (grid: IndexedSeq[String])
    def apply(p: Point): Char = grid(p.y)(p.x)

enum Dir:
  case E, S, W, N

  def turnRight = this match
    case Dir.E => S
    case Dir.S => W
    case Dir.W => N
    case Dir.N => E

  def turnLeft = this match
    case Dir.E => N
    case Dir.S => E
    case Dir.W => S
    case Dir.N => W

  def reverse = this match
    case Dir.E => W
    case Dir.S => N
    case Dir.W => E
    case Dir.N => S

case class Line(p: Point, q: Point):
  def dx = q.x - p.x
  def dy = q.y - p.y

  def horizontal = dy == 0
  def vertical = dx == 0

  def xRange = p.x to q.x by (if vertical then 1 else dx.sign)
  def yRange = p.y to q.y by (if horizontal then 1 else dy.sign)

  def points: Seq[Point] =
    if horizontal then xRange.map(Point(_, p.y))
    else if vertical then yRange.map(Point(p.x, _))
    else (xRange zip yRange).map(Point(_, _))

case class Interval[N : Integral](min: N, max: N):
  override def toString = s"$min..$max"
  private val one = Integral[N].one

  def size: N = max + one - min
  infix def contains(n: N) = min <= n && n <= max
  def toRange: Range = Range.inclusive(min.toInt, max.toInt)
  def toNumericRange = NumericRange.inclusive[N](min, max, one)
  def iterator = toNumericRange.iterator
  def supersetOf(r: Interval[N]) = min <= r.min && r.max <= max

  def intersect(r: Interval[N]): Option[Interval[N]] =
    if supersetOf(r) then Some(r)
    else if r contains min then Some(copy(max = max.min(r.max)))
    else if r contains max then Some(copy(min = min.max(r.min)))
    else None

  def diff(n: Interval[N]): List[Interval[N]] =
    if min < n.min && n.max < max then
      List(copy(max = n.min - one), copy(min = n.max + one))
    else if n.min <= min && max <= n.max then Nil
    else if n contains max then List(copy(max = n.min - one))
    else if n contains min then List(copy(min = n.max + one))
    else List(this)

  def diff(intervals: Seq[Interval[N]]): List[Interval[N]] =
    intervals.foldLeft(List(this)) { (disjoint, n) =>
      disjoint.flatMap(_.diff(n))
    }

  def union(r: Interval[N]): List[Interval[N]] =
    if intersect(r).isEmpty then List(this, r)
    else List(Interval(min min r.min, max max r.max))


object Interval:
  def apply[N : Integral](n1: N, n2: N): Interval[N] = new Interval[N](n1 min n2, n1 max n2)
  def apply(range: Range): Interval[Int] = new Interval[Int](range.min, range.max)
  def apply[N : Integral](numericRange: NumericRange[N]): Interval[N] = new Interval[N](numericRange.min, numericRange.max)

  def unapply(s: String) = s match
    case s"$n1..$n2" => n1.toIntOption.zip(n2.toIntOption).map(apply(_, _))

case class Area(xRange: Range, yRange: Range):
  def left = xRange.min
  def right = xRange.max
  def top = yRange.min
  def bot = yRange.max

  def width = xRange.size
  def height = yRange.size
  def size = width * height

  def topLeft = Point(left, top)
  def topRight = Point(right, top)
  def botLeft = Point(left, bot)
  def botRight = Point(right, bot)

  def topBorder = Line(topLeft, topRight)
  def botBorder = Line(botLeft, botRight)
  def leftBorder = Line(topLeft, botLeft)
  def rightBorder = Line(topRight, botRight)

  def apply(p: Point): Boolean = contains(p)

  def contains(p: Point) =
    xRange.contains(p.x) && yRange.contains(p.y)

  def adjacent(p: Point) = p.adjacent.filter(contains)

  def expand(n: Int): Area =
    copy(left - n to right + n, top - n to bot + n)

  def transpose = Area(yRange, xRange)

  def pointsIterator = for
    y <- yRange.iterator
    x <- xRange
  yield Point(x, y)

  def draw(f: Point => Char): String =
    val sb = collection.mutable.StringBuilder()
    for y <- yRange do
      for x <- xRange do sb.addOne(f(Point(x, y)))
      sb.addOne('\n')
    sb.result()

  def intersect(a: Area): Option[Area] =
    for
      xInterval <- Interval(xRange).intersect(Interval(a.xRange))
      yInterval <- Interval(yRange).intersect(Interval(a.yRange))
    yield
      Area(xInterval.toRange, yInterval.toRange)

  def diff(a: Area): List[Area] =
    intersect(a).fold(List(this)): a =>
      for
        xInterval <- Interval(a.xRange) :: Interval(xRange).diff(Interval(a.xRange))
        yInterval <- Interval(a.yRange) :: Interval(yRange).diff(Interval(a.yRange))
        intervalArea = Area(xInterval.toRange, yInterval.toRange)
        if a != intervalArea
      yield intervalArea

  def union(a: Area): List[Area] =
    val thisDiffed = this.diff(a)
    val aDiffed = a.diff(this)
    this.intersect(a).toList ++ thisDiffed ++ aDiffed

object Area:
  def apply(grid: IndexedSeq[IndexedSeq[_]]): Area =
    Area(
      xRange = grid.headOption.fold(0 to 0)(_.indices),
      yRange = grid.indices
    )

  def apply(grid: IndexedSeq[String])(using wrap: String => collection.immutable.WrappedString): Area =
    apply(grid.map(wrap))

  def bounding(p: Point, q: Point) =
    val dx = q.x - p.x
    val dy = q.y - p.y
    Area(
      xRange = p.x to q.x by (if dx == 0 then 1 else dx.sign),
      yRange = p.y to q.y by (if dy == 0 then 1 else dy.sign)
    )

  def bounding[T](points: Map[Point, T]): Area =
    val xs = points.keys.map(_.x)
    val ys = points.keys.map(_.y)
    Area(
      xRange = xs.min to xs.max,
      yRange = ys.min to ys.max
    )

  def bounding(points: Set[Point]): Area =
    val xs = points.map(_.x)
    val ys = points.map(_.y)
    Area(
      xRange = xs.min to xs.max,
      yRange = ys.min to ys.max
    )

  def apply(left: Int, right: Int, top: Int, bot: Int): Area =
    Area(left to right, top to bot)

extension (p: Point) def xy = (p.x, p.y)

// TODO: Set/Map Grid
// TODO: 2D Grid
