package aoc

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

case class Line(p: Point, q: Point):
  def dx = q.x - p.x
  def dy = q.y - p.y

  def horizontal = dy == 0
  def vertical = dx == 0

  def xRange = p.x to q.x by (if horizontal then dx.sign else 1)
  def yRange = p.y to q.y by (if vertical then dy.sign else 1)

  def points: Seq[Point] =
    if horizontal then xRange.map(Point(_, p.y))
    else if vertical then yRange.map(Point(p.x, _))
    else (xRange zip yRange).map(Point(_, _))

case class Interval(min: Int, max: Int):
  override def toString = s"$min..$max"

  def size = max + 1 - min
  def contains(n: Int) = min <= n && n <= max
  def toRange = scala.Range.inclusive(min, max)
  def iterator = toRange.iterator
  def supersetOf(r: Interval) = min <= r.min && r.max <= max

  def intersect(r: Interval): Option[Interval] =
    if supersetOf(r) then Some(r)
    else if r contains min then Some(copy(max = max.min(r.max)))
    else if r contains max then Some(copy(min = min.max(r.min)))
    else None

  def diff(n: Interval): List[Interval] =
    if min < n.min && n.max < max then
      List(copy(max = n.min - 1), copy(min = n.max + 1))
    else if n.min <= min && max <= n.max then Nil
    else if n contains max then List(copy(max = n.min - 1))
    else if n contains min then List(copy(min = n.max + 1))
    else List(this)

  def diff(intervals: Seq[Interval]): List[Interval] =
    intervals.foldLeft(List(this)) { (disjoint, n) =>
      disjoint.flatMap(_.diff(n))
    }

object Interval:
  def apply(n1: Int, n2: Int): Interval = new Interval(n1 min n2, n1 max n2)
  def apply(range: Range): Interval = new Interval(range.min, range.max)

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

  def contains(p: Point) =
    xRange.contains(p.x) && yRange.contains(p.y)

  def expand(n: Int): Area =
    copy(left - n to right + n, top - n to bot + n)

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

object Area:
  def apply(grid: IndexedSeq[IndexedSeq[_]]): Area =
    Area(
      xRange = grid.headOption.fold(0 to 0)(_.indices),
      yRange = grid.indices
    )

  def apply(grid: IndexedSeq[String])(using wrap: String => collection.immutable.WrappedString): Area =
    apply(grid.map(wrap))

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
