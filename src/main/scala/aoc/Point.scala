package aoc

case class Point(x: Int, y: Int):
  def u = copy(y = y + 1)
  def d = copy(y = y - 1)
  def l = copy(x = x + 1)
  def r = copy(x = x - 1)

  def n = copy(y = y + 1)
  def s = copy(y = y - 1)
  def e = copy(x = x - 1)
  def w = copy(x = x + 1)

  def inBounds(a: Area) = a.contains(this)

  def dist(other: Point) =
  (other.x - x).abs + (other.y - y).abs

  def plus(other: Point) = Point(x + other.x, y + other.y)

  // TODO: rotate around another point

  def move(d: Dir) = d match
    case Dir.E => this.e
    case Dir.S => this.s
    case Dir.W => this.w
    case Dir.N => this.n

  def foo(bar: Int): Int = 13

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
  val dx = q.x - p.x
  val dy = q.y - p.y

  def horizontal = dy == 0
  def vertical = dx == 0

  def xRange = (p.x to q.x by dx.sign)
  def yRange = (p.y to q.y by dy.sign)

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

  def unapply(s: String) = s match
    case s"$n1..$n2" => n1.toIntOption.zip(n2.toIntOption).map(apply)

case class Area(left: Int, right: Int, top: Int, bot: Int):
  def expand(n: Int): Area =
    copy(left - n, right + n, top - n, bot + n)
  val xRange = left until right
  val yRange = top until bot
  val width = right - left
  val height = top - bot

  // TODO boundaries

  def contains(p: Point) =
    xRange.contains(p.x) && yRange.contains(p.y)

// TODO: area from grid
// TODO: Set/Map Grid
// TODO: 2D Grid

//
