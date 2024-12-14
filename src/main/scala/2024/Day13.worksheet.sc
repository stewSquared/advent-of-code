val input = io.Source.fromResource("2024/day-13.txt").getLines.toList

// import aoc.Point

case class Point(x: Long, y: Long):
  def plus(p: Point) = Point(x + p.x, y + p.y)
  def minus(p: Point) = Point(x - p.x, y - p.y)

  infix def -(p: Point) = minus(p)
  infix def +(p: Point) = plus(p)
  infix def *(n: Long) = Point(x * n, y * n)

object Point:
  val origin = Point(0L, 0L)

val aButtons = input.collect:
  case s"Button A: X+$x, Y+$y" => Point(x.toLong, y.toLong)

val bButtons = input.collect:
  case s"Button B: X+$x, Y+$y" => Point(x.toLong, y.toLong)

val prizes = input.collect:
  case s"Prize: X=$x, Y=$y" => Point(x.toLong, y.toLong)

case class Machine(
  a: Point,
  b: Point,
  prize: Point
)

val machines = aButtons.zip(bButtons).zip(prizes).map:
  case ((a, b), prize) => Machine(a, b, prize)

val machines2 = machines.map: m =>
  val off = 10000000000000L
  m.copy(prize = Point(m.prize.x + off, m.prize.y + off))

def winnable(m: Machine): Boolean =
  // necessary but not sufficient
  import aoc.math.{gcf, gcfN, lcm, lcmN}
  val xWorks = m.prize.x % gcf(m.a.x, m.b.x) == 0
  val yWorks = m.prize.y % gcf(m.a.y, m.b.y) == 0
  xWorks && yWorks

machines.count(winnable)
machines.flatMap(solve).size

machines
  .filter(m => solve(m).nonEmpty)
  .count(winnable)

def solve(m: Machine): Option[(Long, Long)] =
  import m.{a, b, prize => p}
  for
    tb <- locally:
      val n = a.y*p.x - a.x*p.y
      val d = b.x*a.y - b.y*a.x
      Option.when(n % d == 0)(n / d)
    ta <- locally:
      val n = p.y - b.y * tb
      val d = a.y
      Option.when(n % d == 0)(n / d)
  yield
    assert(a*ta + b*tb == p)
    assert(ta > 0)
    assert(tb > 0)
    // TODO: why is it always positive?
    ta -> tb

def cost(c: (Long, Long)): Long = c._1*3 + c._2

val ans1 = machines.flatMap(solve).map(cost) .sum
val ans2 = machines2.flatMap(solve).map(cost).sum
