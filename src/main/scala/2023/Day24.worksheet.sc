import aoc.*

import scala.collection.immutable.BitSet

val input = io.Source.fromResource("2023/day-24.txt").getLines.toList

case class Position(x: Double, y: Double, z: Double):
  def +(that: Position): Position = Position(x + that.x, y + that.y, z + that.z)

case class Velocity(x: Double, y: Double, z: Double):
  def +(that: Velocity): Velocity = Velocity(x + that.x, y + that.y, z + that.z)
  def angle = math.atan2(y, x)

case class HailStone(position: Position, velocity: Velocity)
  // def next: HailStone = HailStone(position + velocity, velocity)

val hailStones: List[HailStone] = input.map:
  case s"$x, $y, $z @ $xv, $yv, $zv" =>
    HailStone(
      Position(x.toDouble, y.toDouble, z.toDouble),
      Velocity(xv.toDouble, yv.toDouble, zv.toDouble)
    )

val minBound = 200000000000000L
val maxBound = 400000000000000L

val bounds = (minBound to maxBound)

def angleBetween(angle: Double, min: Double, max: Double): Boolean =
  if min < max then
    min <= angle && angle <= max
  else
    min <= angle || angle <= max

def inPath(p: Position, h: HailStone): Boolean =

  // val a = HailStone(Position(1, 1, 0), Velocity(1, 2, 0))
  // inPath(Position(2, 3, 0), a) // true
  // inPath(Position(0, -1, 0), a) // false

  val xInPath =
    if h.velocity.x > 0 then
      p.x >= h.position.x
    else if h.velocity.x < 0 then
      p.x <= h.position.x
    else
      h.position.x == p.x

  val yInPath =
    if h.velocity.y > 0 then
      p.y >= h.position.y
    else if h.velocity.y < 0 then
      p.y <= h.position.y
    else
      h.position.y == p.y

  xInPath && yInPath

def intersect(a: HailStone, b: HailStone): Option[Position] =
  val axCoefficent = a.velocity.y / a.velocity.x
  val ayIntersect = a.position.y - axCoefficent * a.position.x
  // println(s"ya = $axCoefficent * xa + $ayIntersect")

  val bxCoefficent = b.velocity.y / b.velocity.x
  val byIntersect = b.position.y - bxCoefficent * b.position.x
  // println(s"yb = $bxCoefficent * xb + $byIntersect")

  val x = (byIntersect - ayIntersect) / (axCoefficent - bxCoefficent)
  val y = axCoefficent * x + ayIntersect

  val p = Position(x, y, 0)
  // println(p)
  Option.when(inPath(p, a) && inPath(p, b))(p)

val a = HailStone(Position(1, 1, 0), Velocity(1, 2, 0))
val b = HailStone(Position(4, 2, 0), Velocity(-2, 1, 0))

val a2 = HailStone(Position(1, 1, 0), Velocity(-1, -2, 0))
val b2 = HailStone(Position(4, 2, 0), Velocity(2, -1, 0))

inPath(Position(2, 3, 0), a)
inPath(Position(0, -1, 0), a)

intersect(a, b)
intersect(a2, b2)

val ans1 = hailStones.combinations(2).count:
  case List(a, b) =>
    intersect(a, b).exists: i =>
      // val minBound = 7
      // val maxBound = 27
      minBound <= i.x && i.x <= maxBound && minBound <= i.y && i.y <= maxBound



//
