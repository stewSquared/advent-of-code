import aoc.*

import scala.collection.immutable.BitSet

val input = io.Source.fromResource("2023/day-24.txt").getLines.toList

case class Position(x: Double, y: Double, z: Double):
  def +(that: Position): Position = Position(x + that.x, y + that.y, z + that.z)

object Position:
  def apply(x: Long, y: Long, z: Long): Position = Position(x.toDouble, y.toDouble, z.toDouble)

case class Velocity(x: Double, y: Double, z: Double):
  def +(that: Velocity): Velocity = Velocity(x + that.x, y + that.y, z + that.z)
  // def *(t: Double): Velocity = Velocity(x * t, y * t, z * t)
  def angle = math.atan2(y, x)

object Velocity:
  def apply(x: Long, y: Long, z: Long): Velocity = Velocity(x.toDouble, y.toDouble, z.toDouble)

case class HailStone(position: Position, velocity: Velocity):
  // def next: HailStone = HailStone(position + velocity, velocity)
  def move(t: Double): Position =
    Position(
      position.x + velocity.x * t,
      position.y + velocity.y * t,
      position.z + velocity.z * t
    )

object HailStone:
  def parse(s: String): HailStone =
    val s"$x, $y, $z @ $xv, $yv, $zv" = (s: @unchecked)
    HailStone(
      Position(x.toDouble, y.toDouble, z.toDouble),
      Velocity(xv.toDouble, yv.toDouble, zv.toDouble)
    )

val hailStones: List[HailStone] = input.map(HailStone.parse)

val foo = 100000000000L

hailStones.map(_.position.x).min
hailStones.map(_.position.x).max
hailStones.map(_.velocity.x).min
hailStones.map(_.velocity.x).max

hailStones.map(_.position.y).min
hailStones.map(_.position.y).max
hailStones.map(_.velocity.y).min
hailStones.map(_.velocity.y).max

hailStones.map(_.position.z).min
hailStones.map(_.position.z).max
hailStones.map(_.velocity.z).min
hailStones.map(_.velocity.z).max


// hailStones.map(h => h.velocity.x -> h.position.x).sortBy(_._1) foreach println

// hailStones.map(h => h.velocity.x -> h.position.x.round / foo).map(_.swap).sortBy(_._1).reverse foreach println

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

val h1 = HailStone.parse("18, 19, 22 @ -1, -1, -2")
val h2 = HailStone.parse("20, 19, 15 @ 1, -5, -3")

intersect(h1, h2)

val ans1 = hailStones.combinations(2).count:
  case List(a, b) =>
    intersect(a, b).exists: i =>
      // val minBound = 7
      // val maxBound = 27
      minBound <= i.x && i.x <= maxBound && minBound <= i.y && i.y <= maxBound
  case _ => ???


def times(rockGuess: HailStone) =
  val xTimes = hailStones.map:
    case HailStone(Position(x, _, _), Velocity(xv, _, _)) =>
      (x - rockGuess.position.x) / (rockGuess.velocity.x - xv)
  val yTimes = hailStones.map:
    case HailStone(Position(_, y, _), Velocity(_, yv, _)) =>
      (y - rockGuess.position.y) / (rockGuess.velocity.y - yv)
  val zTimes = hailStones.map:
    case HailStone(Position(_, _, z), Velocity(_, _, zv)) =>
      (z - rockGuess.position.z) / (rockGuess.velocity.z - zv)

  xTimes.zip(yTimes).zip(zTimes).map:
    case ((x, y), z) => (x, y, z)

val startCandidates = for
  case (x, xv) <- List(0.0 -> -500.0, 3.0*minBound -> 500.0)
  case (y, yv) <- List(0.0 -> -500.0, 3.0*minBound -> 500.0)
  case (z, zv) <- List(0.0 -> -500.0, 3.0*minBound -> 500.0)
yield HailStone(Position(x, y, z), Velocity(xv, yv, zv))

def dev(times: List[(Double, Double, Double)]) = times.map:
  case (xt, yt, zt) =>
    val a = (xt + yt + zt) / 3
    (xt - a).abs + (yt - a).abs + (zt - a).abs
    // math.sqrt((xt - a) * (xt - a) + (yt - a) * (yt - a) + (zt - a) * (zt - a))

def dev3(times: List[(Double, Double, Double)]) = times.map:
  case (xt, yt, zt) =>
    val a = (xt + yt + zt) / 3
    (xt, yt, zt) -> ((xt - a).abs + (yt - a).abs + (zt - a).abs)


startCandidates.map(times).map(dev(_).sum) foreach println


def dev2(times: List[(Double, Double, Double)]) = times.map:
  case (xt, yt, zt) =>
    val a = (xt + yt + zt) / 3
    ((xt - a), (yt - a), (zt - a))
.unzip3 match
  case (xs, ys, zs) => (xs.sum, ys.sum, zs.sum)


def stdDevs(times: List[(Double, Double, Double)]) = times.map:
  case (xt, yt, zt) =>
    val a = (xt + yt + zt) / 3
    ((xt - a), (yt - a), (zt - a))
.unzip3 match
  case (xs, ys, zs) =>
    val xd = math.sqrt(xs.map(x => x * x).sum)
    val yd = math.sqrt(ys.map(y => y * y).sum)
    val zd = math.sqrt(zs.map(z => z * z).sum)
    (xd, yd, zd)

/*

    // math.sqrt((xt - a) * (xt - a) + (yt - a) * (yt - a) + (zt - a) * (zt - a))

startCandidates.map(times).map(dev2) foreach println

val c = startCandidates.head

dev2(times(c))
c
dev2(times(c.copy(position = c.position.copy(x = c.position.x + minBound / 2))))
dev2(times(c.copy(position = c.position.copy(x = c.position.x - minBound / 2))))

dev2(times(c))._1
dev2(times(c.copy(position = c.position.copy(x = c.position.x + minBound / 10))))._1
dev2(times(c.copy(position = c.position.copy(x = c.position.x + minBound / 8))))._1
dev2(times(c.copy(position = c.position.copy(x = c.position.x + minBound / 6))))._1
dev2(times(c.copy(position = c.position.copy(x = c.position.x + minBound / 4))))._1
dev2(times(c.copy(position = c.position.copy(x = c.position.x + minBound / 2))))._1

dev2(times(c.copy(position = c.position.copy(x = c.position.x + minBound))))._1
dev2(times(c.copy(position = c.position.copy(x = c.position.x - minBound / 10))))._1


2 + 2


times(HailStone(Position(minBound / 2, 0, 0), Velocity(-1000, 0, 0))).count(_._1 < 0)

times(HailStone(Position(0, 0, 0), Velocity(1000, 0, 0))).count(_._1 < 0)
times(HailStone(Position(-maxBound, 0, 0), Velocity(1000000, 0, 0))).count(_._1 < 0)
times(HailStone(Position(maxBound, 0, 0), Velocity(1000000, 0, 0))).count(_._1 < 0)
times(HailStone(Position(maxBound * 2, 0, 0), Velocity(1000000, 0, 0))).count(_._1 < 0)
times(HailStone(Position(maxBound * 2, 0, 0), Velocity(100, 0, 0))).count(_._1 < 0)
times(HailStone(Position(maxBound * 2, 0, 0), Velocity(1000, 0, 0))).count(_._1 < 0)
times(HailStone(Position(maxBound * 1.5, 0, 0), Velocity(500, 0, 0))).count(_._1 < 0)

times(HailStone(Position(0, 0, 0), Velocity(-500, 0, 0))).count(_._1 < 0)


hailStones.size

val s1 = HailStone.parse("191731833242514, 65972251560457, 316387608559380 @ 186, 455, -102")
val s2 = HailStone.parse("214215320269065, 278593244545934, 264845346123318 @ -21, -42, -153")
val s3 = HailStone.parse("186161461797496, 290505104611629, 104676336120728 @ 148, -65, 437")

*/

// (x1 - xr) / (vxr - vx1) = (y1 - yr) / (vyr - vy1)
// (vyr - vy1) * (x1 - xr) = (y1 - yr) * (vxr - vx1)
// vyr*x1 - vyr*xr - vy1*x1 + vy1*xr = y1*vxr - y1*vx1 - yr*vxr + yr*vx1

// vyr*x1 - vy1*x1 + vy1*xr = y1*vxr - y1*vx1 - yr*vxr + yr*vx1 + vyr*xr
// vyr*x1 - vy1*x1 + vy1*xr - (y1*vxr - y1*vx1 - yr*vxr + yr*vx1) = vyr*xr
// vyr*xr = vyr*x1 - vy1*x1 + vy1*xr - (y1*vxr - y1*vx1 - yr*vxr + yr*vx1)
// vyr*xr = vyr*x1 - vy1*x1 + vy1*xr - y1*vxr + y1*vx1 + yr*vxr - yr*vx1

// (x2 - xr) / (vxr - vx2) = (y2 - yr) / (vyr - vy2)
// (vyr - vy2) * (x2 - xr) = (y2 - yr) * (vxr - vx2)
// vyr*x2 - vyr*xr - vy2*x2 + vy2*xr = y2*vxr - y2*vx2 - yr*vxr + yr*vx2
// vyr*x2 - vy2*x2 + vy2*xr = y2*vxr - y2*vx2 - yr*vxr + yr*vx2 + vyr*xr
// vyr*x2 - vy2*x2 + vy2*xr - (y2*vxr - y2*vx2 - yr*vxr + yr*vx2) = vyr*xr
// vyr*xr = vyr*x2 - vy2*x2 + vy2*xr - (y2*vxr - y2*vx2 - yr*vxr + yr*vx2)
// vyr*xr = vyr*x2 - vy2*x2 + vy2*xr - y2*vxr + y2*vx2 + yr*vxr - yr*vx2

// vyr*x1 - vy1*x1 + vy1*xr - y1*vxr + y1*vx1 + yr*vxr - yr*vx1 = vyr*x2 - vy2*x2 + vy2*xr - y2*vxr + y2*vx2 + yr*vxr - yr*vx2
// vyr*x1 - vy1*x1 + vy1*xr - y1*vxr + y1*vx1 - yr*vx1 = vyr*x2 - vy2*x2 + vy2*xr - y2*vxr + y2*vx2 - yr*vx2

// get other equations like this
// vyr*x1 - vy1*x1 + vy1*xr - y1*vxr + y1*vx1 - yr*vx1 = vyr*x2 - vy2*x2 + vy2*xr - y2*vxr + y2*vx2 - yr*vx2


// vyr*x3 - vy3*x3 + vy3*xr - y3*vxr + y3*vx3 - yr*vx3 = vyr*x2 - vy2*x2 + vy2*xr - y2*vxr + y2*vx2 - yr*vx2


// vyr*x1 + vy1*xr - y1*vxr - yr*vx1 - vyr*x2 - vy2*xr + y2*vxr + yr*vx2 = - vy2*x2 + y2*vx2 + vy1*x1 - y1*vx1
// vyr*x1 - vyr*x2 + vy1*xr - vy2*xr + y2*vxr - y1*vxr + yr*vx2 - yr*vx1 = - vy2*x2 + y2*vx2 + vy1*x1 - y1*vx1
// xr(vy1 - vy2) + vxr(y2 - y1) + yr(vx2 - vx1) + vyr*(x1 - x2) = - vy2*x2 + y2*vx2 + vy1*x1 - y1*vx1

// xr(vy1 - vy2) + vxr(y2 - y1) + yr(vx2 - vx1) + vyr*(x1 - x2) = y2*vx2 - y1*vx1 + vy1*x1 - vy2*x2





// xr(vy3 - vy2) + vxr(y2 - y3) + yr(vx2 - vx3) + vyr*(x3 - x2)                                 = y2*vx2 - y3*vx3 + vy3*x3 - vy2*x2
// xr(vz1 - vz2) + vxr(z2 - z1) +             0               0 + zr(vx2 - vx1) + vzr*(x1 - x2) = z2*vx2 - z1*vx1 + vz1*x1 - vz2*x2
// xr(vz1 - vz3) + vxr(z3 - z1) +             0 +             0 + zr(vx3 - vx1) + vzr*(x1 - x3) = z3*vx3 - z1*vx1 + vz1*x1 - vz3*x3
// xr(vz3 - vz2) + vxr(z2 - z3) +             0 +             0 + zr(vx2 - vx3) + vzr*(x3 - x2) = z2*vx2 - z3*vx3 + vz3*x3 - vz2*x2



// xr(vy1 - vy2) + vxr(y2 - y1) + yr(vx2 - vx1) + vyr*(x1 - x2)                                 = y2*vx2 - y1*vx1 + vy1*x1 - vy2*x2
// xr(vy1 - vy3) + vxr(y3 - y1) + yr(vx3 - vx1) + vyr*(x1 - x3)                                 = y3*vx3 - y1*vx1 + vy1*x1 - vy3*x3
// xr(vy3 - vy2) + vxr(y2 - y3) + yr(vx2 - vx3) + vyr*(x3 - x2)                                 = y2*vx2 - y3*vx3 + vy3*x3 - vy2*x2
// xr(vz1 - vz2) + vxr(z2 - z1) +             0               0 + zr(vx2 - vx1) + vzr*(x1 - x2) = z2*vx2 - z1*vx1 + vz1*x1 - vz2*x2
// xr(vz1 - vz3) + vxr(z3 - z1) +             0 +             0 + zr(vx3 - vx1) + vzr*(x1 - x3) = z3*vx3 - z1*vx1 + vz1*x1 - vz3*x3
// xr(vz3 - vz2) + vxr(z2 - z3) +             0 +             0 + zr(vx2 - vx3) + vzr*(x3 - x2) = z2*vx2 - z3*vx3 + vz3*x3 - vz2*x2




// gaussian elimination

// xr(vy1 - vy2) + vxr(y2 - y1) + yr(vx2 - vx1) + vyr*(x1 - x2)                                 = y2*vx2 - y1*vx1 + vy1*x1 - vy2*x2
// xr(vy1 - vy3) + vxr(y3 - y1) + yr(vx3 - vx1) + vyr*(x1 - x3)                                 = y3*vx3 - y1*vx1 + vy1*x1 - vy3*x3
// xr(vy3 - vy2) + vxr(y2 - y3) + yr(vx2 - vx3) + vyr*(x3 - x2)                                 = y2*vx2 - y3*vx3 + vy3*x3 - vy2*x2
// xr(vz1 - vz2) + vxr(z2 - z1) +             0               0 + zr(vx2 - vx1) + vzr*(x1 - x2) = z2*vx2 - z1*vx1 + vz1*x1 - vz2*x2
// xr(vz1 - vz3) + vxr(z3 - z1) +             0 +             0 + zr(vx3 - vx1) + vzr*(x1 - x3) = z3*vx3 - z1*vx1 + vz1*x1 - vz3*x3
// xr(vz3 - vz2) + vxr(z2 - z3) +             0 +             0 + zr(vx2 - vx3) + vzr*(x3 - x2) = z2*vx2 - z3*vx3 + vz3*x3 - vz2*x2

// (vy1 - vy2) / (vz3 - vz2)

// attempt two at getting equations
// xr(vy1 - vy2) + vxr(y2 - y1) + yr(vx2 - vx1) + vyr(x1 - x2) +                              = y2*vx2 - y1*vx1 + vy1*x1 - vy2*x2
// xr(vy2 - vy3) + vxr(y3 - y2) + yr(vx3 - vx2) + vyr(x2 - x3) +                              = y3*vx3 - y2*vx2 + vy2*x2 - vy3*x3
// xr(vz1 - vz2) + vxr(z2 - z1) +           0.0 +          0.0 + zr(vx2 - vx1) + vzr*(x1 - x2) = z2*vx2 - z1*vx1 + vz1*x1 - vz2*x2
// xr(vz2 - vz3) + vxr(z3 - z2) +           0.0 +          0.0 + zr(vx3 - vx2) + vzr*(x2 - x3) = z3*vx3 - z2*vx2 + vz2*x2 - vz3*x3
//                                yr(vz1 - vz2) + vyr(z2 - z1) + zr(vy2 - vy1) + vzr*(y1 - y2) = z2*vy2 - z1*vy1 + vz1*y1 - vz2*y2
//                                yr(vz2 - vz3) + vyr(z3 - z2) + zr(vy3 - vy2) + vzr*(y2 - y3) = z3*vy3 - z2*vy2 + vz2*y2 - vz3*y3



Vector(vy1 - vy2, y2 - y1, vx2 - vx1, x1 - x2,       0.0,     0.0, y2*vx2 - y1*vx1 + vy1*x1 - vy2*x2)
Vector(vy2 - vy3, y3 - y2, vx3 - vx2, x2 - x3,       0.0,     0.0, y3*vx3 - y2*vx2 + vy2*x2 - vy3*x3)
Vector(vz1 - vz2, z2 - z1,       0.0,     0.0, vx2 - vx1, x1 - x2, z2*vx2 - z1*vx1 + vz1*x1 - vz2*x2)
Vector(vz2 - vz3, z3 - z2,       0.0,     0.0, vx3 - vx2, x2 - x3, z3*vx3 - z2*vx2 + vz2*x2 - vz3*x3)
Vector(      0.0,     0.0, vz1 - vz2, z2 - z1, vy2 - vy1, y1 - y2, z2*vy2 - z1*vy1 + vz1*y1 - vz2*y2)
Vector(      0.0,     0.0, vz2 - vz3, z3 - z2, vy3 - vy2, y2 - y3, z3*vy3 - z2*vy2 + vz2*y2 - vz3*y3)

val hs1 = hailStones(92)
val hs3 = hailStones(145)
val hs2 = hailStones(248)

val x1 = hs1.position.x
val y1 = hs1.position.y
val z1 = hs1.position.z
val vx1 = hs1.velocity.x
val vy1 = hs1.velocity.y
val vz1 = hs1.velocity.z
val x2 = hs2.position.x
val y2 = hs2.position.y
val z2 = hs2.position.z
val vx2 = hs2.velocity.x
val vy2 = hs2.velocity.y
val vz2 = hs2.velocity.z
val x3 = hs3.position.x
val y3 = hs3.position.y
val z3 = hs3.position.z
val vx3 = hs3.velocity.x
val vy3 = hs3.velocity.y
val vz3 = hs3.velocity.z

// Vector(vy1 - vy2, y2 - y1, vx2 - vx1, x1 - x2,       0.0,     0.0, y2*vx2 - y1*vx1 + vy1*x1 - vy2*x2)
// Vector(vy1 - vy3, y3 - y1, vx3 - vx1, x1 - x3,       0.0,     0.0, y3*vx3 - y1*vx1 + vy1*x1 - vy3*x3)
// Vector(vy3 - vy2, y2 - y3, vx2 - vx3, x3 - x2,       0.0,     0.0, y2*vx2 - y3*vx3 + vy3*x3 - vy2*x2)
// Vector(vz1 - vz2, z2 - z1,       0.0,     0.0, vx2 - vx1, x1 - x2, z2*vx2 - z1*vx1 + vz1*x1 - vz2*x2)
// Vector(vz1 - vz3, z3 - z1,       0.0,     0.0, vx3 - vx1, x1 - x3, z3*vx3 - z1*vx1 + vz1*x1 - vz3*x3)
// Vector(vz3 - vz2, z2 - z3,       0.0,     0.0, vx2 - vx3, x3 - x2, z2*vx2 - z3*vx3 + vz3*x3 - vz2*x2)


// val matrix = Vector(
//   Vector(vy1 - vy2, y2 - y1, vx2 - vx1, x1 - x2,       0.0,     0.0, y2*vx2 - y1*vx1 + vy1*x1 - vy2*x2),
//   Vector(vz3 - vz2, z2 - z3,       0.0,     0.0, vx2 - vx3, x3 - x2, z2*vx2 - z3*vx3 + vz3*x3 - vz2*x2),
//   Vector(vy3 - vy2, y2 - y3, vx2 - vx3, x3 - x2,       0.0,     0.0, y2*vx2 - y3*vx3 + vy3*x3 - vy2*x2),
//   Vector(vy1 - vy3, y3 - y1, vx3 - vx1, x1 - x3,       0.0,     0.0, y3*vx3 - y1*vx1 + vy1*x1 - vy3*x3),
//   Vector(vz1 - vz3, z3 - z1,       0.0,     0.0, vx3 - vx1, x1 - x3, z3*vx3 - z1*vx1 + vz1*x1 - vz3*x3),
//   Vector(vz1 - vz2, z2 - z1,       0.0,     0.0, vx2 - vx1, x1 - x2, z2*vx2 - z1*vx1 + vz1*x1 - vz2*x2),
// )

Vector(vy1 - vy2, y2 - y1, vx2 - vx1, x1 - x2,       0.0,     0.0, y2*vx2 - y1*vx1 + vy1*x1 - vy2*x2)
Vector(vy2 - vy3, y3 - y2, vx3 - vx2, x2 - x3,       0.0,     0.0, y3*vx3 - y2*vx2 + vy2*x2 - vy3*x3)
Vector(vz1 - vz2, z2 - z1,       0.0,     0.0, vx2 - vx1, x1 - x2, z2*vx2 - z1*vx1 + vz1*x1 - vz2*x2)
Vector(vz2 - vz3, z3 - z2,       0.0,     0.0, vx3 - vx2, x2 - x3, z3*vx3 - z2*vx2 + vz2*x2 - vz3*x3)
Vector(      0.0,     0.0, vz1 - vz2, z2 - z1, vy2 - vy1, y1 - y2, z2*vy2 - z1*vy1 + vz1*y1 - vz2*y2)
Vector(      0.0,     0.0, vz2 - vz3, z3 - z2, vy3 - vy2, y2 - y3, z3*vy3 - z2*vy2 + vz2*y2 - vz3*y3)

val matrix = Array(
  Array(vy1 - vy2, y2 - y1, vx2 - vx1, x1 - x2,       0.0,     0.0, y2*vx2 - y1*vx1 + vy1*x1 - vy2*x2),
  Array(vy2 - vy3, y3 - y2, vx3 - vx2, x2 - x3,       0.0,     0.0, y3*vx3 - y2*vx2 + vy2*x2 - vy3*x3),
  Array(vz1 - vz2, z2 - z1,       0.0,     0.0, vx2 - vx1, x1 - x2, z2*vx2 - z1*vx1 + vz1*x1 - vz2*x2),
  Array(vz2 - vz3, z3 - z2,       0.0,     0.0, vx3 - vx2, x2 - x3, z3*vx3 - z2*vx2 + vz2*x2 - vz3*x3),
  Array(      0.0,     0.0, vz1 - vz2, z2 - z1, vy2 - vy1, y1 - y2, z2*vy2 - z1*vy1 + vz1*y1 - vz2*y2),
  Array(      0.0,     0.0, vz2 - vz3, z3 - z2, vy3 - vy2, y2 - y3, z3*vy3 - z2*vy2 + vz2*y2 - vz3*y3),
)

def showZeroes(matrix: Array[Array[Double]]): Unit =
  for row <- matrix do
    for v <- row do
      // print(v)
      // print(" ")
      // print(v - v/100)
      if v.abs < 0.0005 then print("0.0 ")
      else
        val s = v.toString
        if s.length > 7 then
          print(s.take(4) + s.takeRight(3))
          print(" ")
        else
          print(s)
          print(" ")
      // else print("1 ")
      // print(if v == 0.0 then "0 " else "1 ")
    println()
  println("----------")

def reorderMatrix(n: Int, matrix: Array[Array[Double]]): Unit =
  val newPivotI = matrix.zipWithIndex.drop(n).map: (row, i) =>
    row(n) -> i
  .maxBy(_._1)._2

  for i <- matrix.head.indices do
    val tmp = matrix(newPivotI)(i)
    matrix(newPivotI)(i) = matrix(n)(i)
    matrix(n)(i) = tmp

def gaussianElimination(matrix: Array[Array[Double]]): Array[Double] =
  println("start")
  showZeroes(matrix)

  for n <- matrix.indices.dropRight(1) do
    reorderMatrix(n, matrix)
    val pivot = matrix(n)
    for m <- n + 1 until matrix.length /* if matrix(m)(n) != 0*/ do
      // if (pivot(n) == 0) { println(s"$m,$n") }
      // if (pivot(n).abs < 1.0) { assert(false, "pivot 0")}
      // if (pivot(n).abs < 0.05) {
      //   println(pivot.mkString(","))
      //   println(s"$m,$n")
      //   assert(false, s"pivot $n has 0 ${pivot.mkString(",")}")
      // }
      val factor = matrix(m)(n) / pivot(n)
      for k <- 0 until pivot.length do
        matrix(m)(k) -= matrix(n)(k) * factor

    println(s"after pivot $n")
    showZeroes(matrix)

  // assert echelon form
  // matrix.zipWithIndex.foreach:
  //   case (row, m) =>
  //     for n <- 0 until m do
  //       assert(matrix(m)(n).abs < 0.05, s"not echelon form: $m, $n")

  // now we reduce:

  for m <- matrix.indices.reverse.dropRight(1) do
  // for m <- 5 until 0 by -1 do
    val pivot = matrix(m)
    for mTarget <- (0 until m) do
      val factor = matrix(mTarget)(m) / matrix(m)(m)
      for k <- 0 until pivot.length do
        matrix(mTarget)(k) -= matrix(m)(k) * factor

  println("after reduce")
  showZeroes(matrix)
  for m <- matrix.indices do
    matrix(m)(matrix.length) /= matrix(m)(m)

  matrix.transpose.last

val example = Array(
  Array(2.0, 1.0, -1.0, 8.0),
  Array(-3.0, -1.0, 2.0, -11.0),
  Array(-2.0, 1.0, 2.0, -3.0),
)

gaussianElimination(example).toVector.map(_.round)

val Vector(xr, vxr, yr, vyr, zr, zyr) =
  gaussianElimination(matrix).toVector.map(_.round)

xr
yr
zr

val rock = HailStone(Position(xr, yr, zr), Velocity(vxr, vyr, zyr))

// val rock = HailStone(Position(133619443970449L, 263917577518425L, 180640699244167L), Velocity(314, 19, 197))

// val rock = HailStone(
//   Position(
//     133619443970449L,
//     263917577518425L,
//     180640699244167L),
//   Velocity(314, 19, 197))

val possibleRocks = for
  dx <- -10 to 10
  dy <- -10 to 10
  dz <- -10 to 10
yield HailStone(
  Position(
    133619443970449L.toDouble + dx,
    263917577518425L.toDouble + dy,
    180640699244167L.toDouble + dz),
  Velocity(314, 19, 197))

dev3(times(rock)) foreach println


dev(times(rock)).sum

times(rock)
dev(times(rock))
dev2(times(rock))
stdDevs(times(rock))

val ans2 = rock.position.x + rock.position.y + rock.position.z
ans2.toLong
// 578177720733040 too low
// 578177720733041 too low
// 578177720733042 too low
