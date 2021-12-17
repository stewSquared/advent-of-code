val input = io.Source.fromResource("2021/day-17-1.txt").getLines.next()

case class Point(x: Int, y: Int)

object Point:
  opaque type Position <: Point = Point
  opaque type Velocity <: Point = Point

  def Position(x: Int, y: Int): Position = Point(x, y)
  def Velocity(x: Int, y: Int): Velocity = Point(x, y)
  final val Origin = Position(0, 0)

  extension (p: Position) def move(v: Velocity) = Position(p.x + v.x, p.y + v.y)
  extension (v: Velocity) def drag = Velocity(v.x - v.x.sign, v.y - 1)

import Point.*

case class Area(left: Int, right: Int, bot: Int, top: Int):
  def contains(pos: Position) =
    (left to right).contains(pos.x) && (bot to top).contains(pos.y)

case class Probe(pos: Position, vel: Velocity):
  def step = copy(pos.move(vel), vel.drag)

  def missed(a: Area): Boolean =
    val falling = vel.y < 0
    val vertical = vel.x == 0
    val short = pos.x < a.left
    val below = pos.y < a.bot
    val beyond = pos.x > a.right
    short && vertical || below && falling || beyond

object Probe:
  opaque type Trajectory <: LazyList[Probe] = LazyList[Probe]
  extension (trajectory: Trajectory)
    def hits(target: Area) = trajectory
      .takeWhile(!_.missed(target))
      .exists(state => target.contains(state.pos))

  def launch(vel: Velocity): Trajectory =
    LazyList.iterate(Probe(Origin, vel))(_.step)

val target = input match
  case s"target area: x=$left..$right, y=$bot..$top" =>
    Area(left.toInt, right.toInt, bot.toInt, top.toInt)

import target.{left, right, bot}

val launches = for
  x <- math.sqrt(left).toInt to right
  y <- bot until bot.abs
yield Probe launch Velocity(x, y)

val ans1 = (bot * bot + bot) / 2
val ans2 = launches.count(_ hits target)
