val input = io.Source.fromResource("2021/day-17-1.txt").getLines.next()

case class Point(x: Int, y: Int):
  def move(v: Point) = Point(x + v.x, y + v.y)
  def drag = Point(x - x.sign, y - 1)

object Point:
  final val Origin = Point(0, 0)

case class Area(left: Int, right: Int, bot: Int, top: Int):
  def contains(pos: Point) =
    (left to right).contains(pos.x) && (bot to top).contains(pos.y)

case class Probe(pos: Point, vel: Point):
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

  def launch(vel: Point): Trajectory =
    LazyList.iterate(Probe(Point.Origin, vel))(_.step)

val target = input match
  case s"target area: x=$left..$right, y=$bot..$top" =>
    Area(left.toInt, right.toInt, bot.toInt, top.toInt)

import target.{left, right, bot}

val launches = for
  x <- math.sqrt(left).toInt to right
  y <- bot.abs to bot by -1
  velocity = Point(x, y)
  trajectory = Probe launch velocity
  if trajectory hits target
yield trajectory

val ans1 = (bot * bot + bot) / 2
val ans2 = launches.size
