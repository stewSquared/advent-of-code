val input = io.Source.fromResource("2021/day-17-1.txt").getLines.next()

case class Point(x: Int, y: Int):
  def move(v: Point) = Point(x + v.x, y + v.y)
  def drag = Point(x - x.sign, y - 1)

object Point:
  final val Origin = Point(0, 0)

case class Area(left: Int, right: Int, bot: Int, top: Int):
  def contains(pos: Point) =
    (left to right).contains(pos.x) && (bot to top).contains(pos.y)

type Trajectory = Seq[Point]

extension (trajectory: Trajectory)
  def highest = trajectory.maxBy(_.y).y
  def hits(target: Area) = trajectory.exists(target.contains)

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
  def fire(vel: Point) = new reflect.Selectable {
    def at(target: Area): Trajectory =
      val start = Probe(Point.Origin, vel)
      val steps = Iterator.iterate(start)(_.step)
      steps.takeWhile(!_.missed(target)).map(_.pos).toSeq
  }

val target = input match
  case s"target area: x=$left..$right, y=$bot..$top" =>
    Area(left.toInt, right.toInt, bot.toInt, top.toInt)

import target.{left, right, bot}

def launches: Iterator[Trajectory] =
  for
    x <- math.sqrt(left).toInt.to(right).iterator
    y <- bot.abs to bot by -1
    velocity = Point(x, y)
  yield Probe fire velocity at target

val ans1 = launches.find(_ hits target).get.highest
val ans2 = launches.count(_ hits target)
