val input = io.Source.fromResource("2022/day-18.txt").getLines().toList

val cubes = input.collect { case s"$x,$y,$z" =>
  Point(x.toInt, y.toInt, z.toInt)
}.toSet

val xRange = (cubes.map(_.x).min - 1) to (cubes.map(_.x).max + 1)
val yRange = (cubes.map(_.y).min - 1) to (cubes.map(_.y).max + 1)
val zRange = (cubes.map(_.z).min - 1) to (cubes.map(_.z).max + 1)

case class Point(x: Int, y: Int, z: Int):
  def inBounds(p: Point) =
    xRange.contains(x) && yRange.contains(y) && zRange.contains(z)

  def adj = Set(
    copy(x = x + 1),
    copy(y = y + 1),
    copy(z = z + 1),
    copy(x = x - 1),
    copy(y = y - 1),
    copy(z = z - 1)
  ).filter(inBounds)

val ans1 = cubes.iterator.map(_.adj.diff(cubes).size).sum

def search: Int =
  val start = Point(xRange.min, yRange.min, zRange.min)
  var visited = Set(start)
  val toVisit = collection.mutable.Queue(start)
  var exposed = 0
  while toVisit.nonEmpty do
    val next = toVisit.dequeue().adj.diff(visited)
    val (blocked, air) = next.partition(cubes)
    visited |= air
    toVisit ++= air
    exposed += blocked.size
  exposed

val ans2 = search
