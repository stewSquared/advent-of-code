import aoc.*

val input = io.Source.fromResource("2023/day-11.txt").getLines().toVector

val area = Area(input)

val galaxies = area.pointsIterator.filter(input(_) == '#').toVector

val xOccupied = galaxies.map(_.x).toSet
val xEmpty = area.xRange.filterNot(xOccupied).toSet

val yOccupied = galaxies.map(_.y).toSet
val yEmpty = area.yRange.filterNot(yOccupied).toSet

def dist(a: Point, b: Point, growth: Long): Long =
  val dx = b.x - a.x
  val dy = b.y - a.y
  val emptyCrossedX = if dx == 0 then 0 else (a.x to b.x by dx.sign).count(xEmpty)
  val emptyCrossedY = if dy == 0 then 0 else (a.y to b.y by dy.sign).count(yEmpty)
  a.dist(b) + emptyCrossedX * (growth - 1) + emptyCrossedY * (growth - 1)

def distances(growth: Long) = for
  i <- galaxies.indices
  j <- 0 until i
  g = galaxies(i)
  h = galaxies(j)
yield dist(g, h, growth)

val ans1 = distances(2).sum
val ans2 = distances(1_000_000).sum
