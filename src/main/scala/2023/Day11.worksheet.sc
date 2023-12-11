import aoc.*

val input = io.Source.fromResource("2023/day-11.txt").getLines().toVector

val area = Area(input)
val galaxies = area.pointsIterator.filter(input(_) == '#').toVector

val xOccupied = galaxies.map(_.x).toSet
val yOccupied = galaxies.map(_.y).toSet

def dist(p: Point, q: Point, growth: Long): Long =
  val bounds = Area.bounding(p, q)
  val emptyX = bounds.xRange.count(!xOccupied(_))
  val emptyY = bounds.yRange.count(!yOccupied(_))
  p.dist(q) + (emptyX + emptyY) * (growth - 1)

def distances(growth: Long) = galaxies.combinations(2).collect:
  case Vector(g, h) => dist(g, h, growth)

val ans1 = distances(2).sum
val ans2 = distances(1_000_000).sum
