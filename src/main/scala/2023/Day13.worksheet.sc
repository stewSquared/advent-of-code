import aoc.*

val input = io.Source.fromResource("2023/day-13.txt").getLines()

val rockMaps: List[(Set[Point], Area)] =
  val lb = collection.mutable.ListBuffer.empty[(Set[Point], Area)]
  while input.hasNext do
    val grid = input.takeWhile(_.nonEmpty).toVector
    val area = Area(grid)
    val points = area.pointsIterator.filter(grid(_) == '#').toSet
    lb += (points -> area)
  lb.result()

def findMirror(points: Set[Point], area: Area, smudge: Int): Option[Int] =
  val flipped = points.map(p => Point(area.right - p.x, p.y))
  (1 to area.right).find: cols =>
    val shift = 2 * cols - area.width
    val shifted = flipped.map(p => p.copy(x = p.x + shift))
    val overlap = area.intersect(Area.bounding(shifted)).get
    points.filter(overlap(_)).diff(shifted).sizeIs == smudge

def score(points: Set[Point], area: Area, smudge: Int) =
  val vert = findMirror(points, area, smudge)
  val horiz = findMirror(points.map(_.swap), area.transpose, smudge)
  horiz.fold(0)(_ * 100) + vert.getOrElse(0)

val ans1 = rockMaps.map(score(_, _, 0)).sum
val ans2 = rockMaps.map(score(_, _, 1)).sum
