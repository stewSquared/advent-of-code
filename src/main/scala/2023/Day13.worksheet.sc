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
  val shifts = 1 - area.right until area.right by 2
  val mirrorShift = shifts.find: shift =>
    val shifted = flipped.map(p => p.copy(x = p.x + shift))
    val intersectArea = area.intersect(Area.bounding(shifted)).get
    val intersect = shifted.intersect(points)
    val leftDiff = points.filter(intersectArea.contains).diff(intersect).size == smudge
    val rightDiff = shifted.filter(intersectArea.contains).diff(intersect).size == smudge
    leftDiff && rightDiff

  mirrorShift.map(shift => (area.xRange.size + shift) / 2)

def mirrors(points: Set[Point], area: Area, smudge: Int) =
  val vert = findMirror(points, area, smudge)
  val transposePoints = points.map(p => p.copy(x = p.y, y = p.x))
  val transposeArea = Area(area.yRange, area.xRange)
  val horiz = findMirror(transposePoints, transposeArea, smudge)
  (horiz, vert)

val ans1 =
  val (horiz, vert) = rockMaps.map(mirrors(_, _, 0)).unzip
  horiz.flatten.sum * 100 + vert.flatten.sum

val ans2 =
  val (horiz, vert) = rockMaps.map(mirrors(_, _, 1)).unzip
  horiz.flatten.sum * 100 + vert.flatten.sum
