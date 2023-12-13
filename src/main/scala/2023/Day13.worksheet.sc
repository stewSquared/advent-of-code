import scala.compiletime.ops.int
import aoc.*

val input = io.Source.fromResource("2023/day-13.txt").getLines()

val rockMaps: List[(Set[Point], Area)] =
  val lb = collection.mutable.ListBuffer.empty[(Set[Point], Area)]
  while input.hasNext do
    val grid = input.takeWhile(_.nonEmpty).toVector
    val area = Area(grid)
    val points = area.pointsIterator.filter(grid(_) == '#').toSet
      // .map(p => Point(area.xRange.max - p.x, area.yRange.max - p.y))
    lb += (points -> area)
  lb.result()

var (points, area) = rockMaps.head

var flipped = points.map(p => Point(area.right - p.x, p.y))

area.draw(p => if points(p) then '#' else '.')
area.draw(p => if flipped(p) then '#' else '.')

points = rockMaps(1)._1
area = rockMaps(1)._2
flipped = points.map(p => Point(area.right - p.x, p.y))
area.draw(p => if points(p) then '#' else '.')
area.draw(p => if flipped(p) then '#' else '.')


// rockMaps.map(_._2.xRange.size)
// 1 -> 5 == 9 / 2 + 1
// 3 -> 6 == 9 / 2 + 3 / 2
// 5 -> 7 == 9 / 2 + 5 / 2

def findMirror(points: Set[Point], area: Area): Option[Int] =
  val flipped = points.map(p => Point(area.right - p.x, p.y))
  val shifts = 1 - area.right until area.right by 2
  // println(shifts)
  val mirrorShift = shifts.find: shift =>
    val shifted = flipped.map(p => p.copy(x = p.x + shift))
    val shiftedArea = Area.bounding(shifted)
    val intersectArea = shiftedArea.intersect(area).get
    val intersect = shifted.intersect(points)
    // points.filter(intersectArea.contains) == intersect && shifted.filter(intersectArea.contains) == intersect
    val leftDiff = points.filter(intersectArea.contains).diff(intersect).size == 1
    val rightDiff = shifted.filter(intersectArea.contains).diff(intersect).size == 1
    leftDiff && rightDiff
    // val pass = shifted.diff(points).union(points) == points.diff(shifted).union(shifted)
    // println(shift)
    // area.draw(p => if points(p) then '#' else '.')
    // Area.bounding(shifted).draw(p => if shifted(p) then '#' else '.')

  mirrorShift.map(shift => (area.xRange.size + shift) / 2)

val mirrors = rockMaps.zipWithIndex.map[(Option[Int], Option[Int])]:
  case ((points, area), i) =>
    val vert = findMirror(points, area)
    val transposePoints = points.map(p => p.copy(x = p.y, y = p.x))
    val transposeArea = Area(area.yRange, area.xRange)
    val horiz = findMirror(transposePoints, transposeArea)
    if (vert.isEmpty && horiz.isEmpty) {
      println(s"no mirrors found for $i")
      println:
        area.draw(p => if points(p) then '#' else '.')
      println()
    }
    else vert -> horiz
    (horiz, vert)

mirrors foreach println


findMirror(rockMaps.head._1, rockMaps.head._2)

val ans1 =
  val (horiz, vert) = mirrors.unzip
  horiz.flatten.sum * 100 + vert.flatten.sum

//
