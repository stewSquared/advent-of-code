val input = io.Source.fromResource("2024/day-04.txt").getLines().toList

def countXmas(s: String): Int =
  val occurences = Iterator.iterate(-1): i =>
    s.indexOf("XMAS", i + 1)
  occurences.drop(1).takeWhile(_ != -1).size

val right = input.map(countXmas).sum
val left = input.map(_.reverse).map(countXmas).sum
val up = input.transpose.map(_.mkString).map(countXmas).sum
val down = input.transpose.map(_.mkString.reverse).map(countXmas).sum

val shiftedRight = input.zipWithIndex.map:
  case (s, i) => " " * i + s + " " * (input.size - i - 1)

val rightUp = shiftedRight.transpose.map(_.mkString).map(countXmas).sum
val rightDown = shiftedRight.transpose.map(_.mkString).map(_.reverse).map(countXmas).sum

val shiftedLeft = input.zipWithIndex.map:
  case (s, i) => " " * (input.size - i - 1) + s + " " * i

val leftUp = shiftedLeft.transpose.map(_.mkString).map(countXmas).sum
val leftDown = shiftedLeft.transpose.map(_.mkString).map(_.reverse).map(countXmas).sum

val ans1 = right + left + up + down + rightUp + rightDown + leftUp + leftDown

val grid = input.map(_.toVector).toVector

import aoc.Area
import aoc.Point

val area = Area(grid)

val subAreas =
  for
    x <- 0 to area.xRange.last - 2
    y <- 0 to area.yRange.last - 2
  yield Area.bounding(Point(x, y), Point(x + 2, y + 2))

def countXmasDiagonal(subArea: Area): Int =
  if grid(subArea.topLeft.d.r) != 'A' then 0
  else
    val crosses = for
      (tl, br) <- "MS" zip "SM"
      (tr, bl) <- "MS" zip "SM"
    yield
      grid(subArea.topLeft) == tl && grid(subArea.botRight) == br &&
      grid(subArea.topRight) == tr && grid(subArea.botLeft) == bl
    if crosses.exists(identity) then 1 else 0

val ans2 = subAreas.map(countXmasDiagonal).sum
