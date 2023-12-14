import aoc.*

val grid = io.Source.fromResource("2023/day-14.txt").getLines().toVector

val area = Area(grid)

val rounded: Set[Point] = area.pointsIterator.filter(grid(_) == 'O').toSet
val squared: Set[Point] = area.pointsIterator.filter(grid(_) == '#').toSet

def rollNorth(p: Point, rounded: Set[Point]): Point =
  val occpuied = rounded union squared
  assert(rounded(p))
  Iterator.iterate(p)(_.n).drop(1)
    .takeWhile(area.contains)
    .takeWhile(!occpuied(_))
    .toList.lastOption.getOrElse(p)

val finalRounded = area.pointsIterator.filter(rounded).foldLeft(rounded):
  case (rounded, p) =>
    val end = rollNorth(p, rounded)
    rounded.excl(p).incl(end)

val ans1 = finalRounded.toList.map(p => area.height - p.y).sum

println:
  area.draw: p =>
    if rounded(p) then 'O'
    else if squared(p) then '#' else '.'

println:
  area.draw: p =>
    if finalRounded(p) then 'O'
    else if squared(p) then '#' else '.'
