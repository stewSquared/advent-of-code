import aoc.{Area, Point, Dir}

val grid = io.Source.fromResource("2024/day-20.txt").getLines.toVector

val area = Area(grid)
val startPos = area.pointsIterator.find(grid(_) == 'S').get
val endPos = area.pointsIterator.find(grid(_) == 'E').get
val walls = area.pointsIterator.filter(grid(_) == '#').toSet

val path: List[Point] =
  def search(pos: Point, visited: Set[Point]): List[Point] =
    pos.adjacent.diff(visited).diff(walls).headOption match
      case Some(next) => pos :: search(next, visited + pos)
      case None => List(pos)
  search(startPos, Set.empty)

val normalTime = path.zipWithIndex.toMap

def cheat(p: Point): Option[Int] =
  val pathPoints = p.adjacent.filter(area.contains).filterNot(walls)
  // Note: assuming no diagonal walls
  Option.when(pathPoints.size == 2):
    val List(a, b) = pathPoints.toList.sortBy(normalTime)
    normalTime(endPos) - normalTime(b) + normalTime(a) + 2

val innerWallPoints = walls.filter(area.expand(-1).contains).toList

val ans1 = innerWallPoints
  .flatMap(cheat)
  .count(_ <= normalTime(endPos) - 100)

def timeSaved(from: Point, to: Point): Int =
  - (normalTime(from) - normalTime(to) + from.dist(to))

def cheatable(from: Point): List[Point] =
  path.filter(to => from.dist(to) <= 20 && timeSaved(from, to) >= 100)

val ans2 = path.map(cheatable(_).size).sum
