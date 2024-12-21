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

val heatMap = path.zipWithIndex.toMap

heatMap(startPos)
val normalTime = heatMap(endPos)

def cheat(p: Point): Option[Int] =
  val pathPoints = p.adjacent.filter(area.contains).filterNot(walls)
  // Note: assuming no diagonal walls
  Option.when(pathPoints.size == 2):
    val List(a, b) = pathPoints.toList.sortBy(heatMap)
    heatMap(endPos) - heatMap(b) + heatMap(a) + 2

cheat(startPos.r)

def timeSaved(from: Point, to: Point): Int =
  // assert(heatMap(from) < heatMap(to)) // or maybe option
  - (heatMap(from) - heatMap(to) + from.dist(to))

def cheatable(from: Point): List[Point] =
  val surrounding = Area.bounding(Set(from)).expand(20)
  path.filter(surrounding.contains)
    .filter(to => timeSaved(from, to) >= 100)

def connectedByWalls(from: Point, to: Point): Boolean = true

// startPos
// Area.bounding(Set(startPos)).expand(20)
// path.filter(Area.bounding(Set(startPos)).expand(20).contains)
// path.filter(Area.bounding(Set(startPos)).expand(20).contains)
//   .map(timeSaved(startPos, _))
//   .count(_ >= 50)

// cheatable(startPos)

val ans2 = path
  .map: start =>
    cheatable(start).count: end =>
      connectedByWalls(start, end)
  .sum
// too high

// def cheat2(p: Point): List[Int] =

val innerWallPoints = walls.filter(area.expand(-1).contains).toList

innerWallPoints
  .flatMap(cheat)
  .count(((normalTime - 64) to (normalTime - 20)).contains)

// val ans1 = innerWallPoints
//   .flatMap(cheat)
//   .count(_ <= normalTime - 100)
