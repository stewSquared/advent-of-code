import aoc.{Area, Point, Dir}

val grid = io.Source.fromResource("2024/day-20.txt").getLines.toVector

val area = Area(grid)
val startPos = area.pointsIterator.find(grid(_) == 'S').get
val endPos = area.pointsIterator.find(grid(_) == 'E').get
val walls = area.pointsIterator.filter(grid(_) == '#').toSet

// todo don't need branching
def floodFill(blocked: Set[Point]): Iterator[Set[Point]] =
  Iterator.unfold(Set.empty[Point] -> Set(startPos)):
    case (prev, curr) => Option.when(curr.nonEmpty):
      val next = curr.flatMap(_.adjacent)
        .diff(prev)
        .diff(blocked)
        .filter(_.inBounds(area))

      curr -> (curr -> next)

val heatMap: Map[Point, Int] =
  floodFill(walls)
    .takeWhile(_.nonEmpty)
    .map(_.head)
    .zipWithIndex.toMap

heatMap(startPos)
val normalTime = heatMap(endPos)

def cheat(p: Point): Option[Int] =
  val pathPoints = p.adjacent.filter(area.contains).filterNot(walls)
  // Note: assuming no diagonal walls
  Option.when(pathPoints.size == 2):
    val List(a, b) = pathPoints.toList.sortBy(heatMap)
    heatMap(endPos) - heatMap(b) + heatMap(a) + 2

cheat(startPos.r)

// def cheat2(p: Point): List[Int] =

val innerWallPoints = walls.filter(area.expand(-1).contains).toList

innerWallPoints
  .flatMap(cheat)
  .count(((normalTime - 64) to (normalTime - 20)).contains)

val ans1 = innerWallPoints
  .flatMap(cheat)
  .count(_ <= normalTime - 100)


// val heatmap: Map[Point, Int] =

//
