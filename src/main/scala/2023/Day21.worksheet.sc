import aoc.*

val grid = io.Source.fromResource("2023/day-21.txt").getLines.toVector

val area = Area(grid)
val start = area.pointsIterator.find(p => grid(p) == 'S').get
val rocks = area.pointsIterator.filter(p => grid(p) == '#').toSet

def stepsFrom(start: Point) = LazyList.unfold(Set(start)): current =>
  val next = current.flatMap(_.adjacent).filter(area.contains).diff(rocks)
  Option.when(current.nonEmpty):
    current -> next

val steps = stepsFrom(start)

val ans1 = steps(64).size

val maxSteps = 26501365L

val centerRight = start.copy(x = area.right)
val centerLeft = start.copy(x = area.left)
val centerTop = start.copy(y = area.top)
val centerBot = start.copy(y = area.bot)

val cornerPoints = List(area.botLeft, area.topRight, area.botRight, area.topLeft)
val midPoints = List(centerRight, centerLeft, centerTop, centerBot)

val fullGrids = (maxSteps - start.dist(area.topRight)) / area.width
val stepsRemaining = (maxSteps.toInt - start.dist(area.topRight)) % area.width

val smallCornerSteps = stepsRemaining - 2
val largeCornerSteps = stepsRemaining + area.topRight.dist(area.topLeft) - 1
val farCornerSteps = stepsRemaining + area.topRight.dist(centerRight) - 1

val oddCovering = steps(start.dist(area.topRight) + 1)
val evenCovering = steps(start.dist(area.topRight))

val smallCorners = cornerPoints.map(stepsFrom(_)(smallCornerSteps))
val largeCorners = cornerPoints.map(stepsFrom(_)(largeCornerSteps))
val farCorners = midPoints.map(stepsFrom(_)(farCornerSteps))

val ans2 = List(
  evenCovering.size.toLong * (fullGrids + 1) * (fullGrids + 1),
  oddCovering.size.toLong * fullGrids * fullGrids,
  smallCorners.map(_.size.toLong).sum * (fullGrids + 1),
  largeCorners.map(_.size.toLong).sum * fullGrids,
  farCorners.map(_.size.toLong).sum,
).sum
