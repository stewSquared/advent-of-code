import aoc.*

val grid = io.Source.fromResource("2023/day-21.txt").getLines.toVector

val largeGrid =
  val widened = grid.map(row => row + row + row)
  widened ++ widened ++ widened

val area = Area(grid)
val largeArea = Area(largeGrid)

val xRange = grid.head.indices
val yRange = grid.indices

def inBounds(p: Point) = xRange.contains(p.x) && yRange.contains(p.y)

val start: Point = area.pointsIterator.find(p => grid(p) == 'S').get

val rocks = area.pointsIterator.filter(p => grid(p) == '#').toSet
rocks.size

val largeRocks = largeArea.pointsIterator.filter(p => largeGrid(p) == '#').toSet
largeRocks.size
rocks.size * 9

rocks(start)
rocks(start.e)
rocks(start.s.e)

val steps = LazyList.unfold(Set.empty[Point], Set(start)):
  case (prev, current) =>
    val next = current
      .flatMap(_.adjacent)
      .filter(inBounds)
      .diff(rocks)
      // .diff(prev)
    Option.when(next.nonEmpty):
      current -> (current, next)

def stepsFrom(start: Point) = LazyList.unfold(Set.empty[Point], Set(start)):
  case (prev, current) =>
    val next = current
      .flatMap(_.adjacent)
      .filter(inBounds)
      .diff(rocks)
      // .diff(prev)
    Option.when(next.nonEmpty):
      current -> (current, next)

val ans1 = steps(64).size

show(65)

def show(n: Int): Unit =
  val reachable = steps(n)
  println:
    area.draw:
      // case p if p == start => 'S'
      case p if reachable(p) => 'O'
      case p if rocks(p) => '#'
      case _ => '.'


val starts = List(area.topLeft, area.botLeft, area.topRight, area.botRight,
  start.copy(x = area.left), start.copy(x = area.right),
  start.copy(y = area.top), start.copy(y = area.bot))

val distances = List(64,64,64,64,130,130,130,130)

2 + 2

(starts.zip(distances)).foreach: (s,d) =>
  println:
    val reachable = stepsFrom(s)(d)
    area.draw:
      // case p if p == start => 'S'
      case p if reachable(p) => 'O'
      case p if rocks(p) => '#'
      case _ => '.'


26501365L * 26501365L

area.size[Long]
area.width
area.height

def totalReachable(n: Int) = (n.toLong + 1) * (n.toLong + 1)

start.dist(area.topRight)
start.dist(area.botLeft)
start

val largeStart = Point(x = start.x + area.width, y = start.y + area.height)
val largeSteps = LazyList.unfold(Set.empty[Point], Set(largeStart)):
  case (prev, current) =>
    val next = current
      .flatMap(_.adjacent)
      .filter(largeArea.contains)
      .diff(largeRocks)
      // .diff(prev)
    Option.when(next.nonEmpty):
      current -> (current, next)


largeSteps(26501365 % area.width + area.width).size

val maxSteps = 26501365L
// val maxSteps = 26501365L % area.width + area.width
// val maxSteps = 65

steps(65).size

maxSteps % area.width
maxSteps / area.width

start.dist(area.topRight)
(maxSteps - start.dist(area.topRight)) % area.width
(maxSteps - start.dist(area.topRight)) / area.width
(maxSteps - start.dist(area.botLeft)) / area.width

area.width * 202299L + start.dist(area.topRight) + 66

val maxGridsAway = ((maxSteps - start.dist(area.botLeft)) / area.width).toLong
val gridsCovered = (maxGridsAway * (maxGridsAway + 1L) / 2L) * 4 + 1

val oddGridsCovered = (maxGridsAway * (maxGridsAway + 1L) / 2L) * 2
val evenGridsCovered = (maxGridsAway * (maxGridsAway + 1L) / 2L) * 2 + 1

val oddCovering = steps(151).size.toLong
val evenCovering = steps(150).size.toLong

show(151)

stepsFrom(area.botLeft)(64).size
stepsFrom(area.botLeft)(195).size

val topRightCornersSmall = stepsFrom(area.botLeft)(64).size.toLong
val botLeftCornersSmall = stepsFrom(area.topRight)(64).size.toLong
val topLeftCornersSmall = stepsFrom(area.botRight)(64).size.toLong
val botRightCornersSmall = stepsFrom(area.topLeft)(64).size.toLong

val topRightCornersBig = stepsFrom(area.botLeft)(195).size.toLong
val botLeftCornersBig = stepsFrom(area.topRight)(195).size.toLong
val topLeftCornersBig = stepsFrom(area.botRight)(195).size.toLong
val botRightCornersBig = stepsFrom(area.topLeft)(195).size.toLong

// oddCovering * gridsCovered
oddCovering * oddGridsCovered
oddCovering * evenGridsCovered
stepsFrom(start.copy(x = area.right))(130).size.toLong
stepsFrom(start.copy(x = area.left))(130).size.toLong
stepsFrom(start.copy(y = area.top))(130).size.toLong
stepsFrom(start.copy(y = area.bot))(130).size.toLong
maxGridsAway * topRightCornersBig
maxGridsAway * botLeftCornersBig
maxGridsAway * topLeftCornersBig
maxGridsAway * botRightCornersBig
(maxGridsAway + 1) * topRightCornersSmall
(maxGridsAway + 1) * botLeftCornersSmall
(maxGridsAway + 1) * topLeftCornersSmall
(maxGridsAway + 1) * botRightCornersSmall




start.copy(x = area.right).dist(start.copy(x = area.left))

val ans2 = List(
// oddCovering * gridsCovered,
// oddCovering * oddGridsCovered,
// evenCovering * evenGridsCovered,
oddCovering * maxGridsAway * maxGridsAway,
evenCovering * (maxGridsAway + 1) * (maxGridsAway + 1),
stepsFrom(start.copy(x = area.right))(130).size.toLong,
stepsFrom(start.copy(x = area.left))(130).size.toLong,
stepsFrom(start.copy(y = area.top))(130).size.toLong,
stepsFrom(start.copy(y = area.bot))(130).size.toLong,
maxGridsAway * topRightCornersBig,
maxGridsAway * botLeftCornersBig,
maxGridsAway * topLeftCornersBig,
maxGridsAway * botRightCornersBig,
(maxGridsAway + 1) * topRightCornersSmall,
(maxGridsAway + 1) * botLeftCornersSmall,
(maxGridsAway + 1) * topLeftCornersSmall,
(maxGridsAway + 1) * botRightCornersSmall,
).sum
30418 - 34310

// 621494536591286
// 621494515551920
// 619939383258648
// 621494536591256 is also wrong
// 621494536591248 is plain wrong
// 933016304163648 too high
// 120284987518923 too low
// 120284987522777 still too low
