val input = io.Source.fromResource("2024/day-06.txt").getLines.toList

import aoc.{Point, Area, Dir, Line}

val area = Area(input.toVector)

val obstacles = Set.from:
  for
    x <- area.xRange
    y <- area.yRange
    p = Point(x, y)
    if input(p.y)(p.x) == '#'
  yield p

case class Guard(pos: Point, dir: Dir)

val initGuard: Guard = area
  .pointsIterator
  .find: p =>
    val c = input(p.y)(p.x)
    c != '.' && c != '#'
  .map: p =>
    val c = input(p.y)(p.x)
    c match
      case '^' => Guard(p, Dir.N)
      case 'v' => Guard(p, Dir.S)
      case '<' => Guard(p, Dir.W)
      case '>' => Guard(p, Dir.E)
  .get

def walk(guard: Guard, obstacles: Set[Point]): Guard =
  val next = guard.pos.move(guard.dir)
  if obstacles(next) then
    Guard(guard.pos, guard.dir.turnRight)
  else
    Guard(next, guard.dir)

def walk(guard: Guard, obstaclesVert: Map[Int, Set[Point]], obstaclesHorz: Map[Int, Set[Point]]): (Line, Guard) =
  guard match
    case Guard(pos, dir@Dir.N) if area.topBorder.contains(pos) => Line(pos, pos.move(dir)) -> Guard(pos.move(dir), dir)
    case Guard(pos, dir@Dir.S) if area.botBorder.contains(pos) => Line(pos, pos.move(dir)) -> Guard(pos.move(dir), dir)
    case Guard(pos, dir@Dir.E) if area.rightBorder.contains(pos) => Line(pos, pos.move(dir)) -> Guard(pos.move(dir), dir)
    case Guard(pos, dir@Dir.W) if area.leftBorder.contains(pos) => Line(pos, pos.move(dir)) -> Guard(pos.move(dir), dir)

    case Guard(pos@Point(x, y), dir@(Dir.N | Dir.S)) =>
      val pointsInWay =
        obstaclesVert(x)
          .filter(o => if dir == Dir.N then o.y < y else o.y > y)

      if pointsInWay.isEmpty then
        val endPos = pos.copy(y = if dir == Dir.N then area.yRange.min else area.yRange.max)
        Line(pos, endPos) -> Guard(endPos, dir)
      else
        val endPos = pointsInWay.minBy(pos.dist).move(dir.reverse)
        val line = Line(pos, endPos)
        line -> Guard(endPos, dir.turnRight)

    case Guard(pos@Point(x, y), dir@(Dir.W | Dir.E)) =>
      val pointsInWay =
        obstaclesHorz(y)
          .filter(o => if dir == Dir.W then o.x < x else o.x > x)

      if pointsInWay.isEmpty then
        val endPos = pos.copy(x = if dir == Dir.W then area.xRange.min else area.xRange.max)
        Line(pos, endPos) -> Guard(endPos, dir)
      else
        val endPos = pointsInWay.minBy(pos.dist).move(dir.reverse)
        val line = Line(pos, endPos)
        line -> Guard(endPos, dir.turnRight)

val path = Iterator.iterate(initGuard)(walk(_, obstacles))

val visited = path.takeWhile(g => area.contains(g.pos)).map(_.pos).toSet

val ans1 = visited.size

def isLoop(path: Iterator[Guard]): Boolean =
  // use tortoise hare
  val (tPath, hPath) = path.duplicate
  hPath.next

  // functional approach
  while hPath.hasNext do
    val t = tPath.next
    val h = hPath.next
    if t == h then return true
    if hPath.hasNext then
      hPath.next
  false

def linePath(guard: Guard, obs: Set[Point]): Iterator[Guard] =
  val obstaclesVert: Map[Int, Set[Point]] = obs.groupBy(_.x).withDefaultValue(Set.empty)
  val obstaclesHorz: Map[Int, Set[Point]] = obs.groupBy(_.y).withDefaultValue(Set.empty)

  Iterator.iterate(guard)(walk(_, obstaclesVert, obstaclesHorz)._2)
    .takeWhile:
      case Guard(pos, _) => area.contains(pos)

val obstaclesVert: Map[Int, Set[Point]] = obstacles.groupBy(_.x)
val obstaclesHorz: Map[Int, Set[Point]] = obstacles.groupBy(_.y)

val ans2 = (visited - initGuard.pos)
  .map(p => linePath(initGuard, obstacles + p))
  .filter(isLoop)
  .size

// initGuard
// area.topBorder.contains(initGuard.pos)
// walk(initGuard, obstaclesVert, obstaclesHorz)
// linePath(initGuard, obstacles) foreach println
