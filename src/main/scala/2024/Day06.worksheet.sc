val input = io.Source.fromResource("2024/day-06.txt").getLines.toVector

import aoc.{Point, Area, Dir, Line}

val area = Area(input.toVector)

val obstacles = Set.from:
  for
    x <- area.xRange
    y <- area.yRange
    p = Point(x, y)
    if input(p) == '#'
  yield p

case class Guard(pos: Point, dir: Dir)

val initGuard: Guard = area
  .pointsIterator
  .collectFirst:
    case p if input(p) == '^' => Guard(p, Dir.N)
  .get

def walk(guard: Guard, obstacles: Set[Point]): Guard =
  val next = guard.pos.move(guard.dir)
  if obstacles(next) then
    Guard(guard.pos, guard.dir.turnRight)
  else
    Guard(next, guard.dir)

def walk(guard: Guard, obstaclesVert: Map[Int, Set[Point]], obstaclesHorz: Map[Int, Set[Point]]): Option[Guard] =
  guard match
    case Guard(pos@Point(x, y), dir) =>
      val pointsInWay =
        if dir.isVertical then
          obstaclesVert(x)
            .filter(o => if dir == Dir.N then o.y < y else o.y > y)
        else
          obstaclesHorz(y)
            .filter(o => if dir == Dir.W then o.x < x else o.x > x)

      Option.when(pointsInWay.nonEmpty):
        val endPos = pointsInWay.minBy(pos.dist).move(dir.reverse)
        Guard(endPos, dir.turnRight)

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

  Iterator.unfold(guard): guard =>
    val next = walk(guard, obstaclesVert, obstaclesHorz)
    next.map(g => (g, g))

val ans2 = (visited - initGuard.pos)
  .map(p => linePath(initGuard, obstacles + p))
  .filter(isLoop)
  .size
