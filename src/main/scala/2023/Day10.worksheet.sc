import `2022`.adj
val grid = io.Source.fromResource("2023/day-10.txt").getLines().toVector

import aoc.*

val blockUnicode = '\u2588'

val area = Area(grid)

val start = area.pointsIterator.find(grid(_) == 'S').get

def adjacent(p: Point): Set[Point] =
  // Note: S is a J
  val n = Option.when(p.n.inBounds(area) && "7|F".contains(grid(p.n)) && "SJ|L".contains(grid(p)))(p.n)
  val s = Option.when(p.s.inBounds(area) && "J|L".contains(grid(p.s)) && "7|F".contains(grid(p)))(p.s)
  val e = Option.when(p.e.inBounds(area) && "7-J".contains(grid(p.e)) && "L-F".contains(grid(p)))(p.e)
  val w = Option.when(p.w.inBounds(area) && "L-F".contains(grid(p.w)) && "7-JS".contains(grid(p)))(p.w)
  List(n, s, e, w).flatten.filter(_.inBounds(area)).toSet

start
start.n
start.w
adjacent(start)
adjacent(start).diff(Set.empty)


def stepPoint(start: Point): Iterator[Set[Point]] =
  Iterator.unfold(Set.empty[Point] -> Set(start)):
    case (visited, current) =>
      Option.when(current.nonEmpty):
        val next = current.flatMap(adjacent).diff(visited)
        (current, current -> next)

val ps = stepPoint(start)

val ans1 = stepPoint(start).toList.size - 1

val boundaryPoints = stepPoint(start).reduce(_ union _)

area.draw(p => if boundaryPoints.contains(p) then grid(p) else '.')

def nextDir(pos: Point, dir: Dir) =
  (dir, pos.move(dir)) match
    case (Dir.N, p) if grid(p) == '|' => dir
    case (Dir.N, p) if grid(p) == '7' => Dir.W
    case (Dir.N, p) if grid(p) == 'F' => Dir.E
    case (Dir.S, p) if grid(p) == '|' => dir
    case (Dir.S, p) if grid(p) == 'J' => Dir.W
    case (Dir.S, p) if grid(p) == 'L' => Dir.E
    case (Dir.E, p) if grid(p) == '-' => dir
    case (Dir.E, p) if grid(p) == 'J' => Dir.N
    case (Dir.E, p) if grid(p) == '7' => Dir.S
    case (Dir.W, p) if grid(p) == '-' => dir
    case (Dir.W, p) if grid(p) == 'L' => Dir.N
    case (Dir.W, p) if grid(p) == 'F' => Dir.S

def walk = Iterator.unfold[(List[Point], List[Point]), (Point, Dir)]((start, Dir.N)):
  case (pos, dir) =>
    val next = pos.move(dir)
    val left = pos.move(dir.turnLeft)
    val right = pos.move(dir.turnRight)
    Option.when(next != start):
      val leftSpace =
        // vas list = if nextDir(dir) != dir then List(left, )
        List(left, left.move(dir))//, left.move(dir), left.move(dir.turnLeft.turnLeft))
          .filter(_.inBounds(area))
          .filterNot(boundaryPoints)
      val rightSpace =
        List(right, right.move(dir))//, right.move(dir), right.move(dir.turnRight.turnRight))
          .filter(_.inBounds(area))
          .filterNot(boundaryPoints)
      // val leftSpace = Option.when(left.inBounds(area) && grid(left) == '.')(left)
      // val rightSpace = Option.when(right.inBounds(area) && grid(right) == '.')(right)
      val newState = (next, nextDir(pos, dir))
      (leftSpace, rightSpace) -> newState


val (left, right) = walk.toList.unzip

val leftSeeds = left.flatten.toSet
val rightSeeds = right.flatten.toSet

leftSeeds.intersect(rightSeeds).size

def flood(seeds: Set[Point]): Set[Point] =
  val points = Iterator.unfold(Set.empty[Point] -> seeds):
    case (visited, current) =>
      Option.when(current.nonEmpty):
        val next = current.flatMap(_.adjacent.filter(area.contains)).diff(boundaryPoints).diff(visited)
        (current, current -> next)
  points.reduce(_ union _)

val leftFlood = flood(leftSeeds)
val rightFlood = flood(rightSeeds)

leftSeeds.size
leftFlood.size
rightSeeds.size
rightFlood.size

// val spacePoints = area.pointsIterator.filter(grid(_) == '.').toSet

rightFlood.size
leftFlood.size
boundaryPoints.size
area.size
rightFlood.size + leftFlood.size + boundaryPoints.size
area.size - (rightFlood.size + leftFlood.size + boundaryPoints.size)

val unaccounted = area.pointsIterator.toSet.diff(rightFlood).diff(leftFlood).diff(boundaryPoints)
unaccounted.size

rightFlood.contains(Point.origin)
leftFlood.contains(Point.origin)
// false is inside

leftFlood.intersect(rightFlood).size

area.draw: p =>
  if leftFlood.contains(p) then 'O'
  else if rightFlood.contains(p) then 'I'
  else if boundaryPoints.contains(p) then blockUnicode
  else if unaccounted.contains(p) then 'X'
  // else if boundaryPoints.contains(p) then grid(p)
  // else if unaccounted.contains(p) then blockUnicode
  else grid(p)

val ans2 = rightFlood.size

// rightFlood.diff(boundaryPoints).

// val (left, right) = walk.unzip

// def walk(current: Point, dir: Dir, )



// area.xRange.dropRight(1)

// val tileCorners = Area(area.xRange.dropRight(1), area.yRange.dropRight(1))

// def cornerTiles(p: Point): Option[(Point, Point, Point, Point)] =
//   Option.when(tileCorners.contains(p)):
//     (p, p.e, p.s, p.se)

// def adjCornerTiles(p: Point): Set[Point] =
//   val (nw, ne, sw, se) = cornerTiles(p).get
//   val n = Option.when(p.n.inBounds(tileCorners) && ".|7JS".contains(grid(nw)) && ".|LF".contains(grid(ne)))(p.n)
//   val s = Option.when(p.s.inBounds(tileCorners) && ".|7JS".contains(grid(sw)) && ".|LF".contains(grid(se)))(p.s)
//   val e = Option.when(p.e.inBounds(tileCorners) && ".-LJS".contains(grid(ne)) && ".-7F".contains(grid(se)))(p.e)
//   val w = Option.when(p.w.inBounds(tileCorners) && ".-LJS".contains(grid(nw)) && ".-7F".contains(grid(sw)))(p.w)
//   List(n, s, e, w).flatten.toSet


// adjCornerTiles(Point(0, 0))

// val (nw, ne, sw, se) = cornerTiles(Point.origin).get
// grid(sw)
// grid(se)

// val outsideCornerPoints = Iterator.unfold(Set.empty[Point] -> Set(Point.origin)):
//   (visited, current) =>
//     Option.when(current.nonEmpty):
//       val next = current.flatMap(adjCornerTiles).diff(visited)
//       (current, current -> next)

// val s = outsideCornerPoints.reduce(_ union _)
// val outsideAndBoundaries = s.union(s.map(_.e)).union(s.map(_.s)).union(s.map(_.se))

// outsideAndBoundaries.size

// def print(p: Point): Char =
//   if boundaryPoints.contains(p) then grid(p)
//   else if outsideAndBoundaries.contains(p) then 'O'
//   else 'I'

// area.draw(print)

// area.draw(p => grid(p))

area.draw(p => if grid(p) == '.' then '.' else blockUnicode)

// val ans2 = area.size - outsideAndBoundaries.size

// //
