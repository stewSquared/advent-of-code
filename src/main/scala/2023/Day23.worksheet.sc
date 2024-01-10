import aoc.*

import collection.immutable.BitSet

val maze = io.Source.fromResource("2023/day-23.txt").getLines.toVector

val area = Area(maze)

val slopes = Map.from[Point, Dir]:
  area.pointsIterator.collect:
    case p if maze(p) == '>' => p -> Dir.E
    case p if maze(p) == 'v' => p -> Dir.S
    case p if maze(p) == '<' => p -> Dir.W
    case p if maze(p) == '^' => p -> Dir.N

val walkable = Set.from[Point]:
  area.pointsIterator.filter(p => maze(p) != '#')

val start = walkable.minBy(_.y)
val end = walkable.maxBy(_.y)

val junctions: Set[Point] = walkable.filter: p =>
  p.adjacent.count(walkable) > 2
.toSet + start + end

def connectedJunctions(pos: Point) = List.from[(Point, Int)]:
  def walk(pos: Point, dir: Dir): Option[Point] =
    val p = pos.move(dir)
    Option.when(walkable(p) && slopes.get(p).forall(_ == dir))(p)

  def search(pos: Point, facing: Dir, dist: Int): Option[(Point, Int)] =
    if junctions.contains(pos) then Some(pos, dist) else
      val next = for
        nextFacing <- LazyList(facing, facing.turnRight, facing.turnLeft)
        nextPos <- walk(pos, nextFacing)
      yield search(nextPos, nextFacing, dist + 1)

      next.headOption.flatten

  for
    d <- Dir.values
    p <- walk(pos, d)
    junction <- search(p, d, 1)
  yield junction

def longestDownhillHike(pos: Point, dist: Int): Int =
  if pos == end then dist else
    connectedJunctions(pos).foldLeft(0):
      case (max, (n, d)) => max.max(longestDownhillHike(n, dist + d))

val ans1 = longestDownhillHike(start, 0)

type Index = Int
val indexOf: Map[Point, Index] =
  junctions.toList.sortBy(_.dist(start)).zipWithIndex.toMap

val fullAdj: Map[Index, List[(Index, Int)]] =
  junctions.toList.flatMap: p1 =>
    connectedJunctions(p1).flatMap: (p2, d) =>
      val forward = indexOf(p1) -> (indexOf(p2), d)
      val reverse = indexOf(p2) -> (indexOf(p1), d)
      List(forward, reverse)
  .groupMap(_._1)(_._2)

def longestHike(junction: Index, visited: BitSet, totalDist: Int): Int =
  if junction == indexOf(end) then totalDist else
    fullAdj(junction).foldLeft(0):
      case (max, (j, _)) if visited(j) => max
      case (max, (j, d)) => max.max(longestHike(j, visited + j, totalDist + d))

val ans2 = longestHike(indexOf(start), BitSet.empty, 0)
