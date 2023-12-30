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

val nodes: Set[Point] = walkable.filter: p =>
  p.adjacent.count(walkable) > 2
.toSet + start + end

def nodesFrom(pos: Point) = List.from[(Point, Int)]:
  def next(pos: Point, dir: Dir): List[(Point, Dir)] =
    for
      d <- List(dir, dir.turnRight, dir.turnLeft)
      p = pos.move(d)
      if walkable(p) && slopes.get(p).forall(_ == d)
    yield p -> d

  def search(p: Point, d: Dir, dist: Int): Option[(Point, Int)] =
    next(p, d) match
      case (p, d) :: Nil if nodes(p) => Some(p, dist + 1)
      case (p, d) :: Nil => search(p, d, dist + 1)
      case _ => None

  Dir.values.flatMap(next(pos, _)).distinct.flatMap(search(_, _, 1))

def longestDownhillHike(pos: Point, dist: Int): Int =
  if pos == end then dist else
    nodesFrom(pos).foldLeft(0):
      case (max, (n, d)) => max.max(longestDownhillHike(n, dist + d))

val ans1 = longestDownhillHike(start, 0)

type Node = Int
val indexOf: Map[Point, Node] =
  nodes.toList.sortBy(_.dist(start)).zipWithIndex.toMap

val fullAdj: Map[Node, List[(Node, Int)]] =
  nodes.toList.flatMap: p1 =>
    nodesFrom(p1).flatMap: (p2, d) =>
      val forward = indexOf(p1) -> (indexOf(p2), d)
      val reverse = indexOf(p2) -> (indexOf(p1), d)
      List(forward, reverse)
  .groupMap(_._1)(_._2)

def longestHike(node: Node, visited: BitSet, dist: Int): Int =
  if node == indexOf(end) then dist else
    fullAdj(node).collect:
      case (n, d) if !visited(n) => longestHike(n, visited + n, dist + d)
    .maxOption.getOrElse(0)

val ans2 = longestHike(indexOf(start), BitSet.empty, 0)
