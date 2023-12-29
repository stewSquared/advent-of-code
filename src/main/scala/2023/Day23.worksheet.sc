import aoc.*

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

def maxPathFrom(pos: Point, visited: Set[Point]): Int =
  if pos == end then visited.size else
    val next = for
      dir <- Dir.values
      newPos = pos.move(dir)
      if slopes.get(newPos).forall(_ == dir)
      if walkable(newPos) && !visited(newPos)
    yield newPos
    next.foldLeft(0)(_ max maxPathFrom(_, visited + pos))

val ans1 = maxPathFrom(start, Set.empty)
