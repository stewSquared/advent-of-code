val grid = io.Source.fromResource("2024/day-08.txt").getLines.toVector

import aoc.{Area, Point}

val bounds = Area(grid)

val antennaGroups: Map[Char, List[Point]] =
  bounds.pointsIterator
    .collect:
      case p if grid(p) != '.' => grid(p) -> p
    .toList
    .groupMap(_._1)(_._2)

def antinodes(ps: List[Point]): Set[Point] =
  ps.combinations(2)
    .flatMap:
      case List(p1, p2) =>
        val d = p2 - p1
        List(p2 + d, p1 - d)
    .toSet

val ans1 = antennaGroups.values
  .map(antinodes).reduce(_ union _)
  .filter(bounds.contains)
  .size

def gcf(a: Int, b: Int): Int = // TODO add to math library
  if b == 0 then a else gcf(b, a % b)

def antinodes2(ps: List[Point]): Set[Point] =
  ps.combinations(2)
    .flatMap:
      case List(p1, p2) =>
        val d = p2 - p1
        val e =
          val f = gcf(d.x, d.y)
          Point(d.x / f, d.y / f)

        val as1 = Iterator
          .iterate(p1)(_ + e)
          .takeWhile(bounds.contains)

        val as2 = Iterator
          .iterate(p1)(_ - e)
          .takeWhile(bounds.contains)

        (as1 ++ as2)
    .toSet

val ans2 = antennaGroups
  .values.map(antinodes2)
  .reduce(_ union _)
  .size
