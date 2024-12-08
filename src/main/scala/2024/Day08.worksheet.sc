val grid = io.Source.fromResource("2024/day-08.txt").getLines.toVector

import aoc.{Area, Point}

val bounds = Area(grid)

val antennas: Map[Point, Char] =
  bounds.pointsIterator
    .collect:
      case p if grid(p) != '.' => p -> grid(p)
    .toMap

val antennaGroups: Map[Char, List[Point]] =
  antennas.toList.groupMap((p, c) => c)((p, c) => p)

def antinodes(ps: List[Point]): List[Point] =
  ps.combinations(2)
    .flatMap:
      case List(p1, p2) =>
        val d = p2 - p1
        List(p2 + d, p1 - d)
    .toList
    .distinct

val ans1 = antennaGroups.values
  .flatMap(antinodes)
  .filter(bounds.contains)
  .toList
  .distinct.size

def gcf(a: Int, b: Int): Int =
  if b == 0 then a else gcf(b, a % b)

def antinodes2(ps: List[Point]): List[Point] =
  // TODO check 1 antenna
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
          .toList

        val as2 = Iterator
          .iterate(p2)(_ - e)
          .takeWhile(bounds.contains)
          .toList

        as1 ++ as2
    .toList
    .distinct

val ans2 = antennaGroups.values
  .flatMap(antinodes2)
  // .filter(bounds.contains)
  .toList
  .distinct.size
