val farm = io.Source.fromResource("2024/day-12.txt").getLines.toVector

import aoc.{Area, Point}

val area = Area(farm)

def floodFill(start: Point): Set[Point] =
  def search(visited: Set[Point], visiting: Set[Point]): Set[Point] =
    if visiting.isEmpty then visited else
      val next = visiting
        .flatMap(_.adjacent)
        .diff(visited)
        .filter(area.contains)
        .filter(farm(_) == farm(start))
      search(visited.union(visiting), next)
  search(Set.empty, Set(start))

val areaPoints = area.pointsIterator.toSet

val allRegions = LazyList.unfold(areaPoints): unfenced =>
  Option.when(unfenced.nonEmpty):
    val r = floodFill(unfenced.head)
    r -> unfenced.diff(r)

def perimeter(region: Set[Point]): Int =
  region.toList.map: p =>
    p.adjacent.count(!region.contains(_))
  .sum

val ans1 = allRegions
  .map(r => r.size.toLong * perimeter(r).toLong).sum

def cornerCount(region: Set[Point]): Int =
  val bound = Area.bounding(region).expand(1)
  val subAreas = bound.pointsIterator.toList.map: p =>
    Area.bounding(p, p.d.r)

  val normalCorners = subAreas.count: a =>
    val c = a.pointsIterator.count(region)
    c == 1 || c == 3

  val diagonalCorners = subAreas.count: a =>
    val c = a.pointsIterator.count(region)
    val inBounds = a.pointsIterator.forall(area.contains)
    c == 2 && inBounds && (farm(a.topLeft) == farm(a.botRight) || farm(a.botLeft) == farm(a.topRight))

  normalCorners + diagonalCorners*2

def price(region: Set[Point]): Long =
  region.size.toLong * cornerCount(region).toLong

val ans2 = allRegions.map(price).sum
