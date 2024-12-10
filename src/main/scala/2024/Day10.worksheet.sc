val topoMap = io.Source.fromResource("2024/day-10.txt").getLines.toVector

import aoc.{Area, Point}

val area = Area(topoMap)

val trailHeads = area.pointsIterator
  .filter(topoMap(_) == '0')
  .toList

def height(p: Point): Int =
  val c = topoMap(p)
  if c.isDigit then c.asDigit else -1

def climb(p: Point): List[Point] =
  val h = height(p)
  p.adjacent.filter(area.contains).filter(h + 1 == height(_))
    .toList

def score1(p: Point): Int =
  val h = height(p)
  if h == 9 then 1 else
    climb(p).map(score1).sum

def peaks(p: Point): Set[Point] =
  val h = height(p)
  println(" " * h + p.toString + s" $h")
  if h == 9 then Set(p) else
    climb(p).map(peaks).foldLeft(Set.empty[Point])(_ union _)

def score2(p: Point): Int = peaks(p).size

val ans1 = trailHeads.map(score1).sum
val ans2 = trailHeads.map(score2).sum
