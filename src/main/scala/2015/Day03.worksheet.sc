val input = io.Source.fromResource("2015/day-03.txt").getLines().next()

import aoc.Point

def move(p: Point, c: Char): Point = (p, c) match
  case (p, '^') => p.n
  case (p, 'v') => p.s
  case (p, '<') => p.w
  case (p, '>') => p.e
// TODO add char movement to the library

val path = input.scanLeft(Point.origin)(move)

val ans1 = path.distinct.size

val santaPath = input.sliding(1, 2).flatten
  .scanLeft(Point.origin)(move)

val roboPath = input.tail.sliding(1, 2).flatten
  .scanLeft(Point.origin)(move)

val ans2 = (santaPath.toSet union roboPath.toSet).size

//
