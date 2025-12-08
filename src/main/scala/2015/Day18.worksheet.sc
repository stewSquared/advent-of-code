val input = io.Source.fromResource("2015/day-18.txt").getLines().toVector

import aoc.{Point, Area}

val area = Area(input)

val init = Set.from[Point]:
  area.pointsIterator.filter: p =>
    input(p) == '#'

def next(state: Set[Point]): Set[Point] = Set.from[Point]:
  area.pointsIterator.filter: p =>
    (state(p), p.surrounding.count(state)) match
      case (true, 2|3) => true
      case (true, _)   => false
      case (false, 3)  => true
      case (false, _)  => false

val states = LazyList.iterate(init)(next)
val ans1 = states(100).size

val corners = Set(area.topLeft, area.topRight, area.botLeft, area.botRight)

def next2(state: Set[Point]): Set[Point] =
  next(state.union(corners)).union(corners)

val states2 = LazyList.iterate(init)(next2)
val ans2 = states2(100).size
