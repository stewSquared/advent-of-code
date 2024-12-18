val input = io.Source.fromResource("2024/day-18.txt").getLines.toList

import aoc.{Area, Point}

val area = Area(0 to 70, 0 to 70)

val bytes: List[Point] = input.collect:
  case s"$x,$y" => Point(x.toInt, y.toInt)

val grid1024: Set[Point] = bytes.take(1024).toSet

val start = Point(0, 0)
val end = area.botRight

def next(p: Point, blocked: Set[Point]): Set[Point] =
  p.adjacent
    .diff(blocked)
    .filter(_.inBounds(area))

def search(blocked: Set[Point]): Int =
  import collection.mutable.{PriorityQueue, Map}

  val cost = Map[Point, Int](start -> 0)
  val pq = PriorityQueue.empty[Point](Ordering.by(cost)).reverse

  var visiting = start
  while visiting != end do
    next(visiting, blocked)
      .filterNot(cost.contains)
      .foreach: n =>
        cost(n) = cost(visiting) + 1
        pq.enqueue(n)

    visiting = pq.dequeue()

  cost(visiting)

val ans1 = search(grid1024)

def floodFill(blocked: Set[Point]): Set[Point] =
  val steps = Iterator.unfold(Set.empty[Point] -> Set(start)):
    case (visited, visiting) =>
      Option.when(visiting.nonEmpty):
        val visitingNext = visiting.flatMap(next(_, blocked)).diff(visited)

        visiting -> (visiting -> visitingNext)

  steps.reduce(_ union _)

def reachable(n: Int): Boolean =
  val blocked = bytes.take(n + 1).toSet
  floodFill(blocked).contains(end)

val (a, b) = (0 to 0).splitAt(1/2)

def bs(range: Range): Point =
  if range.size == 1 then
    assert(!reachable(range.start))
    bytes(range.start)
  else
    val (left, right) = range.splitAt(range.size / 2)
    if reachable(left.last) then bs(right) else bs(left)

val ans2 = bs(bytes.indices)
