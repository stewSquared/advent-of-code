val input = io.Source.fromResource("2024/day-18.txt").getLines.toList

import aoc.{Area, Point}

val area = Area(0 to 70, 0 to 70)

val bytes: List[Point] = input.collect:
  case s"$x,$y" => Point(x.toInt, y.toInt)

val grid1024: Set[Point] = bytes.take(1024).toSet

val start = area.topLeft
val end = area.botRight

def walk(p: Point, blocked: Set[Point]): Set[Point] =
  p.adjacent
    .diff(blocked)
    .filter(_.inBounds(area))

def search(blocked: Set[Point]): Int =
  import collection.mutable.{PriorityQueue, Map}

  val cost = Map[Point, Int](start -> 0)
  val pq = PriorityQueue.empty[Point](Ordering.by(cost)).reverse

  var visiting = start
  while visiting != end do
    walk(visiting, blocked)
      .filterNot(cost.contains)
      .foreach: n =>
        cost(n) = cost(visiting) + 1
        pq.enqueue(n)

    visiting = pq.dequeue()

  cost(visiting)

val ans1 = search(grid1024)

def floodFill(blocked: Set[Point]): Set[Point] =
  val steps = Iterator.unfold(Set.empty[Point] -> Set(start)):
    case (prev, curr) =>
      Option.when(curr.nonEmpty):
        val next = curr.flatMap(walk(_, blocked)).diff(prev)
        curr -> (curr -> next)

  steps.reduce(_ union _)

def reachable(n: Int): Boolean =
  val blocked = bytes.take(n + 1).toSet
  floodFill(blocked).contains(end)

def bs(range: Range): Point =
  if range.size == 1 then bytes(range.start) else
    val (left, right) = range.splitAt(range.size / 2)
    if reachable(left.last) then bs(right) else bs(left)

val ans2 = bs(bytes.indices.drop(1024))
