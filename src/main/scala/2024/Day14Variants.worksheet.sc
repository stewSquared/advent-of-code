val input = io.Source.fromResource("2024/day-14.txt").getLines.toList

import aoc.{Point, Area}

val area = Area(0 until 101, 0 until 103)

def wrap(p: Point) =
  val x = (p.x + area.width) % area.width
  val y = (p.y + area.height) % area.height
  Point(x, y)

// todo test negative

case class Robot(p: Point, v: Point):
  def move(n: Int): Robot = copy(p = wrap(p + v*n))

val robots = input.collect:
  case s"p=$px,$py v=$vx,$vy" =>
    Robot(Point(px.toInt, py.toInt), Point(vx.toInt, vy.toInt))

val finalPositions = robots.map(_.move(100)).map(_.p)

val quadrants =
  val midX = area.width / 2
  val midY = area.height / 2
  List(
    Area(0 until midX, 0 until midY),
    Area((midX + 1) until 101, 0 until midY),
    Area(0 until midX, (midY + 1) until 103),
    Area((midX + 1) until 101, (midY + 1) until 103))

val ans1 = quadrants
  .map(q => finalPositions.count(q.contains))
  .map(_.toLong)
  .product

def showImage(state: List[Robot]): Unit =
  val ps = state.map(_.p).toSet
  println:
    area.draw: p =>
      if ps(p) then '#' else '.'

// correct vertical state found visually
// could find programmatically using density heuristic
val state65 = robots.map(_.move(65))

def possibleSolutions = Iterator
  .iterate(state65)(_.map(_.move(area.height)))
  .take(area.width)

// possibleSolutions.zipWithIndex.foreach: (s, i) =>
//   println(s"seconds: ${65 + area.height * i}")
//   showImage(s)
//   println()

showImage(robots.map(_.move(7687)))

// Approaches:
// find border - requires guessing that it has a border
// find solid area - requires guessing
// seek state with many connected components - works with holes or off center
// symmetry -- image technically symmetric, but not centered, requires guessing
//
