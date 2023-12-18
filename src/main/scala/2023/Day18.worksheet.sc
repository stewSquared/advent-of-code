import aoc.*

val input = io.Source.fromResource("2023/day-18.txt").getLines.toList

def toCardinal(dir: String): Dir = dir match
  case "U" => Dir.N
  case "D" => Dir.S
  case "R" => Dir.E
  case "L" => Dir.W

type Color = String

def step(pos: Point, plan: Map[Point, Color], instruction: String) =
  instruction match
    case s"$rawDir $n (#$rawHex)" =>
      val dir = toCardinal(rawDir)
      val newPos = pos.move(dir, n.toInt)
      val updatedPlan = Line(pos.move(dir), newPos).points.foldLeft(plan):
        case (plan, p) => plan.updated(p, rawHex)
      newPos -> updatedPlan

val (_, plan) = input.foldLeft(Point.origin -> Map.empty[Point, Color]):
  case ((pos, plan), instruction) => step(pos, plan, instruction)

val boundaryPoints = plan.keySet

val insidePoint = Point(1, 1)

val flood = Iterator.unfold(Set.empty[Point], Set(insidePoint)):
  case (visited, current) =>
    Option.when(current.nonEmpty):
      val next = current.flatMap(area.adjacent).diff(boundaryPoints).diff(visited)
      (current, current -> next)

val area = Area.bounding(plan)
val inner = flood.reduce(_ union _)
val ans1 = inner.size + boundaryPoints.size

inner.size
boundaryPoints.size

area.size
area.width
area.height

println:
  area.draw: p =>
    if inner.contains(p) then 'X'
    else if plan.contains(p) then '#'
    else '.'



//
