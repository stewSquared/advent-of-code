val input = io.Source.fromResource("2015/day-06.txt").getLines().toList

import aoc.{Point, Area}

val grid = Area(0 to 999, 0 to 999)

enum Instruction:
  case On(a: Area)
  case Off(a: Area)
  case Toggle(a: Area)

import Instruction.{On, Off, Toggle}

val instructions = input.map:
  case s"turn on $x1,$y1 through $x2,$y2" => On:
    Area.bounding(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
  case s"turn off $x1,$y1 through $x2,$y2" => Off:
    Area.bounding(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
  case s"toggle $x1,$y1 through $x2,$y2" => Toggle:
    Area.bounding(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))

val ans1 = grid.pointsIterator.map: p =>
  instructions.foldLeft(false):
    case (b, On(a)) if a.contains(p) => true
    case (b, Off(a)) if a.contains(p) => false
    case (b, Toggle(a)) if a.contains(p) => !b
    case (b, _) => b
.count(identity)

val ans2 = grid.pointsIterator.map: p =>
  instructions.foldLeft(0):
    case (b, On(a)) if a.contains(p) => b + 1
    case (b, Off(a)) if a.contains(p) && b > 0 => b - 1
    case (b, Toggle(a)) if a.contains(p) => b + 2
    case (b, _) => b
.sum
