import aoc.*

val input = io.Source.fromResource("2023/day-10.txt").getLines().toVector

val area = Area(input)

val start = area.pointsIterator.find(input(_) == 'S').get

// NOTE: alternatively tuples/sets of directions
enum Tile:
  case TopBot, TopLeft, TopRight, BotLeft, BotRight, LeftRight, Empty, Start

  def topOpen: Boolean = this match
    case TopBot | TopLeft | TopRight | Start => true
    case _ => false

  def botOpen: Boolean = this match
    case TopBot | BotLeft | BotRight => true
    case _ => false

  def leftOpen: Boolean = this match
    case TopLeft | BotLeft | LeftRight | Start => true
    case _ => false

  def rightOpen: Boolean = this match
    case TopRight | BotRight | LeftRight => true
    case _ => false

  def toChar: Char = this match
    case TopBot => '|'
    case TopLeft => 'J'
    case TopRight => 'L'
    case BotLeft => '7'
    case BotRight => 'F'
    case LeftRight => '-'
    case Empty => '.'
    case Start => 'S'

import Tile.*

val grid: Map[Point, Tile] = Map.from[Point, Tile]:
  for
    (row, y) <- input.zipWithIndex
    (char, x) <- row.zipWithIndex
  yield
    val tile = char match
      case '.' => Empty
      case '7' => BotLeft
      case 'J' => TopLeft
      case 'L' => TopRight
      case 'F' => BotRight
      case '-' => LeftRight
      case '|' => TopBot
      case 'S' => Start
      case _ => ???
    Point(x, y) -> tile

def adjacent(p: Point): Set[Point] =
  val n = Option.when(p.n.inBounds(area) && grid(p).topOpen && grid(p.n).botOpen)(p.n)
  val s = Option.when(p.s.inBounds(area) && grid(p).botOpen && grid(p.s).topOpen)(p.s)
  val e = Option.when(p.e.inBounds(area) && grid(p).rightOpen && grid(p.e).leftOpen)(p.e)
  val w = Option.when(p.w.inBounds(area) && grid(p).leftOpen && grid(p.w).rightOpen)(p.w)
  List(n, s, e, w).flatten.toSet

val loopSteps: List[Set[Point]] =
  val steps = Iterator.unfold(Set.empty[Point] -> Set(start)):
    case (visited, current) =>
      Option.when(current.nonEmpty):
        val next = current.flatMap(adjacent).diff(visited)
        (current, current -> next)
  steps.toList

val ans1 = loopSteps.length - 1

val boundaryPoints = loopSteps.reduce(_ union _)

def nextDir(pos: Point, dir: Dir) =
  val destTile = grid(pos.move(dir))
  (dir, destTile) match
    case (Dir.N, TopBot) => Dir.N
    case (Dir.N, BotLeft) => Dir.W
    case (Dir.N, BotRight) => Dir.E
    case (Dir.S, TopBot) => Dir.S
    case (Dir.S, TopLeft) => Dir.W
    case (Dir.S, TopRight) => Dir.E
    case (Dir.E, LeftRight) => Dir.E
    case (Dir.E, BotLeft) => Dir.S
    case (Dir.E, TopLeft) => Dir.N
    case (Dir.W, LeftRight) => Dir.W
    case (Dir.W, TopRight) => Dir.N
    case (Dir.W, BotRight) => Dir.S
    case _ => ???

def walk = Iterator.unfold[(List[Point], List[Point]), (Point, Dir)]((start, Dir.N)):
  case (pos, dir) =>
    val next = pos.move(dir)
    val left = pos.move(dir.turnLeft)
    val right = pos.move(dir.turnRight)
    Option.when(next != start):
      val leftSpace =
        List(left, left.move(dir))
          .filter(_.inBounds(area))
          .filterNot(boundaryPoints)
      val rightSpace =
        List(right, right.move(dir))
          .filter(_.inBounds(area))
          .filterNot(boundaryPoints)
      val newState = (next, nextDir(pos, dir))
      (leftSpace, rightSpace) -> newState


val (left, right) = walk.toList.unzip

val leftSeeds = left.flatten.toSet
val rightSeeds = right.flatten.toSet

leftSeeds.intersect(rightSeeds).size

def flood(seeds: Set[Point]): Set[Point] =
  val points = Iterator.unfold(Set.empty[Point] -> seeds):
    case (visited, current) =>
      Option.when(current.nonEmpty):
        val next = current.flatMap(area.adjacent).diff(boundaryPoints).diff(visited)
        (current, current -> next)
  points.reduce(_ union _)

val leftFlood = flood(leftSeeds)
val rightFlood = flood(rightSeeds)

leftSeeds.size
rightSeeds.size

leftFlood.size
rightFlood.size
boundaryPoints.size
area.size
rightFlood.size + leftFlood.size + boundaryPoints.size

val unaccounted = area.pointsIterator.toSet.diff(rightFlood).diff(leftFlood).diff(boundaryPoints)
assert(unaccounted.size == 0)

rightFlood.contains(Point.origin)
leftFlood.contains(Point.origin)

val (inside, outside) =
  if leftFlood.contains(Point.origin) then (rightFlood, leftFlood)
  else (leftFlood, rightFlood)

def charToPipes(c: Char): Char =
  c match
    case '.' => '.'
    case '7' => '╗'
    case 'J' => '╝'
    case 'L' => '╚'
    case 'F' => '╔'
    case '-' => '═'
    case '|' => '║'
    case 'S' => 'S'
    case _ => '?'

assert(inside.intersect(outside).isEmpty)

area.draw: p =>
  if inside(p) then 'I'
  else if outside(p) then 'O'
  else if boundaryPoints.contains(p) then charToPipes(input(p))
  else '\u2588'

val ans2 = inside.size
