import aoc.*

val input = io.Source.fromResource("2023/day-10.txt").getLines().toVector

val area = Area(input)

val start = area.pointsIterator.find(input(_) == 'S').get

// NOTE: alternatively tuples/sets of directions
enum Tile:
  case TopBot, TopLeft, TopRight, BotLeft, BotRight, LeftRight, Empty

  def topOpen: Boolean = this match
    case TopBot | TopLeft | TopRight => true
    case _ => false

  def botOpen: Boolean = this match
    case TopBot | BotLeft | BotRight => true
    case _ => false

  def leftOpen: Boolean = this match
    case TopLeft | BotLeft | LeftRight => true
    case _ => false

  def rightOpen: Boolean = this match
    case TopRight | BotRight | LeftRight => true
    case _ => false

  def show: Char = this match
    case TopBot => '║'
    case TopLeft => '╝'
    case TopRight => '╚'
    case BotLeft => '╗'
    case BotRight => '╔'
    case LeftRight => '═'
    case Empty => '.'

object Tile:
  def fromChar(c: Char) = c match
    case '.' => Empty
    case '7' => BotLeft
    case 'J' => TopLeft
    case 'L' => TopRight
    case 'F' => BotRight
    case '-' => LeftRight
    case '|' => TopBot
    case 'S' => startTileType
    case _ => ???

import Tile.*

val startTileType: Tile =
  val n = Tile.fromChar(input(start.n))
  val s = Tile.fromChar(input(start.s))
  val e = Tile.fromChar(input(start.e))
  val w = Tile.fromChar(input(start.w))
  if n.botOpen && s.topOpen then TopBot
  else if n.botOpen && w.rightOpen then TopLeft
  else if n.botOpen && e.leftOpen then TopRight
  else if s.topOpen && w.rightOpen then BotLeft
  else if s.topOpen && e.leftOpen then BotRight
  else if w.rightOpen && e.leftOpen then LeftRight
  else ???

val startDir: Dir =
  if startTileType.topOpen then Dir.N
  else if startTileType.botOpen then Dir.S
  else if startTileType.rightOpen then Dir.E
  else Dir.W

val grid: Map[Point, Tile] = Map.from[Point, Tile]:
  area.pointsIterator.map(p => p -> Tile.fromChar(input(p)))

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
  if destTile.topOpen && dir != Dir.S then Dir.N
  else if destTile.botOpen && dir != Dir.N then Dir.S
  else if destTile.leftOpen && dir != Dir.E then Dir.W
  else if destTile.rightOpen && dir != Dir.W then Dir.E
  else throw new Exception(s"no next dir for $pos $dir")

def walk = Iterator.unfold[(List[Point], List[Point]), (Point, Dir)]((start, Dir.N)):
  case (pos, dir) =>
    val nextPos = pos.move(dir)
    val left = pos.move(dir.turnLeft)
    val right = pos.move(dir.turnRight)
    Option.when(nextPos != start):
      val leftSpace =
        List(left, left.move(dir))
          .filter(_.inBounds(area))
          .filterNot(boundaryPoints)
      val rightSpace =
        List(right, right.move(dir))
          .filter(_.inBounds(area))
          .filterNot(boundaryPoints)
      val newState = (nextPos, nextDir(pos, dir))
      (leftSpace, rightSpace) -> newState

val (left, right) = walk.toList.unzip

val leftSeeds = left.flatten.toSet
val rightSeeds = right.flatten.toSet

assert(leftSeeds.intersect(rightSeeds).isEmpty)

def flood(seeds: Set[Point]): Set[Point] =
  val points = Iterator.unfold(Set.empty[Point] -> seeds):
    case (visited, current) =>
      Option.when(current.nonEmpty):
        val next = current.flatMap(area.adjacent).diff(boundaryPoints).diff(visited)
        (current, current -> next)
  points.reduce(_ union _)

val leftFlood = flood(leftSeeds)
val rightFlood = flood(rightSeeds)

assert(area.size == rightFlood.size + leftFlood.size + boundaryPoints.size)

val (inside, outside) =
  if leftFlood.contains(Point.origin) then (rightFlood, leftFlood)
  else (leftFlood, rightFlood)

assert(inside.intersect(outside).isEmpty)

val unicodeDarkShade = '\u2593'
val unicodeLightShade = '\u2591'
val unicodeBlock = '\u2588'

println:
  area.draw: p =>
    if p == start then unicodeBlock
    else if inside(p) then unicodeDarkShade
    else if outside(p) then unicodeLightShade
    else if boundaryPoints.contains(p) then grid(p).show
    else '?'

val ans2 = inside.size
