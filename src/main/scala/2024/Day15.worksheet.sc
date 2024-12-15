val input = io.Source.fromResource("2024/day-15.txt").getLines.toList

val grid = input.takeWhile(_.nonEmpty).toVector

import aoc.{Point, Area, Dir}

val area = Area(grid)

val start = area.pointsIterator.find(grid(_) == '@').get
val boxes = area.pointsIterator.filter(grid(_) == 'O').toSet
val walls = area.pointsIterator.filter(grid(_) == '#').toSet

def farSideBoxes(pos: Point, dir: Dir, boxes: Set[Point]): Point =
  Iterator
    .iterate(pos)(_.move(dir))
    .drop(1)
    .dropWhile(boxes)
    .next

val movements: List[Dir] = input.drop(area.height + 1).flatMap: line =>
  line.map:
    case '^' => Dir.N
    case 'v' => Dir.S
    case '<' => Dir.W
    case '>' => Dir.E

val (finalPos, finalBoxes) = movements.foldLeft((start, boxes)):
  case ((pos, boxes), dir) =>
    val nextPos = pos.move(dir)
    if !(boxes(nextPos) || walls(nextPos)) then
      nextPos -> boxes
    else if walls(nextPos) then
      pos -> boxes
    else // is box
      val farSide = farSideBoxes(nextPos, dir, boxes)
      if walls(farSide) then
        pos -> boxes
      else
        nextPos -> ((boxes - nextPos) + farSide)

val ans1 = finalBoxes.toList.map:
  case Point(x, y) => 100 * y + x
.sum

val start2 = start.copy(x = start.x * 2)
val boxes2 = finalBoxes.map(b => b.copy(x = b.x * 2))
val walls2 = walls.map(b => b.copy(x = b.x * 2))

def occupied2(pos: Point, boxes2: Set[Point]): Boolean =
  val left = pos.move(Dir.W)
  boxes2(pos) || walls2(pos) || boxes2(left) || walls2(left)

def connectedBoxes(pos: Point, dir: Dir, boxes2: Set[Point]): Set[Point] =
  assert(dir.isVertical)
  def search(p: Point): Set[Point] =
    val n = p.move(dir)
    val nw = n.move(Dir.W)
    val ne = n.move(Dir.E)
    if boxes2(n) then
      search(n) union search(ne) + n
    else if boxes2(nw) then
      search(nw) union search(n) + nw
    else Set.empty[Point]
  search(pos)


val states = movements.scanLeft((start2, boxes2)):
  case ((pos, bs), dir) =>
    // assert(!(bs(pos) || walls2(pos)))
    val nextPos = pos.move(dir)
    if !occupied2(nextPos, bs) then
      nextPos -> bs
    else if walls2(nextPos) || walls2(nextPos.move(Dir.W)) then
      pos -> bs
    else if dir == Dir.N || dir == Dir.S then
      val connected = connectedBoxes(nextPos, dir, bs)
      val noWalls = connected.forall: b =>
        val n = b.move(dir)
        val nw = n.move(Dir.W)
        walls2(n) || walls2(nw)

      if noWalls then
        nextPos -> (boxes2 -- connected ++ connected.map(_.move(dir)))
      else
        pos -> boxes
    else
      val farSide = farSideBoxes2(nextPos, dir, bs)
      if walls2(farSide) then
        pos -> bs
      else
        val boxesToMove = horizontalBoxes(nextPos, dir, bs)
        nextPos -> ((bs -- boxesToMove) ++ boxes.map(_.move(dir)))

val (finalPos2, finalBoxes2) = states.last

def horizontalBoxes(pos: Point, dir: Dir, boxes: Set[Point]): Set[Point] =
  assert(dir.isHorizontal)
  val start = if dir == Dir.E then pos.move(Dir.W) else pos
  Iterator
    .iterate(start)(_.move(dir, 2))
    .drop(1)
    .takeWhile(boxes)
    .toSet

locally:
  val boxes = Set(Point(0, 0))


def farSideBoxes2(pos: Point, dir: Dir, boxes: Set[Point]): Point =
  assert(dir.isHorizontal)
  val start = if dir == Dir.E then pos.move(Dir.W) else pos
  Iterator
    .iterate(start)(_.move(dir, 2))
    .drop(1)
    .dropWhile(boxes)
    .next

val ans2 = finalBoxes2.toList.map:
  case Point(x, y) => 100 * y + x
.sum

states.take(10).foreach: (pos, boxes) =>
  println:
    area.draw: p =>
      if boxes(p) then '['
      else if boxes(p.move(Dir.W)) then ']'
      else if walls2(p) then '#'
      else if walls2(p.move(Dir.W)) then '#'
      else if pos == p then '@'
      else '.'


// 1507629 too low
// 1509774 too low

//
