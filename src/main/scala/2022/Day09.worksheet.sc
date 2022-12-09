val input = io.Source.fromResource("2022/day-09.txt")
  .getLines()
  .toList

case class Point(x: Int, y: Int):
  def u = copy(y = y + 1)
  def d = copy(y = y - 1)
  def r = copy(x = x + 1)
  def l = copy(x = x - 1)

object Point:
  val origin = Point(0, 0)

import Point.origin

case class Rope(h: Point, t: Point):
  def fixTail: Rope =
    val dx = (t.x - h.x).abs
    val dy = (t.y - h.y).abs
    (dx, dy) match
      case (dx, dy) if dx <= 1 && dy <= 1 => this
      case (2, 0) => copy(t = t.copy(x = (h.x + t.x) / 2))
      case (0, 2) => copy(t = t.copy(y = (h.y + t.y) / 2))
      case (2, 2) => copy(t = t.copy(x = (h.x + t.x) / 2, y = (h.y + t.y) / 2))
      case (1, 2) => copy(t = t.copy(x = t.x + (h.x - t.x).sign)).fixTail
      case (2, 1) => copy(t = t.copy(y = t.y + (h.y - t.y).sign)).fixTail

  def u = copy(h.u).fixTail
  def d = copy(h.d).fixTail
  def r = copy(h.r).fixTail
  def l = copy(h.l).fixTail

val ropeStart = Rope(origin, origin)

val ropePositions = input.scanLeft(List(ropeStart)) {
  case (pos, movement) =>
    movement match
      case s"U $n" => List.iterate(pos.last, n.toInt + 1)(_.u).tail
      case s"D $n" => List.iterate(pos.last, n.toInt + 1)(_.d).tail
      case s"R $n" => List.iterate(pos.last, n.toInt + 1)(_.r).tail
      case s"L $n" => List.iterate(pos.last, n.toInt + 1)(_.l).tail
}

val ropeStart2 = List.fill(10)(Point(0,0))

def gap(p: Point, q: Point): Boolean =
  val dx = p.x - q.x
  val dy = p.y - q.y
  dx.abs > 1 || dy.abs > 1

def chase(h: Point, t: Point): Point => Point =
  val dx = h.x - t.x
  val dy = h.y - t.y
  if !gap(h, t) then identity else
    val yMove: Point => Point = dy.sign match
      case 1 => _.u
      case 0 => identity
      case -1 => _.d
    val xMove: Point => Point = dx.sign match
      case 1 => _.r
      case 0 => identity
      case -1 => _.l
    yMove andThen xMove

extension (rope: List[Point])
  def move(d: Point => Point): List[Point] = rope match
    case Nil => Nil
    case h :: Nil => d(h) :: Nil
    case h :: n :: tail =>
      val newHead = d(h)
      newHead :: (n :: tail).move(chase(newHead, n))

val ropePositions2 = input.scanLeft(List(ropeStart2)) {
  case (pos, movement) =>
    movement match
      case s"U $n" => List.iterate(pos.last, n.toInt + 1)(_.move(_.u)).tail
      case s"D $n" => List.iterate(pos.last, n.toInt + 1)(_.move(_.d)).tail
      case s"R $n" => List.iterate(pos.last, n.toInt + 1)(_.move(_.r)).tail
      case s"L $n" => List.iterate(pos.last, n.toInt + 1)(_.move(_.l)).tail
}

ropePositions2.flatten.map {
  rope => rope.map(p => s"(${p.x},${p.y})").mkString
} foreach println

//  ropePositions2.map(_.last).toSet.size // 1947 too low bug

val ans2 = ropePositions2.flatten.map(_.last).toSet.size
// need 36 for example

ropePositions.flatten foreach println

val ans1 = ropePositions.flatten.map(_.t).toSet.size

val headPositions = input.scanLeft(Point(0,0)) {
  case (pos, movement) =>
    movement match
      case s"U $n" => List.iterate(pos, n.toInt + 1)(_.u).last
      case s"D $n" => List.iterate(pos, n.toInt + 1)(_.d).last
      case s"R $n" => List.iterate(pos, n.toInt + 1)(_.r).last
      case s"L $n" => List.iterate(pos, n.toInt + 1)(_.l).last
}

headPositions foreach println

//
