
val input = io.Source.fromResource("2022/day-14.txt").getLines().toList

case class Point(x: Int, y: Int):
  def d = copy(y = y + 1)
  def dl = copy(x = x - 1, y = y + 1)
  def dr = copy(x = x + 1, y = y + 1)

val paths = input.map { line =>
  line.split(" -> ").map {
    case s"$x,$y" => Point(x.toInt, y.toInt)
  }.toList
}

def pathPoints(path: List[Point]): Set[Point] =
  path.sliding(2).flatMap {
    case List(p1, p2) =>
      val vert = p1.x == p2.x
      val horz = p1.y == p2.y
      val dx = p2.x - p1.x
      val dy = p2.y - p1.y

      if dx == 0 then (p1.y to p2.y by dy.sign).map(Point(p1.x, _))
      else if dy == 0 then (p1.x to p2.x by dx.sign).map(Point(_, p1.y))
      else ???
    case _ => ???
  }.toSet

val grid = paths.flatMap(pathPoints).toSet

type Grid = Set[Point]

val source: Point = Point(500, 0)
val abyss: Int = grid.map(_.y).max

def fall(grid: Grid): Option[(Point, Grid)] =
  val resting: Point = LazyList.unfold(source) { pos =>
    val blocked = grid(pos.d) && grid(pos.dl) && grid(pos.dr)
    Option.when(pos.y < abyss && !blocked) {
      if !grid(pos.d) then pos.d
      else if !grid(pos.dl) then pos.dl
      else if !grid(pos.dr) then pos.dr
      else ??? // dead code should be handled by block bool
    }.map(p => p -> p)
  }.last

  Option.unless(resting.y == abyss){
    resting -> grid.incl(resting)
  }

Iterator.unfold(grid)(g => fall(g)).size

val floor = abyss + 2

// Iterator.unfold[Int, Int](3)(_ => Option.empty[(Int, Int)]).next()

def fall2(grid: Grid): Option[(Point, Grid)] =
  val resting: Option[Point] = LazyList.unfold(source) { pos =>
    val blocked = grid(pos.d) && grid(pos.dl) && grid(pos.dr)
    Option.when(pos.y < floor - 1 && !blocked) {
      if !grid(pos.d) then pos.d
      else if !grid(pos.dl) then pos.dl
      else if !grid(pos.dr) then pos.dr
      else ??? // dead code should be handled by block bool
    }.map(p => p -> p)
  }.lastOption

  resting.map(r => r -> grid.incl(r))

Iterator.unfold(grid)(g => fall2(g)).size + 1

//
