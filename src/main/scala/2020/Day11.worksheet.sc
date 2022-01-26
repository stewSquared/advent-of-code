val input = io.Source.fromResource("2020/day-11.txt").getLines.toList

case class Point(x: Int, y: Int):
  def u = Point(x, y + 1)
  def d = Point(x, y - 1)
  def r = Point(x + 1, y)
  def l = Point(x - 1, y)

type Grid = Map[Point, Char]

val width = input(0).length
val height = input.size

val xRange = 0 until width
val yRange = 0 until height

val start: Grid =
  val points =
    for
      (line, y) <- input.zipWithIndex
      (char, x) <- line.zipWithIndex
    yield Point(x, y) -> char
  points.toMap

def show(grid: Grid): String =
  val sb = collection.mutable.StringBuilder()
  for y <- yRange do
    for x <- xRange do sb += grid(Point(x, y))
    sb += '\n'
  sb.result()

def adjacent(p: Point, g: Grid): List[Point] =
  List(p.u.l, p.u, p.u.r, p.l, p.r, p.d.l, p.d, p.d.r)
    .filter(q => xRange.contains(q.x) && yRange.contains(q.y))

def inBounds(p: Point): Boolean =
  xRange.contains(p.x) && yRange.contains(p.y)

def next1(p: Point, g: Grid): Char = g(p) match
  case 'L' if !adjacent(p, g).map(g).contains('#')       => '#'
  case '#' if adjacent(p, g).map(g).count(_ == '#') >= 4 => 'L'
  case c                                                 => c

def next1(grid: Grid): Grid =
  val nextPoints = for
    y <- yRange
    x <- xRange
    p = Point(x, y)
  yield p -> next1(p, grid)
  nextPoints.toMap

val states = Iterator.iterate(start)(next1)

val ans1 = states
  .sliding(2)
  .collectFirst {
    case Seq(l, r) if l == r => r.values.count(_ == '#')
  }.get

def visibleSeat(origin: Point, dir: Point, grid: Grid): Option[Char] =
  val points = Iterator.iterate(origin) { p =>
    p.copy(x = p.x + dir.x, y = p.y + dir.y)
  }
  points
    .drop(1)
    .takeWhile(inBounds)
    .collectFirst { case p if grid(p) != '.' => grid(p) }

def visibleSeats(p: Point, g: Grid): Seq[Char] =
  for
    dx <- -1 to 1
    dy <- -1 to 1
    dir = Point(dx, dy)
    if dir != Point(0, 0)
    seat <- visibleSeat(p, dir, g)
  yield seat

def next2(grid: Grid): Grid =
  val nextPoints = for
    y <- yRange
    x <- xRange
    p = Point(x, y)
  yield
    val nextSeatState = grid.apply(p) match
      case 'L' if !visibleSeats(p, grid).contains('#')       => '#'
      case '#' if visibleSeats(p, grid).count(_ == '#') >= 5 => 'L'
      case c                                                 => c
    p -> nextSeatState
  nextPoints.toMap

val states2 = Iterator.iterate(start)(next2)

println(show(states2.next()))
val all = states2.next()
println(show(all))

visibleSeats(Point(width - 1, 0), all)

println(show(states2.next()))
println(show(states2.next()))

val ans2 = states2
  .sliding(2)
  .collectFirst {
    case Seq(l, r) if l == r => r.values.count(_ == '#')
  }.get
