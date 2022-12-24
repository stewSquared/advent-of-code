val input = io.Source.fromResource("2022/day-24.txt").getLines().toVector

val inBounds: Set[Point] =
  val points = for
    y <- input.indices
    x <- input.head.indices
    c = input(y)(x)
    if c != '#'
  yield Point(x, y)
  points.toSet

case class Point(x: Int, y: Int):
  def n = copy(y = y - 1)
  def s = copy(y = y + 1)
  def w = copy(x = x - 1)
  def e = copy(x = x + 1)
  def adj = List(n,s,e,w).filter(inBounds)
  def distTo(that: Point): Int =
    val dx = that.x - this.x
    val dy = that.y - this.y
    dx.abs + dy.abs

enum Bliz:
  case N, S, E, W

  def move(from: Point): Point =
    val target = this match
      case Bliz.N => from.n
      case Bliz.S => from.s
      case Bliz.E => from.e
      case Bliz.W => from.w
    if inBounds(target) then target else
      this match
        case Bliz.N => target.copy(y = Bliz.yRange.max)
        case Bliz.S => target.copy(y = Bliz.yRange.min)
        case Bliz.E => target.copy(x = Bliz.xRange.min)
        case Bliz.W => target.copy(x = Bliz.xRange.max)

object Bliz:
  val xRange = inBounds.map(_.x).min to inBounds.map(_.x).max
  val yRange = (inBounds.map(_.y).min + 1) to (inBounds.map(_.y).max - 1)

  def fromChar(c: Char): Bliz = c match
    case '^' => Bliz.N
    case 'v' => Bliz.S
    case '>' => Bliz.E
    case '<' => Bliz.W

type Grid = Map[Point, List[Bliz]] // potentially more efficent rep

val grid0: Grid =
  val points = for
    y <- input.indices
    x <- input.head.indices
    c = input(y)(x)
    if c != '#'
    p = Point(x, y)
  yield
    val space = if c == '.' then List.empty[Bliz] else List(Bliz.fromChar(c))
    p -> space
  points.toMap

def step(grid: Grid): Grid =
  grid.toList.flatMap {
    case (p, bs) => bs.map(b => b.move(p) -> b)
  }.groupMap(_._1)(_._2)

val grids = LazyList.iterate(grid0)(step)

val start = grid0.keys.minBy(_.y)
val end = grid0.keys.maxBy(_.y)

case class State(pos: Point, time: Int)

def search(start: Point, end: Point, time: Int): Int =
  import collection.mutable.{PriorityQueue, Set}
  given Ordering[State] = Ordering.by(s => s.time + s.pos.distTo(end))
  val pq = PriorityQueue.empty[State].reverse
  var visiting = State(start, time)
  val visited = Set(visiting)

  while visiting.pos != end do
    val nextStates = (visiting.pos :: visiting.pos.adj)
      .filterNot(grids(visiting.time + 1).contains)
      .map(State(_, visiting.time + 1))
      .filterNot(visited)
    pq.enqueue(nextStates*)
    visited ++= nextStates
    visiting = pq.dequeue()

  visiting.time

val ans1 = search(start, end, 0)

val t2 = search(end, start, ans1)
val ans2 = search(start, end, t2)
