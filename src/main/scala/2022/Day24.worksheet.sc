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
  def e = copy(x = x + 1)
  def w = copy(x = x - 1)
  def adj = List(n, s, e, w).filter(inBounds)
  def distTo(that: Point): Int =
    val dx = that.x - this.x
    val dy = that.y - this.y
    dx.abs + dy.abs

val start = inBounds.minBy(_.y)
val end = inBounds.maxBy(_.y)

val valley = inBounds - start - end

val xRange = valley.map(_.x).min to valley.map(_.x).max
val yRange = valley.map(_.y).min to valley.map(_.y).max

case class State(pos: Point, time: Int):
  def nextStates =
    (pos :: pos.adj).map(State(_, time + 1)).filterNot(_.blizzard)

  def blizzard =
    val ny = math.floorMod(pos.y + time - 1, yRange.size) + 1
    val sy = math.floorMod(pos.y - time - 1, yRange.size) + 1
    val ex = math.floorMod(pos.x - time - 1, xRange.size) + 1
    val wx = math.floorMod(pos.x + time - 1, xRange.size) + 1

    input(ny)(pos.x) == '^'
    || input(sy)(pos.x) == 'v'
    || input(pos.y)(ex) == '>'
    || input(pos.y)(wx) == '<'

def search(start: Point, end: Point, time: Int): Int =
  import collection.mutable.{PriorityQueue, Set}
  given Ordering[State] = Ordering.by(s => s.time + s.pos.distTo(end))

  val pq = PriorityQueue.empty[State].reverse
  var visiting = State(start, time)
  val visited = Set(visiting)

  while visiting.pos != end do
    val states = visiting.nextStates.filterNot(visited)
    pq.enqueue(states*)
    visited ++= states
    visiting = pq.dequeue()

  visiting.time

val ans1 = search(start, end, 0)
val t2 = search(end, start, ans1)
val ans2 = search(start, end, t2)
