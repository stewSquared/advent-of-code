val lines = io.Source.fromResource("2022/day-12.txt").getLines().toVector

val grid = lines.map(_.toVector)

case class Point(x: Int, y: Int):
  def u = copy(y = y + 1)
  def d = copy(y = y - 1)
  def r = copy(x = x + 1)
  def l = copy(x = x - 1)

  def adj = List(u,d,r,l)

extension (grid: Vector[Vector[Char]])
  def apply(p: Point) = grid(p.y)(p.x)
  def elevation(p: Point): Int = grid(p.y)(p.x) match
    case 'S' => 'a'.toInt
    case 'E' => 'z'.toInt
    case c => c.toInt

val xRange = grid(0).indices
val yRange = grid.indices

def inBounds(p: Point) = xRange.contains(p.x) && yRange.contains(p.y)

val points = for
  x <- xRange
  y <- yRange
yield Point(x, y)

val start = points.find(grid(_) == 'S').get
val end = points.find(grid(_) == 'E').get

// Part 1
// var visiting = start
// val cost = collection.mutable.Map[Point, Int](start -> 0)

val lowPoints = points.filter(grid.elevation(_) == 'a'.toInt)
val cost = collection.mutable.Map[Point, Int](lowPoints.map(_ -> 0)*)
val toVisit = collection.mutable.PriorityQueue.empty[Point](Ordering.by(cost)).reverse
toVisit.enqueue(lowPoints*)

var visiting = toVisit.dequeue()

while !cost.contains(end) do
  println(visiting)
  val next = visiting.adj
    .filter(inBounds)
    .filterNot(cost.contains)
    .filter(q => grid.elevation(q) <= grid.elevation(visiting) + 1)
  next.foreach { q => cost(q) = cost(visiting) + 1}
  next.foreach {toVisit.enqueue(_)}
  visiting = toVisit.dequeue()

val ans = cost(end)
