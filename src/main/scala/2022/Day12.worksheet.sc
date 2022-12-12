import collection.mutable.{Map, PriorityQueue}

val grid = io.Source.fromResource("2022/day-12.txt").getLines().toVector

case class Point(x: Int, y: Int):
  def u = copy(y = y + 1)
  def d = copy(y = y - 1)
  def l = copy(x = x + 1)
  def r = copy(x = x - 1)

  def inBounds = xRange.contains(x) && yRange.contains(y)
  def adj = List(u,d,l,r).filter(_.inBounds)

val xRange = grid(0).indices
val yRange = grid.indices

val points = for
  y <- yRange
  x <- xRange
yield Point(x, y)

def elevation(p: Point) = grid(p.y)(p.x) match
  case 'S' => 'a'
  case 'E' => 'z'
  case c => c

val start = points.find(p => grid(p.y)(p.x) == 'S').get
val end = points.find(p => grid(p.y)(p.x) == 'E').get
val lowPoints = points.filter(elevation(_) == 'a')

def ans(startingPoints: Seq[Point]) =
  val cost = Map[Point, Int](startingPoints.map(_ -> 0)*)
  val toVisit = PriorityQueue.empty(Ordering.by(cost)).reverse
  toVisit.enqueue(startingPoints*)

  var visiting = toVisit.dequeue()

  while !cost.contains(end) do
    visiting.adj
      .filterNot(cost.contains)
      .filter(elevation(_) <= elevation(visiting) + 1)
      .foreach { q =>
        cost(q) = cost(visiting) + 1
        toVisit.enqueue(q)
      }
    visiting = toVisit.dequeue()

  cost(end)

val ans1 = ans(List(start))
val ans2 = ans(lowPoints)
