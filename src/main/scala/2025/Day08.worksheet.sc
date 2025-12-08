val input = io.Source.fromResource("2025/day-08.txt").getLines().toList

case class Point(x: Int, y: Int, z: Int):
  def dist(p: Point): Long =
    val dx = x.toLong - p.x.toLong
    val dy = y.toLong - p.y.toLong
    val dz = z.toLong - p.z.toLong
    dx*dx + dy*dy + dz*dz

val boxes: List[Point] = input.collect:
  case s"$x,$y,$z" => Point(x.toInt, y.toInt, z.toInt)
.sortBy(p => (p.x, p.y, p.z))

val edges = boxes.combinations(2).collect:
  case List(a, b) => a -> b
.toList.sortBy(_.dist(_))

boxes.size
edges.size

type Edge = (Point, Point)

def sameCircuit(a: Point, b: Point, adj: Map[Point, Set[Point]]): Boolean =
  def search(visited: Set[Point], current: Set[Point]): Boolean =
    current.nonEmpty && (current.contains(b) || locally:
      val next = current.flatMap(adj).diff(visited)
      search(visited.union(current), next)
    )
  search(Set.empty, Set(a))

def connect(edge: (Point, Point), adj: Map[Point, Set[Point]]): Map[Point, Set[Point]] =
  val (a, b) = edge
  adj.updated(a, adj(a) + b)
    .updated(b, adj(b) + a)

val emptyAdj = Map.empty[Point, Set[Point]].withDefaultValue(Set.empty)

val allStates = edges.scanLeft(emptyAdj):
  case (adj, edge@(a, b)) => connect(edge, adj)

val endState = allStates(1000)

def component(a: Point, adj: Map[Point, Set[Point]]): Set[Point] =
  def flood(visited: Set[Point], current: Set[Point]): Set[Point] =
    if current.isEmpty then visited else
      val next = current.flatMap(adj).diff(visited)
      flood(visited.union(current), next)
  flood(Set.empty, Set(a))

def components(adj: Map[Point, Set[Point]]) =
  Iterator.unfold(boxes.toSet): unsorted =>
    Option.unless(unsorted.isEmpty):
      val a = unsorted.head
      val c = component(a, adj)
      c -> unsorted.diff(c)
  .toList

val ans1 = components(endState).toList.map(_.size)
  .sorted.takeRight(3).product

def fullyConnected(adj: Map[Point, Set[Point]]): Boolean =
  val a = adj.head._1
  component(a, adj).sizeIs == boxes.size

// val index = allStates.drop(1).indexWhere(fullyConnected)
// val finalEdge = edges(index)

// val ans2 =
//   val (a, b) = finalEdge
//   a.x.toLong * b.x.toLong

// 741379604
