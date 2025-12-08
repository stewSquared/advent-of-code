val input = io.Source.fromResource("2025/day-08.txt").getLines().toList

case class Point(x: Long, y: Long, z: Long):
  def dist(p: Point): Long =
    val dx = x - p.x
    val dy = y - p.y
    val dz = z - p.z
    dx*dx + dy*dy + dz*dz

type Edge = (Point, Point)
type Adj = Map[Point, Set[Point]]

val boxes: List[Point] = input.collect:
  case s"$x,$y,$z" => Point(x.toLong, y.toLong, z.toLong)

val edges = boxes.combinations(2).collect:
  case List(p, q) => p -> q
.toList.sortBy(_.dist(_))

def connect(adj: Adj, edge: Edge): Adj =
  val (p, q) = edge
  adj.updated(p, adj(p) + q)
    .updated(q, adj(q) + p)

def component(a: Point, adj: Adj): Set[Point] =
  def flood(visited: Set[Point], current: Set[Point]): Set[Point] =
    if current.isEmpty then visited else
      val next = current.flatMap(adj).diff(visited)
      flood(visited.union(current), next)
  flood(Set.empty, Set(a))

def components(adj: Adj) = List.from:
  Iterator.unfold(boxes.toSet): unsorted =>
    unsorted.headOption.map: p =>
      val c = component(p, adj)
      c -> unsorted.diff(c)

val emptyAdj: Adj = Map.empty.withDefaultValue(Set.empty)
val states = LazyList.from(edges).scanLeft(emptyAdj)(connect(_, _))

val ans1 = components(states(1000)).map(_.size)
  .sorted.takeRight(3).product

def fullyConnected(adj: Adj): Boolean =
  adj.keys.headOption.exists(component(_, adj).sizeIs == boxes.size)

val finalEdge = edges(states.indexWhere(fullyConnected))

val ans2 =
  val (p, q) = finalEdge
  p.x.toLong * q.x.toLong
