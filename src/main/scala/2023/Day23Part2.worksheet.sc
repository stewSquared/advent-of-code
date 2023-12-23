import aoc.*

val maze = io.Source.fromResource("2023/day-23.txt").getLines.toVector

val area = Area(maze)

val slopes = Map.from[Point, Dir]:
  area.pointsIterator.collect:
    case p if maze(p) == '>' => p -> Dir.E
    case p if maze(p) == 'v' => p -> Dir.S
    case p if maze(p) == '<' => p -> Dir.W
    case p if maze(p) == '^' => p -> Dir.N

val path = Set.from[Point]:
  area.pointsIterator.filter(p => maze(p) != '#')

val start = path.find(_.y == area.top).get
val end = path.find(_.y == area.bot).get

// path.contains(Point(3,4))
// path.contains(Point(3,4))

val nodes = area.pointsIterator.filter: p =>
  path(p) && p.adjacent.count(path) > 2
.toSet + start + end

def adjacentNodes(states: List[State], completed: List[State]): List[(Point, Int)] =
  assert(states.forall(s => !nodes(s.pos)))

  if states.isEmpty then
    completed.map(s => s.pos -> s.visited.size)
  else
    val (newCompleted, nextStates) = states.flatMap(_.next).partition(s => nodes(s.pos))
    adjacentNodes(nextStates, completed ++ newCompleted)

val ss = State(start, Set.empty)

val adj = Map.from[Point, List[(Point, Int)]]:
  nodes.map: n =>
    n -> adjacentNodes(State(n, Set.empty).next, Nil)

adj.toList.flatMap:
  case (p, pds) => pds.map:
    case (p2, d) =>
      val s = List(p,p2).sortBy(p => p.x -> p.y)
      (s(0), d, s(1))
.distinct
.foreach:
  case (p, d, q) => println(s""" "${p.x},${p.y}" -- "${q.x},${q.y}" [label=$d];""")



adj(start)

nodes.size

// adj.toList.sortBy(_._1.dist(Point.origin)) foreach println

2 + 2

case class State(pos: Point, visited: Set[Point]):
  def next: List[State] =
    pos.adjacent.toList.filter(path).filterNot(visited).map: p =>
      State(p, visited + pos)

case class SearchState(node: Point, dist: Int):
  def next(visited: Set[Point]): List[SearchState] =
    adj(node).collect:
      case (n, d) if !visited(n) =>
        SearchState(n, dist + d)

def allPaths(state: SearchState, path: List[SearchState]): List[SearchState] =
  val (completed, toSearch) = state.next(path.map(_.node).toSet).partition(s => s.node == end)

  toSearch.flatMap(s => allPaths(s, state :: path)) ++ completed

val ans2 = allPaths(SearchState(start, 0), Nil).map(_.dist).max
