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

case class SearchState(node: Point, visited: Set[Point], dist: Int):
  def next: List[SearchState] =
    adj(node).collect:
      case (n, d) if !visited(n) =>
        SearchState(n, visited + node, dist + d)

def allPaths(paths: List[SearchState], completed: List[SearchState]): List[SearchState] =
  paths.foreach: s =>
    print(s"${s.node} ${s.dist} |");
  println()
  // println(paths.map(_.node))
  if paths.isEmpty then completed else
    val (newCompleted, nextPaths) = paths.flatMap(_.next).partition(s => s.node == end)
    allPaths(nextPaths, completed ++ newCompleted)

// allPaths(List(SearchState(start, Set.empty, 0)), Nil).map(_.dist).max


case class SearchState2(node: Point, prev: Option[Point], dist: Int):
  def next(visited: Set[Point]): List[SearchState2] =
    adj(node).collect:
      case (n, d) if !visited(n) =>
        SearchState2(n, Some(node), dist + d)

def getVisited(ss: SearchState2, back: Map[SearchState2, SearchState2]): Set[Point] =
  if ss.node == start then Set.empty else
    val prev = back(ss)
    getVisited(prev, back) + prev.node

// val ss2 = SearchState2(start, 0)
// ss2.next(Set.empty)

def allPaths2(paths: List[SearchState2], back: Map[SearchState2, SearchState2], completed: List[SearchState2]): List[SearchState2] =
  paths.foreach: s =>
    print(s"${s.node} ${s.dist} |");
  println()
  // println(paths.map(_.node))
  // println(paths)
  // back.foreach: tup =>
  //   println(s"  $tup")
  if paths.isEmpty then completed else
    val (newCompleted, nextPaths) = paths.flatMap(s => s.next(getVisited(s, back) + s.node).map(s2 => s -> s2)).partition { case (_, s) => s.node == end }
    val newBack = nextPaths.foldLeft(back):
      case (b, (s1, s2)) =>
        // assert(b.get(s2))
        b + (s2 -> s1)

    // nextPaths.foldLeft(back) { case (b, s) => b + (s -> paths.find(_.node == s.node).get) }
    allPaths2(nextPaths.map(_._2), newBack, completed ++ newCompleted.map(_._2))


allPaths2(List(SearchState2(start, None, 0)), Map.empty, Nil).map(_.dist).max

159 + 80 + 310 + 112 + 222 + 520 + 280 + 250 + 146 + 140 + 74 + 232 + 72 + 334 + 44 + 440 + 472 + 60 + 222 + 20 + 188 + 196 + 198 + 162 + 156 + 392 + 84 + 128 + 190 + 112 + 336 + 188 + 158 + 49
6736 - 80 - 116 - 72 - 20 - 188 - 198 - 74 + 102 + 202 + 72 + 120 + 170 + 138
// ^v one off these is fishy
49+158 + 188 + 336 + 112 + 190 +28+84+392+156+162+102+202+140+46+250+280+520+222+112+310+170+232+72+196+120+222+60+472+440+44+334+138+159

// 6698
// 6792 is wrong
// 6736 still too low
// 6022 still too low
// 5118 is too low
//
