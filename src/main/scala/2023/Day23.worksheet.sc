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

case class State(pos: Point, visited: Set[Point]):
  def next: List[State] =
    val adj = Dir.values.map(d => d -> pos.move(d)).collect:
      case (d, p) if slopes.get(p).forall(_ == d) => p

    adj.toList.filter(path).filterNot(visited).map: p =>
      State(p, visited + pos)

State(start, Set.empty).next

State(start, Set.empty).next.flatMap(_.next) foreach println


def allPaths(states: List[State], completed: List[State]): List[State] =
  assert(states.forall(_.pos != end))

  if states.isEmpty then completed else
    val (newCompleted, nextStates) = states.flatMap(_.next).partition(_.pos == end) //.distinct?
    allPaths(nextStates, completed ++ newCompleted)

val ans1 = allPaths(List(State(start, Set.empty)), Nil).map(_.visited.size).max
