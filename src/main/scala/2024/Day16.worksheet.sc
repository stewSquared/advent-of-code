import aoc.{Point, Area, Dir}

val maze = io.Source.fromResource("2024/day-16.txt").getLines.toVector

val area = Area(maze)
val startPos = area.pointsIterator.find(maze(_) == 'S').get
val endPos = area.pointsIterator.find(maze(_) == 'E').get
val walls = area.pointsIterator.filter(maze(_) == '#').toSet

case class State(pos: Point, dir: Dir):
  def next: List[(State, Int)] =
    List(dir, dir.turnLeft, dir.turnRight)
      .zip(List(1, 1001, 1001))
      .map((d, s) => ((pos.move(d), d, s)))
      .filter((p, _, _) => p.inBounds(area) && !walls(p))
      .map((p, d, s) => State(p, d) -> s)

def search: (Int, Int) =
  import collection.mutable.{PriorityQueue, Map}

  val start = State(startPos, Dir.E)
  val minCost = Map[State, Int](start -> 0)
  val queue = PriorityQueue.empty[State](Ordering.by(minCost).reverse)

  val pathsBack = Map
    .empty[(State, Int), List[(State, Int)]]
    .withDefaultValue(Nil)

  var visiting = start
  var endCost = Int.MaxValue

  while minCost(visiting) <= endCost do
    if visiting.pos == endPos then
      endCost = endCost min minCost(visiting)

    visiting.next
      .filterNot: (s, c) =>
        minCost.get(s).exists(_ < minCost(visiting) + c)
      .foreach: (s, c) =>
        minCost(s) = minCost(visiting) + c
        queue.enqueue(s)
        pathsBack(s -> minCost(s)) =
          (visiting, minCost(visiting)) :: pathsBack(s -> minCost(s))

    visiting = queue.dequeue

  val endStates = Dir.values.map(State(endPos, _) -> endCost).toSet
  val seats = Iterator.unfold(endStates): states =>
    val nextStates = states.flatMap(pathsBack)
    val nextPositions = nextStates.map(_._1.pos)
    Option.when(nextStates.nonEmpty)(nextPositions -> nextStates)
  .reduce(_ union _) + endPos

  println:
    area.draw: p =>
      if seats.contains(p) then '█'
      else if walls(p) then '#'
      else ' '

  (endCost, seats.size)

val (ans1, ans2) = search
