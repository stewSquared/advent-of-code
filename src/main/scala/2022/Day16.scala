package `2022`

val input = io.Source.fromResource("2022/day-16.txt").getLines().toList

val flowRate: Map[String, Int] = input.collect {
  case s"Valve $valve has flow rate=$rate; $_" =>
    valve -> rate.toInt
}.toMap

val adj: Map[String, List[String]] = input.collect {
  case s"Valve $valve has $_ valves $valves" =>
    valve -> valves.split(", ").toList // maybe sort
  case s"Valve $valve has $_ valve $other" =>
    valve -> List(other)
}.toMap

def prune(room: String, canVisit: Map[String, Map[String, Int]]) =
  val adj = canVisit(room).keys.toVector

  val removed = canVisit
    .removed(room)
    .view
    .mapValues(_.removed(room))
    .toMap

  val costs = for
    ia <- adj.indices
    ib <- 0 until ia
    a = adj(ia)
    b = adj(ib)
    oldCost = canVisit(a).get(b).getOrElse(Int.MaxValue)
    newCost = canVisit(room)(a) + canVisit(room)(b)
  yield
    assert(a != b, s"$a and $b were the same")
    assert(
      (canVisit(a).get(b)) == (canVisit(b).get(a)),
      s"asymmetric costs $a $b \n ${canVisit(a).get(b)} \n ${canVisit(b).get(a)}"
    )
    (a, b) -> (newCost min oldCost)

  costs.foldLeft(removed) { case (acc, ((a, b), c)) =>
    acc
      .updated(a, acc(a).updated(b, c))
      .updated(b, acc(b).updated(a, c))
  }

val empty = flowRate.collect {
  case (k, f) if f == 0 && k != "AA" => k
}

val bigAdj: Map[String, Map[String, Int]] =
  val start = adj.view.mapValues(_.map(a => a -> 1).toMap).toMap
  empty.foldLeft(start)((acc, a) => prune(a, acc))

val bigNodes = bigAdj.keySet

def fill(startNode: String): Map[String, Int] =
  import collection.mutable.{PriorityQueue, Map}
  // var visited = startCosts.keys
  val cost = Map[String, Int](bigAdj(startNode).toSeq*)
  val toVisit = PriorityQueue.empty[String](using Ordering.by(cost)).reverse
  toVisit.enqueue(cost.keys.toSeq*)
  var visiting = toVisit.dequeue()

  while cost.size < bigNodes.size - 1 do
    for
      (n, c) <- bigAdj(visiting)
      // if !cost.contains(n)
      newCost = cost(visiting) + c
      if cost.get(n).forall(newCost < _)
      if n != startNode
    do
      // println(s"setting cost of $startNode -> $n as ${cost(visiting)}($visiting) + $c")
      cost(n) = cost(visiting) + c
      toVisit.enqueue(n)

    visiting = toVisit.dequeue()

  cost.toMap

val canVisit = bigNodes.iterator.map { n =>
  n -> fill(n)
}.toMap

case class PlayerState(visiting: String, time: Int):
  def travelling = time > 0

case class DoubleSearchState(
    state: PlayerState,
    state2: PlayerState,
    time: Int,
    pressure: Int,
    flow: Int,
    opened: Set[String],
    indent: Int = 0
):
  def tick: DoubleSearchState =
    copy(
      state = state.copy(time = state.time - 1),
      state2 = state2.copy(time = state2.time - 1),
      time = time - 1,
      pressure = pressure + flow,
      indent = indent + 1
    )

  def openValve(room: String) =
    // maybe assert player is in room
    copy(flow = flow + flowRate(room), opened = opened + room)

  def increaseFlow(f: Int) = copy(flow = flow + f)
  def ignore(room: String) = copy(opened = opened + room)
  def availableRooms(from: String): Option[Seq[(String, Int)]] =
    val rooms = canVisit(from).view.filterKeys { r =>
      !(opened(r) || r == state.visiting || r == state2.visiting)
    }.toSeq
    Option.when(rooms.nonEmpty)(rooms)

  def send(nextRoom: String) =
    // assert in room
    val cost = canVisit(state.visiting)(nextRoom)
    copy(state = PlayerState(nextRoom, cost))

  def send2(nextRoom: String) =
    // assert in room
    val cost = canVisit(state2.visiting)(nextRoom)
    copy(state2 = PlayerState(nextRoom, cost))

  def stand = copy(state = state.copy(time = time))
  def stand2 = copy(state2 = state2.copy(time = time))

  def next: List[DoubleSearchState] =
    // println(s"${"." * indent}$state $eState f:$flow p:$pressure t:$time")
    if time < 0 then ???
    else if time < 20 && (pressure + flow * time) < 1000 then Nil
    // ^ this line is super important for performance
    else if time == 0 then Nil
    else
      (state, state2) match
        case (s, e) if s.travelling && e.travelling => List(this.tick)

        case (PlayerState(room, 0), e) if e.travelling =>
          val nextStates =
            for
              (nextRoom, cost) <- availableRooms(room).getOrElse(
                Map(room -> 100)
              )
            yield this.tick.openValve(room).send(nextRoom)

          nextStates.toList

        case (s, PlayerState(room, 0)) if s.travelling =>
          val nextStates = for
            // (nextRoom, cost) <- canVisit.get(room).getOrElse(Map(room -> 100))
            (nextRoom, cost) <- canVisit(room)
            if nextRoom != s.visiting
            if !opened(nextRoom)
          yield this.tick.openValve(room).send2(nextRoom)

          if nextStates.nonEmpty then nextStates.toList
          else List(this.tick.openValve(room).stand2)

        case (PlayerState(room, 0), PlayerState(room2, 0)) =>
          val nextStates = for
            (nextRoom, cost) <- canVisit.get(room).getOrElse(Map(room -> 100))
            (nextRoom2, cost2) <- canVisit
              .get(room2)
              .getOrElse(Map(room2 -> 100))
            if !opened(nextRoom)
            if !opened(nextRoom2)
            if nextRoom != room2
            if nextRoom2 != room
            if nextRoom != nextRoom2
          yield
            if room == room2 && room2 == "AA" then
              this
                .ignore("AA")
                .copy(
                  state = PlayerState(nextRoom, cost),
                  state2 = PlayerState(nextRoom2, cost2)
                )
            else
              this.tick
                .openValve(room)
                .openValve(room2)
                .send(nextRoom)
                .send2(nextRoom2)

          if nextStates.nonEmpty then nextStates.toList
          else if canVisit(room).keySet == canVisit(room2).keySet then
            assert(canVisit(room).keys.size == 1)
            val sharedRoom = canVisit(room).keys.head
            val cost = canVisit(room)(sharedRoom)
            val cost2 = canVisit(room2)(sharedRoom)
            val nextState =
              if cost < cost2 then
                this.tick
                  .openValve(room)
                  .openValve(room2)
                  .send(sharedRoom)
                  .stand2
              else
                this.tick
                  .openValve(room)
                  .openValve(room2)
                  .stand
                  .send2(sharedRoom)
            List(nextState)
          else
            val bothStand = this.tick
              .openValve(room)
              .openValve(room2)
              .stand
              .stand2
            List(bothStand)

        case (_, _) => ???

val startState = DoubleSearchState(
  PlayerState("AA", 0),
  PlayerState("AA", 0),
  time = 26,
  pressure = 0,
  opened = Set.empty,
  flow = 0
)

def maxPressure(state: DoubleSearchState): Int =
  state.next.iterator.map(maxPressure(_)).maxOption.getOrElse(state.pressure)

@main def day16(): Unit =
  val ans2 = maxPressure(startState)
  println(ans2)
