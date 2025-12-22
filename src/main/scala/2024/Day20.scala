package y2024

import aoc.{Area, Point, Dir}

@main def d20: Unit =

  val grid = io.Source.fromResource("2024/day-20.txt").getLines.toVector
  val area = Area(grid)
  val startPos = area.pointsIterator.find(grid(_) == 'S').get
  val endPos = area.pointsIterator.find(grid(_) == 'E').get
  val walls = area.pointsIterator.filter(grid(_) == '#').toSet

  enum Cheat:
    case CanCheat
    case Cheating(time: Int, pos: Point)
    case Cheated(time: Int, pos: Point)

    def next(time: Int, pos: Point): Cheat = this match
      case CanCheat => Cheating(time, pos)
      case Cheating(time, pos) => Cheated(time, pos)
      case Cheated(_, _) => ???

  case class State(pos: Point, cheat: Cheat):
    def nonCheatingNext: Set[State] =
      pos.adjacent.diff(walls).filter(area.contains).map(State(_, cheat))

    def cheatingNext(time: Int): Set[State] =
      pos.adjacent.filter(area.contains).map: newPos =>
        State(newPos, cheat.next(time + 1, newPos))

    def next(time: Int): Set[State] = cheat match
      case Cheat.CanCheat => nonCheatingNext union cheatingNext(time)
      case Cheat.Cheating(_, _) => nonCheatingNext
      case Cheat.Cheated(_, _) => nonCheatingNext

  def search: Int =
    import collection.mutable.{PriorityQueue, Map}

    val start = State(startPos, Cheat.Cheated(0, startPos))
    val cost = Map[State, Int](start -> 0)
    val queue = PriorityQueue.empty[State](using Ordering.by(cost)).reverse

    var visiting = start

    while visiting.pos != endPos do
      visiting.next(cost(visiting)).diff(cost.keySet).foreach: s =>
        cost(s) = cost(visiting) + 1
        queue.enqueue(s)
      visiting = queue.dequeue

    cost(visiting)

  def findCheats(benchmark: Int): Int =
    import collection.mutable.{PriorityQueue, Map}

    val start = State(startPos, Cheat.CanCheat)
    val cost = Map[State, Int](start -> 0)
    val queue = PriorityQueue.empty[State](using Ordering.by(cost)).reverse

    var visiting = start
    // val cheats = collection.mutable.Set.empty[(State, Int)]
    var cheatCount = 0

    while cost(visiting) <= benchmark do
      if visiting.pos == endPos then
        // println(cost(visiting) -> visiting)
        // cheats += (visiting -> cost(visiting))
        cheatCount += 1
      else
        visiting.next(cost(visiting)).diff(cost.keySet).foreach: s =>
          cost(s) = cost(visiting) + 1
          queue.enqueue(s)
      visiting = queue.dequeue

    // cheats.toSet
    cheatCount

  val noCheatCost = search

  println(noCheatCost)

  val cheats = findCheats(noCheatCost - 100)

  val ans1 = cheats

  println(ans1)
