import aoc.*

val grid = io.Source.fromResource("2023/day-17-ex.txt").getLines.toVector

val area = Area(grid)

def viable(dir: Dir, straight: Int): List[Dir] =
  Option.when(straight < 3)(dir).toList ::: List(dir.turnLeft, dir.turnRight)

case class State(pos: Point, dir: Dir, straight: Int, heat: Int):
  def nextStates: List[State] =
    viable(dir, straight).filter(d => area.contains(pos.move(d))).map: newDir =>
      val newPos = pos.move(newDir)
      val newStraight = if newDir == dir then straight + 1 else 1
      State(newPos, newDir, newStraight, heat + grid(newPos).asDigit)

def search(start: Point, end: Point): Int =
  import collection.mutable.{PriorityQueue, Set, Map}

  given Ordering[State] = Ordering.by(s => s.pos.dist(end) + s.heat)

  val pq = PriorityQueue.empty[State].reverse
  var visiting = State(start, Dir.E, 0, 0)
  val visited = Set(visiting)
  val bestHeat = Map.empty[Point, Int]
  bestHeat(visiting.pos) = 0

  while visiting.pos != end do
    // println(s"visiting: $visiting")
    val states = visiting.nextStates.filterNot: s =>
      (0 to s.straight).exists: straight =>
        visited(s.copy(straight = straight))
    .filter: s =>
      bestHeat.get(s.pos).forall(s.heat - 5 < _)
    states.foreach: s =>
      bestHeat(s.pos) = bestHeat.getOrElse(s.pos, Int.MaxValue).min(s.heat)
    pq.enqueue(states*)
    visited ++= states
    visiting = pq.dequeue()

  visiting.heat

val ans1 = search(Point.origin, area.botRight)
