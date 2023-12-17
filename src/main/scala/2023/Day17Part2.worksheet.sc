import aoc.*

val grid = io.Source.fromResource("2023/day-17.txt").getLines.toVector

val area = Area(grid)

def viable2(dir: Dir, straight: Int): List[Dir] =
  if straight < 10 then
    List(dir, dir.turnLeft, dir.turnRight)
  else
    List(dir.turnLeft, dir.turnRight)

case class State(pos: Point, dir: Dir, straight: Int, heat: Int):
  def nextStates2: List[State] =
    viable2(dir, straight).filter: newDir =>
      if newDir == dir then area.contains(pos.move(newDir))
      else area.contains(pos.move(newDir, 4))
    .map: newDir =>
      if newDir != dir then
        val newPos = pos.move(newDir, 4)
        val newStraight = 4
        val newHeat = Line(pos.move(newDir), newPos).points.map(grid(_).asDigit).sum + heat
        State(newPos, newDir, newStraight, newHeat)
      else
        val newPos = pos.move(newDir)
        val newStraight = straight + 1
        State(newPos, newDir, newStraight, heat + grid(newPos).asDigit)

def search2(start: Point, end: Point): Int =
  import collection.mutable.{PriorityQueue, Set, Map}

  // given Ordering[State] = Ordering.by(s => s.pos.dist(end) + s.heat)
  given Ordering[State] = Ordering.by(s => s.pos.dist(end) + s.heat)

  val movedEast = State(start.move(Dir.E, 4), Dir.E, 4, Line(start.e, start.move(Dir.E, 4)).points.map(grid(_).asDigit).sum)
  val movedSouth = State(start.move(Dir.S, 4), Dir.S, 4, Line(start.s, start.move(Dir.S, 4)).points.map(grid(_).asDigit).sum)
  var visiting = List(movedEast, movedSouth).minBy(_.heat)
  val other = List(movedEast, movedSouth).maxBy(_.heat)

  val pq = PriorityQueue.empty[State].reverse
  val visited = Set(visiting)
  pq.enqueue(other)
  val bestHeat = Map.empty[Point, Int]
  bestHeat(visiting.pos) = visiting.heat
  bestHeat(other.pos) = other.heat

  while visiting.pos != end do
    println(s"visiting: $visiting")
    val states = visiting.nextStates2
    .filterNot: s =>
      (4 to s.straight).exists: straight =>
        visited(s.copy(straight = straight))
    .filter: s =>
      bestHeat.get(s.pos).forall(s.heat - 20 < _)

    // states.foreach: s =>
    //   println(s"enqueuing $s")

    states.foreach: s =>
      bestHeat(s.pos) = bestHeat.getOrElse(s.pos, Int.MaxValue).min(s.heat)
    pq.enqueue(states*)
    visited ++= states
    visiting = pq.dequeue()

  visiting.heat

2 + 2
// val ans1 = search(Point.origin, area.botRight)
val ans2 = search2(Point.origin, area.botRight)
// 1137 too high

// State(Point.origin.move(Dir.E, 4), Dir.E, 4, Line(Point.origin.e, Point.origin.move(Dir.E, 4)).points.map(grid(_).asDigit).sum)



//
