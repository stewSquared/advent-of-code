import aoc.*

val grid = io.Source.fromResource("2023/day-17-ex.txt").getLines
  .map(s => s.map(_.asDigit).toVector).toVector

val area = Area(grid)

def heatLoss(p: Point) = if area.contains(p) then grid(p) else 0

case class State(pos: Point, dir: Dir, streak: Int, totalHeatLoss: Int):
  def straight: State =
    val newPos = pos.move(dir)
    State(pos.move(dir), dir, streak + 1, totalHeatLoss + heatLoss(newPos))

  def turnLeft: State =
    val newDir = dir.turnLeft
    val newPos = pos.move(newDir)
    State(newPos, newDir, 1, totalHeatLoss + heatLoss(newPos))

  def turnRight: State =
    val newDir = dir.turnRight
    val newPos = pos.move(newDir)
    State(pos.move(newDir), newDir, 1, totalHeatLoss + heatLoss(newPos))

  def nextStates: List[State] =
    List(straight, turnLeft, turnRight).filter: s =>
      area.contains(s.pos) && s.streak <= 3

  def nextStates2: List[State] =
    if streak < 4 then List(straight)
    else List(straight, turnLeft, turnRight).filter: s =>
      area.contains(s.pos) && s.streak <= 10

  def trajectory = (pos, dir, streak)

def search(next: State => List[State]): Int =
  import collection.mutable.{PriorityQueue, Set, Map}

  given Ordering[State] = Ordering.by(_.totalHeatLoss)
  val pq = PriorityQueue.empty[State].reverse

  var visiting = State(Point.origin, Dir.E, 0, 0)
  val visited = Set(visiting.trajectory)

  while visiting.pos != area.botRight do
    println(s"visiting: $visiting")
    val states = next(visiting).filterNot(s => visited(s.trajectory))
    pq.enqueue(states*)
    visited ++= states.map(_.trajectory)
    visiting = pq.dequeue()

  visiting.totalHeatLoss

val ans1 = search(_.nextStates)
val ans2 = search(_.nextStates2)
