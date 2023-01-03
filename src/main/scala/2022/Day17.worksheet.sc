val input = io.Source.fromResource("2022/day-17.txt").getLines().next()

case class Point(x: Long, y: Long):
  def l = copy(x = x - 1)
  def r = copy(x = x + 1)
  def d = copy(y = y - 1)

enum Shape(points: List[Point]):
  case Horz extends Shape((2 to 5).map(Point(_, 4)).toList)
  case Cross extends Shape(List((3, 4), (2, 5), (3, 5), (4, 5), (3, 6)).map(Point(_, _)))
  case Angle extends Shape(List((2, 4), (3, 4), (4, 4), (4, 5), (4, 6)).map(Point(_, _)))
  case Vert extends Shape((4 to 7).map(Point(2, _)).toList)
  case Box extends Shape(List((2, 4), (3, 4), (2, 5), (3, 5)).map(Point(_, _)))

  def start(max: Long) = this.points.map(p => p.copy(y = p.y + max))

def jetAt(index: Int): Char = input(index % input.length)

case class State(rocks: Set[Point], jets: Int, height: Long):
  def drop(shape: Shape): State =
    val start = shape.start(height)
    val positions = Iterator.iterate((start, jets, true)) {
      case (shape, i, false) => (shape, i, false)
      case (shape, i, falling) =>
        val pushed =
          val dir = if jetAt(i) == '<' then shape.map(_.l) else shape.map(_.r)
          val valid =
            dir.forall(p => (0 until 7).contains(p.x) && !rocks(p))
          if valid then dir else shape
        val down = pushed.map(_.d)
        val stopped = down.exists(rocks)
        val next = if stopped then pushed else down
        (next, i + 1, !stopped)
    }
    val (resting, i) = positions.dropWhile(_._3).next().take(2)
    State(rocks.concat(resting), i, height.max(resting.maxBy(_.y).y))

def states =
  val floor = (0 until 7).map(x => Point(x, 0)).toSet
  val shapes = LazyList.continually(Shape.values).flatten
  shapes.scanLeft(State(floor, 0, 0))(_ drop _)

val heightAfter = states.map(_.height)

def repeated(seq: Vector[Int]): (Int, Int) =
  def search(i: Int): (Int, Int) =
    val hook = seq.slice(i, i + 50)
    val matchIndex = seq.indexOfSlice(hook)
    val repeating = seq.slice(matchIndex, i)
    if matchIndex < i && seq.drop(matchIndex).startsWith(repeating) then
      matchIndex -> repeating.size
    else search(i + 1)

  search(0)

val (cycleStart, period) =
  val jetsUsed = states.map(_.jets)
  val deltas = jetsUsed.tail.zip(jetsUsed).map(_ - _)
  repeated(deltas.take(3000).toVector)

val tril = 1_000_000_000_000L
val extra = ((tril - cycleStart) % period).toInt
val heightWithExtra = heightAfter(cycleStart + extra)

val numCycles = (tril - cycleStart) / period

val heightBeforeCycle = heightAfter(cycleStart)
val heightAfterFirst = heightAfter(cycleStart + period)
val cycleHeight = heightAfterFirst - heightBeforeCycle

val ans1 = heightAfter(2022)
val ans2 = heightWithExtra + (cycleHeight * numCycles)
