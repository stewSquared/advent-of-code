
val input = io.Source.fromResource("2022/day-17.txt").getLines().next()

case class Point(x: Long, y: Long):
  def l = copy(x = x - 1)
  def r = copy(x = x + 1)
  def d = copy(y = y - 1)

enum Shape:
  case Horz, Cross, Angle, Vert, Box

  def start(max: Long): Set[Point] = this match
    case Horz => (2 to 5).map(x => Point(x, max + 4)).toSet
    case Cross =>
      (2 to 4).map(x => Point(x, max + 5)).toSet +
        Point(3, max + 4) + Point(3, max + 6)
    case Angle =>
      (2 to 4).map(x => Point(x, max + 4)).toSet + Point(4, max + 5) + Point(
        4,
        max + 6
      )
    case Vert =>
      ((max + 4) to (max + 7)).map(y => Point(2, y)).toSet
    case Box =>
      Set(
        Point(2, max + 4),
        Point(3, max + 4),
        Point(2, max + 5),
        Point(3, max + 5)
      )

def touching(shape: Set[Point], rocks: Set[Point]) =
  shape.map(_.d).exists(rocks)

def fall(
    shape: Shape,
    rocks: Set[Point],
    jets: LazyList[Char]
): (Set[Point], LazyList[Char], Int) =
  val start = shape.start(rocks.map(_.y).max)

  def valid(shape: Set[Point]) =
    val xRange = 0 until 7
    shape.forall(p => xRange.contains(p.x)) && !shape.exists(rocks)

  val positions = Iterator.iterate((start, jets, true)) {
    case (shape, jets, false) => (shape, jets, false)
    case (shape, jets, falling) =>
      val pushed = jets match
        case '<' #:: _ =>
          val left = shape.map(_.l)
          if valid(left) then left else shape
        case '>' #:: _ =>
          val right = shape.map(_.r)
          if valid(right) then right else shape

      val down = pushed.map(_.d)
      val stopped = down.exists(rocks)
      val next = if stopped then pushed else down
      (next, jets.tail, !stopped)
  }
  val falls = positions.takeWhile(_._3).size
  val (resting, remainingJets, _) = positions.next()
  (rocks.union(resting), remainingJets, falls)

def shapes: Iterator[Shape] = Iterator.continually(Shape.values).flatten

val floor = (0 until 7).map(x => Point(x, 0)).toSet

val longIndices = Iterator.iterate(0L)(_ + 1L)

def shapesWithIndices = shapes.zip(longIndices)

def infiniteInput: LazyList[Char] = LazyList.continually(input).flatten

1_000_000_000_000L / input.size

def shapesTake(n: Long) = shapesWithIndices.takeWhile(_._2 < n).map(_._1)

def states(n: Long) =
  shapesTake(n).scanLeft[(Set[Point], LazyList[Char], Int)](floor, infiniteInput, 0) {
    // case ((rocks, jets), shape) => fall(shape, rocks, jets)
    case ((rocks, jets, used), shape) =>
      val (nextRock, remainingJets, jetsUsed) = fall(shape, rocks, jets)
      (nextRock, remainingJets, jetsUsed + used)
  }

def jetsUsed = states(3000).map(_._3).sliding(2).collect {
  case Seq(l, r) => r - l
}

def repeated(seq: Vector[Int]): (Int, Int) =
  // @annotation.tailrec
  def search(i: Int): (Int, Int) =
    val hook = seq.slice(i, i + 50)
    val matchIndex = seq.indexOfSlice(hook)
    val repeating = seq.slice(matchIndex, i)
    if matchIndex < i && seq.drop(matchIndex).startsWith(repeating) then
      matchIndex -> repeating.size
    else
      search(i + 1)

  search(0)

val (repeatsStart, repeatSize) = repeated(jetsUsed.toVector)
// 447, 1735

// states(447).toList.last._1.map(_.y).max
val heightBeforeRepeat = 675L

// states(447 + 1735).toList.last._1.map(_.y).max
val heightAfterFirst = 3456L

val tril = 1_000_000_000_000L

val extra = (tril - repeatsStart) % repeatSize
val numRepeats = (tril - repeatsStart) / repeatSize

val repeatHeight = heightAfterFirst - heightBeforeRepeat

states(repeatsStart + extra).toList.last._1.map(_.y).max
val heightWithExtra = 2972

val ans2 =  heightWithExtra + (repeatHeight * numRepeats)
