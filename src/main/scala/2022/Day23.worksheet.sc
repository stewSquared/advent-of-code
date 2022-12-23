val input = io.Source.fromResource("2022/day-23.txt").getLines().toVector

val elvesStart: Set[Point] =
  val points = for
    y <- input.indices
    x <- input.head.indices
    if input(y)(x) == '#'
  yield Point(x, y)
  points.toSet

case class Point(x: Int, y: Int):
  def n = copy(y = y - 1)
  def s = copy(y = y + 1)
  def w = copy(x = x - 1)
  def e = copy(x = x + 1)
  def surrounding = Set(e, s, n, w, s.e, s.w, n.w, n.e)

def propose(elf: Point, elves: Set[Point], round: Int): Option[Point] =
  import elf.*
  def tryMoving =
    def goNorth = Option.when(Set(n, n.e, n.w).intersect(elves).isEmpty)(n)
    def goSouth = Option.when(Set(s, s.e, s.w).intersect(elves).isEmpty)(s)
    def goWest = Option.when(Set(w, n.w, s.w).intersect(elves).isEmpty)(w)
    def goEast = Option.when(Set(e, n.e, s.e).intersect(elves).isEmpty)(e)
    round % 4 match
      case 0 => goNorth orElse goSouth orElse goWest orElse goEast
      case 1 => goSouth orElse goWest orElse goEast orElse goNorth
      case 2 => goWest orElse goEast orElse goNorth orElse goSouth
      case 3 => goEast orElse goNorth orElse goSouth orElse goWest
  Option.unless(surrounding.intersect(elves).isEmpty)(tryMoving).flatten

def next(elves: Set[Point], round: Int): Set[Point] =
  val proposals = elves
    .groupBy(propose(_, elves, round))
    .collect {
      case (Some(dest), starts) if starts.sizeIs == 1 =>
        starts.head -> dest
    }
  val (starts, dests) = proposals.toSet.unzip
  elves.diff(starts).union(dests)

def rounds = LazyList.from(0).scanLeft(elvesStart)(next)

val roundTen = rounds(10)

val xRange = roundTen.map(_.x).min to roundTen.map(_.x).max
val yRange = roundTen.map(_.y).min to roundTen.map(_.y).max

val ans1 = xRange.size * yRange.size - roundTen.size

val ans2 = 1 + rounds.zip(rounds.tail).indexWhere(_ == _)
