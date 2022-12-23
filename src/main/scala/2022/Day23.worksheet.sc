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
  def surrounding = List(n, s, w, e, n.w, n.e, s.w, s.e)

def propose(elf: Point, elves: Set[Point], round: Int): Option[Point] =
  import elf.*
  def tryMoving =
    def goNorth = Option.unless(List(n, n.w, n.e).exists(elves))(n)
    def goSouth = Option.unless(List(s, s.w, s.e).exists(elves))(s)
    def goWest = Option.unless(List(w, n.w, s.w).exists(elves))(w)
    def goEast = Option.unless(List(e, n.e, s.e).exists(elves))(e)
    round % 4 match
      case 0 => goNorth orElse goSouth orElse goWest orElse goEast
      case 1 => goSouth orElse goWest orElse goEast orElse goNorth
      case 2 => goWest orElse goEast orElse goNorth orElse goSouth
      case 3 => goEast orElse goNorth orElse goSouth orElse goWest
  if surrounding.exists(elves) then tryMoving else None

def rounds = LazyList.unfold(elvesStart -> 0) { case (elves, round) =>
  val proposals = elves
    .groupBy(propose(_, elves, round))
    .collect {
      case (Some(dest), starts) if starts.sizeIs == 1 =>
        starts.head -> dest
    }
  Option.when(proposals.nonEmpty) {
    val (starts, dests) = proposals.toSet.unzip
    val next = elves.diff(starts).union(dests)
    next -> (next, round + 1)
  }
}

val roundTen = rounds(9)

val xRange = roundTen.map(_.x).min to roundTen.map(_.x).max
val yRange = roundTen.map(_.y).min to roundTen.map(_.y).max

val ans1 = xRange.size * yRange.size - roundTen.size

val ans2 = 1 + rounds.size
