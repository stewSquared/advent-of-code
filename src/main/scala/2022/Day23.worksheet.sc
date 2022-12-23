val input = io.Source.fromResource("2022/day-23-ex.txt").getLines().toVector

val elvesStart: Set[Point] =
  val points =
    for
      y <- input.indices
      x <- input.head.indices
      if input(y)(x) == '#'
    yield Point(x, y)
  points.toSet

case class Point(x: Int, y: Int):
  def e = copy(x = x + 1)
  def s = copy(y = y + 1)
  def w = copy(x = x - 1)
  def n = copy(y = y - 1)
  def se = copy(x = x + 1, y = y + 1)
  def sw = copy(x = x - 1, y = y + 1)
  def nw = copy(x = x - 1, y = y - 1)
  def ne = copy(x = x + 1, y = y - 1)
  def surrounding = Set(e,s,n,w,se,sw,nw,ne)

def propose(elf: Point, elves: Set[Point], turn: Int): Point =
  val stayStill = Option.when(elf.surrounding.intersect(elves).isEmpty)(elf)

  val tryNorth = Option.when(Set(elf.n, elf.ne, elf.nw).intersect(elves).isEmpty)(elf.n)
  val trySouth = Option.when(Set(elf.s, elf.se, elf.sw).intersect(elves).isEmpty)(elf.s)
  val tryWest = Option.when(Set(elf.w, elf.nw, elf.sw).intersect(elves).isEmpty)(elf.w)
  val tryEast = Option.when(Set(elf.e, elf.ne, elf.se).intersect(elves).isEmpty)(elf.e)

  turn % 4 match
    case 0 => stayStill orElse tryNorth orElse trySouth orElse tryWest orElse tryEast getOrElse elf
    case 1 => stayStill orElse trySouth orElse tryWest orElse tryEast orElse tryNorth getOrElse elf
    case 2 => stayStill orElse tryWest orElse tryEast orElse tryNorth orElse trySouth getOrElse elf
    case 3 => stayStill orElse tryEast orElse tryNorth orElse trySouth orElse tryWest getOrElse elf

def step(elves: Set[Point], turn: Int): Set[Point] =
  val proposals = elves.toList
    .map(e => e -> propose(e, elves, turn))
    .filter(_ != _)

  val validProposals = proposals
    .groupBy((s, d) => d)
    .toList
    .collect {
      case (dest, proposals) if proposals.size == 1 =>
        proposals.head
    }

  val starts = validProposals.map(_._1).toSet
  val destinations = validProposals.map(_._2).toSet
  elves.diff(starts).union(destinations)

def rounds = LazyList.from(0).scanLeft(elvesStart) {
  (elves, turn) => step(elves, turn)
}

val roundTen = rounds(10)

val xRange = roundTen.map(_.x).min to roundTen.map(_.x).max
val yRange = roundTen.map(_.y).min to roundTen.map(_.y).max

val ans1 = xRange.size * yRange.size - roundTen.size

val ans2 = 1 + rounds.zip(rounds.tail).indexWhere(_ == _)
