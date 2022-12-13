val input = io.Source.fromResource("2022/day-13.txt").getLines().toVector

val rawPackets = input.grouped(3).map {
  ps => ps(0) -> ps(1)
}.toVector

sealed trait Packet:
  override def toString = this match
    case PList(packets) => packets.mkString("[",",","]")
    case Num(n) => n.toString

// object Packet:
//   given packetOrdering: Ordering[Packet] = Ordering.

case class PList(packets: List[Packet]) extends Packet

// object PList:
//   given pListOrdering: Ordering[PList] = Ordering.by[PList, List[Packet]](_.packets)

case class Num(n: Int) extends Packet

object Num:
  given numOrdering: Ordering[Num] = Ordering.by[Num, Int](_.n)

def sorted(left: Packet, right: Packet): Boolean = (left, right) match
  case (Num(l), Num(r)) => l <= r
  case (PList(ls), PList(rs)) =>
    def foo(left: List[Packet], right: List[Packet]): Boolean =
      (left, right) match
        case (l::ls, r::rs) if sorted(l, r) && sorted(r,l) => foo(ls, rs)
        case (l::ls, r::rs) => sorted(l,r)
        case (Nil, _) => true
        case (_, Nil) => false
    foo(ls, rs)
  case (PList(_), Num(r)) => sorted(left, PList(List(right)))
  case (Num(l), PList(_)) => sorted(PList(List(left)), right)

object D:
  def unapply(s: String): Option[Num] = s.toIntOption.map(Num(_))

def parseMulti(raw: String): List[Packet] = raw match
  case "" => Nil
  case s"${D(a)}" => List(a)
  case s"${D(a)},$otherPackets" => a :: parseMulti(otherPackets)
  case s"$otherPackets" =>
    val (leadList, rest) = splitMatching(otherPackets)
    parse(leadList) :: parseMulti(rest.tail)

def parse(raw: String): Packet = raw match
  case s"[${D(a)},${packets}]" => PList(a :: parseMulti(packets))
  case s"[$inner]" => PList(parseMulti(inner))

def splitMatching(str: String, from: Int = 0): (String, String) =
  val counts = str.scanLeft((0, 0)) {
    case ((open, close), '[') => (open + 1, close)
    case ((open, close), ']') => (open, close + 1)
    case ((open, close), c)   => (open, close)
  }
  val balanced = counts.indexWhere(_ == _, from = 1)
  str.splitAt(balanced)

val packets = rawPackets.map[(Packet, Packet)] {
  case (l, r) => parse(l) -> parse(r)
}

packets.zipWithIndex.collect {
  case ((left, right), i) if sorted(left, right) => i + 1
}

packets.filter(sorted).foreach {
  case (left, right) =>
    println(left)
    println(right)
    println()
}

packets.count(sorted)
input.size
packets.size

val ans1 = packets.zipWithIndex.collect {
  case ((left, right), i) if sorted(left, right) => i + 1
}.sum


given packetOrdering: Ordering[Packet] =
  Ordering.fromLessThan[Packet]((l, r) => sorted(l,r) && !sorted(r, l))

val start = parse("[[2]]")
val end = parse("[[6]]")

val allPackets = (packets.appended(start -> end)).flatMap {
  case (left, right) => Vector(left, right)
}.sorted

val si = allPackets.indexOf(start) + 1
val ei = allPackets.indexOf(end) + 1
val ans2 = si * ei

// tests

rawPackets.find { // PARSING WORKS!
  case (l, r) => parse(l).toString != l || parse(r).toString != r
}


sorted(parse("[2]"), parse("[2]"))
sorted(parse("[2,3,4]"), parse("[2]"))
sorted(parse("[2]"), parse("[2,3,4]"))
sorted(parse("[2,3,4]"), parse("[4]"))

parseMulti("[5],3")
parseMulti("[5]")
parseMulti("[5],[4]")
parseMulti("5,4,3")

parse("[3,4,5]")
parse("[3,[[4,5],6]]")



//
