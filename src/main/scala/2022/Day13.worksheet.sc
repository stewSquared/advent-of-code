import math.Ordering.Implicits._

val input = io.Source.fromResource("2022/day-13.txt").getLines().toVector

enum Packet:
  case PList(packets: Packet*)
  case Num(n: Int)

  override def toString = this match
    case PList(packets*) => packets.mkString("[", ",", "]")
    case Num(n)          => n.toString

object Packet:
  def parse(raw: String): Packet = raw match
    case s"[$packets]" => PList(parseMulti(packets)*)
    case num           => Num(num.toInt)

  def parseMulti(commaSeparated: String): List[Packet] = commaSeparated match
    case s if s.startsWith("[") =>
      splitMatching(s) match
        case (list, s",$packets") => parse(list) :: parseMulti(packets)
        case (list, _)            => parse(list) :: Nil
    case ""               => Nil
    case s"$num,$packets" => parse(num) :: parseMulti(packets)
    case num              => parse(num) :: Nil

  def splitMatching(str: String): (String, String) =
    val depths = str.iterator.scanLeft(0) {
      case (depth, '[') => depth + 1
      case (depth, ']') => depth - 1
      case (depth, _)   => depth
    }
    val balanced = depths.indexOf(0, from = 1)
    str.splitAt(balanced)

  given Ordering[Packet] with
    def compare(left: Packet, right: Packet): Int = (left, right) match
      case (Num(l), Num(r)) => l - r
      case (PList(l, ls*), PList(r, rs*)) =>
        val compareHead = compare(l, r)
        if compareHead != 0 then compareHead
        else compare(PList(ls*), PList(rs*))
      case (PList(ls*), PList(rs*)) => ls.size - rs.size
      case (PList(_*), Num(r))      => compare(left, PList(right))
      case (Num(l), PList(_*))      => compare(PList(left), right)

import Packet.{PList, Num, parse}

val packets = input
  .sliding(2, 3)
  .collect { case Vector(l, r) => (parse(l), parse(r)) }
  .toVector

val ans1 = packets.zipWithIndex.collect {
  case ((left, right), i) if left <= right => i + 1
}.sum

val d1 = parse("[[2]]")
val d2 = parse("[[6]]")
val allPackets = (packets.appended(d1 -> d2)).flatMap(_.toList).sorted

val i1 = allPackets.indexOf(d1) + 1
val i2 = allPackets.indexOf(d2) + 1
val ans2 = i1 * i2
