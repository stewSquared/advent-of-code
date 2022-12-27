import io.Source
import util.Using
import util.chaining.*

val entries = Using(Source.fromResource("2021/day-08-1.txt")) {
  _.getLines.map(Entry.parse).toList
}.get

enum Segment(val numbers: Set[Int]):
  case Top extends Segment(Set(0,  2,3,  5,6,7,8,9))  // 8
  case Tl  extends Segment(Set(0,      4,5,6,  8,9))  // 6*
  case Tr  extends Segment(Set(0,1,2,3,4,    7,8,9))  // 8
  case Mid extends Segment(Set(    2,3,4,5,6,  8,9))  // 7
  case Bl  extends Segment(Set(0,  2,      6,  8))    // 4*
  case Br  extends Segment(Set(0,1,  3,4,5,6,7,8,9))  // 9*
  case Bot extends Segment(Set(0,  2,3,  5,6,  8,9))  // 7
  // [*] segment uniquely identified by size

import Segment.*

case class Entry(patterns: List[String], output: List[String]):
  require(patterns.distinct.size == 10)
  def solve: Int = {
    output.map { signal =>
      val counts = signal.map(c => patterns.count(_.contains(c)))
      signal.size match {
        case 2 => 1
        case 3 => 7
        case 4 => 4
        case 7 => 8
        case 5 => // 2 3 5
          if counts.contains(Bl.numbers.size) then 2
          else if counts.contains(Tl.numbers.size) then 5
          else 3
        case 6 => // 0 6 9
          if counts.count(_ == Mid.numbers.size) == 1 then 0 // bot appears as often as mid
          else if counts.contains(Bl.numbers.size) then 6
          else 9
      }
    }.mkString.toInt
  }

object Entry:
  def parse(raw: String): Entry =
    raw.split('|') match {
      case Array(p, o) => Entry(
        patterns = p.split(' ').filter(_.nonEmpty).toList,
        output = o.split(' ').filter(_.nonEmpty).toList
      )
    }

val ans1 = entries.map(_.solve).mkString.count("1478".contains)
val ans2 = entries.map(_.solve).sum
