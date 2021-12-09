import io.Source
import util.Using
import util.chaining.*

val entries = Using(Source.fromResource("2021/day-08-1.txt")) {
  _.getLines.map(Entry.parse).toList
}.get

val orientation: Map[String, Set[Int]] = Map(
  "top" -> Set(0,  2,3,  5,6,7,8,9), // 8
  "tl"  -> Set(0,      4,5,6,  8,9), // 6*
  "tr"  -> Set(0,1,2,3,4,    7,8,9), // 8
  "mid" -> Set(    2,3,4,5,6,  8,9), // 7
  "bl"  -> Set(0,  2,      6,  8),   // 4*
  "br"  -> Set(0,1,  3,4,5,6,7,8,9), // 9*
  "bot" -> Set(0,  2,3,  5,6,  8,9)  // 7
  // [*] segment uniquely identified by size
)

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
          if counts.contains(orientation("bl").size) then 2
          else if counts.contains(orientation("tl").size) then 5
          else 3
        case 6 => // 0 6 9
          if counts.count(_ == orientation("mid").size) == 1 then 0 // bot appears as often as mid
          else if counts.contains(orientation("bl").size) then 6
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

val ans2 = entries.map(_.solve).sum




val possibleDigitsByCount: Map[Int, Set[Int]] = Map(
  8 -> (orientation("top") union orientation("tr"))
)

val digitSegments: Map[Int, Int] = Map(
  0 -> 5,
  1 -> 2,
  2 -> 5,
  3 -> 5,
  4 -> 4,
  5 -> 5,
  6 -> 6,
  7 -> 3,
  8 -> 7,
  9 -> 6
)

val possibleDigitsBySize: Map[Int, Set[Int]] = Map(
  2 -> Set(1),
  3 -> Set(7),
  4 -> Set(4),
  5 -> Set(0,2,3,5),
  6 -> Set(6,9),
  7 -> Set(8)
)

// tests
val exampleEntry = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
assert(Entry.parse(exampleEntry).solve == 5353)

orientation("tr")
  .intersect(orientation("br"))
  .intersect(possibleDigitsBySize(5))

orientation("tr")
  .intersect(orientation("br"))
  .intersect(possibleDigitsBySize(6))
