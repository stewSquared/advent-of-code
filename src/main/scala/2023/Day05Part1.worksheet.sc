import scala.compiletime.ops.int
import scala.collection.immutable.{Range => _}

type Range = scala.collection.immutable.NumericRange[Long]

val input = io.Source.fromResource("2023/day-05.txt").getLines()

val seeds =
  val s"seeds: $seedsStr" = input.next()
  seedsStr.split(" ").map(_.toLong).toList

input.next()

def extractRanges(): List[(Range, Range)] =
  val lines = input.takeWhile(_.nonEmpty)
  lines.map {
    case s"$a $b $c" =>
      val dest = a.toLong
      val source = b.toLong
      val length = c.toLong
      (dest until dest + length, source until source + length)
  }.toList

def next(current: List[Long], ranges: List[(Range, Range)]): List[Long] =
  current.map: c =>
    ranges.find(_._2.contains(c)).fold(c): (d, s) =>
      println(s"found $c in $s going to $d")
      // d.min + (c - s.min)
      c + (d.min - s.min)

var current = seeds

println(current)
input.next()
current = next(current, extractRanges())
println(current)
input.next()
current = next(current, extractRanges())
println(current)
input.next()
current = next(current, extractRanges())
println(current)
input.next()
current = next(current, extractRanges())
println(current)
input.next()
current = next(current, extractRanges())
println(current)
input.next()
current = next(current, extractRanges())
println(current)
input.next()
val locations = next(current, extractRanges())

val ans1 = locations.min
