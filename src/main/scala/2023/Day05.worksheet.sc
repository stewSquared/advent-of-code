import annotation.unchecked

type Range = aoc.Interval[Long]
val Range = aoc.Interval
case class ConversionMap(name: String, ranges: List[(Range, Range)])

val input = io.Source.fromResource("2023/day-05.txt").getLines()

val rawSeeds = input.next()

val seeds1: List[Range] =
  val s"seeds: $seedsStr" = rawSeeds
  val nums = seedsStr.split(" ").map(_.toLong).toList
  nums.map(n => Range(n, n))

val seeds2: List[Range] =
  val s"seeds: $seedsStr" = rawSeeds
  val nums = seedsStr.split(" ").map(_.toLong).toList
  nums.grouped(2).toList.collect:
    case List(a, b) => Range(a until a + b)

input.next()

val conversionMaps =
  val lb = collection.mutable.ListBuffer[ConversionMap]()
  while input.nonEmpty do
    val name = input.next()
    val lines = input.takeWhile(_.nonEmpty)
    val ranges = lines.map:
      case s"$a $b $c" =>
        val dest = a.toLong
        val source = b.toLong
        val length = c.toLong
        (Range(dest until dest + length), Range(source until source + length))
    lb += ConversionMap(name, ranges.toList)
  lb.result()

conversionMaps.map(_.name) foreach println

def translate(c: Range, d: Range, s: Range): Range =
  val i = c.intersect(s).get
  val delta = d.min - s.min
  Range(i.min + delta, i.max + delta)

def next(current: List[Range], conversionMap: ConversionMap): List[Range] =
  import conversionMap.ranges
  current.flatMap: c =>
    val overlapping = ranges.filter:
      case (_, s) => s.contains(c.max) || s.contains(c.min)

    val translations = overlapping.map(translate(c, _, _))

    val remainders = overlapping.foldLeft(List(c)):
      case (ranges, (_, s)) => ranges.flatMap(_.diff(s))

    translations ++ remainders

val locations1 = conversionMaps.foldLeft(seeds1)(next)
val ans1 = locations1.map(_.min).min

val locations2 = conversionMaps.foldLeft(seeds2)(next)
val ans2 = locations2.map(_.min).min
