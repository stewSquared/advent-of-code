import scala.collection.immutable.{Range => _}

type Range = scala.collection.immutable.NumericRange[Long]

val input = io.Source.fromResource("2023/day-05.txt").getLines()

val seeds: List[Range] =
  val s"seeds: $seedsStr" = input.next()
  val nums = seedsStr.split(" ").map(_.toLong).toList
  nums.grouped(2).toList.map:
    case List(a, b) => a until (a + b)

seeds foreach println

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

def supersetOf(l: Range, r: Range) = l.min <= r.min && r.max <= l.max

def intersect(l: Range, r: Range): Option[Range] =
  if supersetOf(l, r) then Some(r)
  else if r contains l.min then Some(l.min to l.max.min(r.max))
  else if r contains l.max then Some(l.min.max(r.min) to l.max)
  else None

intersect(79L to 93L, 50L to 98L)
intersect(79L to 93L, 150L to 198L)
intersect(79L to 93L, -150L to 198L)
intersect(79L to 93L, -150L to 85L)
intersect(79L to 93L, 85L to 100L)

def applyRange(c: Range, d: Range, s: Range): Range =
  val i = intersect(c, s).get
  val diff = d.min - s.min
  i.min + diff to i.max + diff

applyRange(79L to 93L, 52L until 100L, 50L to 98L)

def diff(m: Range, n: Range): List[Range] =
  if m.min < n.min && n.max < m.max then
    List(m.min to n.min - 1, n.max + 1 to m.max)
  else if n.min <= m.min && m.max <= n.max then Nil
  else if n contains m.max then List(m.min to n.min - 1)
  else if n contains m.min then List(n.max + 1 to m.max)
  else List(m)

diff(1L to 10L, 8L to 12L)
diff(1L to 10L, 3L to 5L)
diff(1L to 10L, -5L to 5L)
diff(1L to 10L, -5L to 15L)
diff(1L to 10L, 15L to 25L)

def next(current: List[Range], ranges: List[(Range, Range)]): List[Range] =
  current.flatMap: c =>
    val overlaps = ranges.filter: (_, s) =>
      s.contains(c.max) || s.contains(c.min)

    println(s"overlaps for $c: $overlaps")

    val overlapTranslations = overlaps.map(applyRange(c, _, _))

    overlapTranslations.foreach: nc =>
      print("-  translations:")
      println(nc)

    val remaining = overlaps.map(_._2).foldLeft(List(c)): (ranges, s) =>
      ranges.flatMap(diff(_, s))

    overlapTranslations ++ remaining

var current = seeds

// input.next()
// next(seeds, extractRanges())

val its = Iterator.iterate(seeds): current =>
  println(current)
  println(input.next())
  next(current, extractRanges())

val finalRanges = its.take(8).toList.last
val ans2 = finalRanges.map(_.min).min
