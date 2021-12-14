import util.chaining.*

val input = io.Source.fromResource("2021/day-14-1.txt").getLines.toList

type Pair = (Char, Char)

val template = input.head
val insertionRules: Map[Pair, Char] =
  input.collect { case s"$pair -> $c" => (pair(0), pair(1)) -> c.head }.toMap

val start: Map[Pair, Long] =
  Map.from(template.zip(template.tail).groupMapReduce(identity)(_ => 1L)(_ + _))

def step(pairCounts: Map[Pair, Long]): Map[Pair, Long] =
  pairCounts.foldLeft(Map.empty.withDefaultValue(0L)) {
    case (counting, ((l, r), lrCount)) =>
      val n = insertionRules(l -> r)
      counting
        .updated(l -> n, counting(l -> n) + lrCount)
        .updated(n -> r, counting(n -> r) + lrCount)
  }

def elementCounts(pairCounts: Map[Pair, Long]): Map[Char, Long] =
  pairCounts.foldLeft(Map(template.head -> 1L).withDefaultValue(0L)) {
    case (counting, ((_, r), count)) =>
      counting.updated(r, counting(r) + count)
  }

val quantityDiff = LazyList
  .iterate(start)(step)
  .map(elementCounts(_).values.pipe(c => c.max - c.min))

val ans1 = quantityDiff(10)
val ans2 = quantityDiff(40)
