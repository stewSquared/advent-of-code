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

val elementCounts: LazyList[Iterable[Long]] =
  LazyList.iterate(start)(step).map { pairCounts =>
    pairCounts.foldLeft(Map(template.head -> 1L).withDefaultValue(0L)) {
      case (counting, ((_, r), count)) =>
        counting.updated(r, counting(r) + count)
    }.values
  }

val ans1 = elementCounts(10).max - elementCounts(10).min
val ans2 = elementCounts(40).max - elementCounts(40).min
