val input = io.Source.fromResource("2020/day-09.txt")
  .getLines.map(_.toLong).toIndexedSeq

// val preamble = input.take(25)
// val encrypted = input.drop(25)

def valid(preamble: Seq[Long]): Set[Long] =
  preamble.combinations(2).map(_.sum).toSet

val ans1 = input.sliding(26).collectFirst {
  case ns: IndexedSeq[Long] if !valid(ns.init).contains(ns.last) => ns.last
}

val target = ans1.get

def findContiguous(from: Int, until: Int): Seq[Long] =
  val slice = input.slice(from, until)
  if slice.sum < target then findContiguous(from, until + 1)
  else if slice.sum > target then findContiguous(from + 1, until)
  else slice

val ans2 =
  val range = findContiguous(0, 2)
  range.min + range.max
