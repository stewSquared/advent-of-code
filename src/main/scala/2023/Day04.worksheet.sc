val input = io.Source.fromResource("2023/day-04.txt").getLines().toList

val matches: Map[Int, Set[Int]] =
  val tups = input.map:
    case s"Card $n: $left | $right" =>
      val have = left.split(" +").filter(_.nonEmpty).map(_.toInt).toSet
      val winning = right.split(" +").filter(_.nonEmpty).map(_.toInt).toSet
      n.stripLeading.toInt -> have.intersect(winning)
  tups.toMap

def score(n: Int) =
  math.pow(2, matches(n).size - 1).toInt

val ans1 = matches.keysIterator.map(score).sum

val counts = scala.collection.mutable.Map.empty[Int, Int]
matches.keysIterator.foreach(counts(_) = 1)

for
  n <- 1 to matches.size
  newCards = (n + 1) to (n + matches(n).size)
  c <- newCards if matches.contains(c)
do
  counts(c) += counts(n)

val ans2 = counts.valuesIterator.sum
