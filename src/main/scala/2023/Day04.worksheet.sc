val input = io.Source.fromResource("2023/day-04.txt").getLines().toList

val cards: Map[Int, (Set[Int], Set[Int])] =
  val tups = input.map:
    case s"Card $n: $left | $right" =>
      val have = left.split(" +").filter(_.nonEmpty).map(_.toInt).toList
      val winning = right.split(" +").filter(_.nonEmpty).map(_.toInt).toList
      n.stripLeading.toInt -> (have.toSet, winning.toSet)
  tups.toMap

cards foreach println

val originalCounts = cards.view.mapValues(_ => 1).toMap

val counts = scala.collection.mutable.Map.empty[Int, Int]
cards.keysIterator.foreach(counts(_) = 1)

def cardsWon(n: Int) =
  val (have, winning) = cards(n)
  have.intersect(winning).size

def count(card: Int): Unit =
  val s = cardsWon(card)
  val newCards = (card + 1) to (card + s)
  for c <- newCards if cards.contains(c) do counts(c) += counts(card)

def score(n: Int) =
  val (have, winning) = cards(n)
  val matches = have.intersect(winning).size
  math.pow(2, matches - 1).toInt

val ans1 = cards.keysIterator.map(score).sum

val ans2 = counts.valuesIterator.sum
