val input = io.Source.fromResource("2023/day-04.txt").getLines().toList

val cards: Map[Int, (Set[Int], Set[Int])] =
  val tups = input.map:
    case s"Card   $n: $left | $right" =>
      val have = left.split(" +").filter(_.nonEmpty).map(_.toInt).toList
      val winning = right.split(" +").filter(_.nonEmpty).map(_.toInt).toList
      n.toInt -> (have.toSet, winning.toSet)
    case s"Card  $n: $left | $right" =>
      val have = left.split(" +").filter(_.nonEmpty).map(_.toInt).toList
      val winning = right.split(" +").filter(_.nonEmpty).map(_.toInt).toList
      n.toInt -> (have.toSet, winning.toSet)
    case s"Card $n: $left | $right" =>
      val have = left.split(" +").filter(_.nonEmpty).map(_.toInt).toList
      val winning = right.split(" +").filter(_.nonEmpty).map(_.toInt).toList
      n.toInt -> (have.toSet, winning.toSet)
  tups.toMap

cards foreach println

val originalCounts = cards.view.mapValues(_ => 1).toMap

val scoreMemo = scala.collection.mutable.Map.empty[Int, Int]
val countMemo = scala.collection.mutable.Map.empty[Int, Int]
cards.keysIterator.foreach(countMemo(_) = 1)

def count(card: Int) =
  val s = cardsWon(card)
  val newCards = (card + 1) to (card + s)
  println(s"winning ${newCards.toList}")
  for c <- newCards if cards.contains(c) do countMemo(c) += countMemo(card)

cards.keysIterator.toList.sorted.foreach(count)

countMemo foreach println

oldScore(1)
oldScore(2)
oldScore(3)
oldScore(4)
oldScore(5)
oldScore(6)

def cardsWon(n: Int) =
  val (have, winning) = cards(n)
  have.intersect(winning).size

def oldScore(n: Int) =
  val (have, winning) = cards(n)
  val matches = have.intersect(winning).size
  math.pow(2, matches - 1).toInt

  // scoreMemo.getOrElseUpdate(n, {
  //   val (have, winning) = cards(n)
  //   val matches = have.intersect(winning).size
  //   math.pow(2, matches - 1).toInt
  // }
  // )

val ans1 = cards.keysIterator.map(oldScore).sum

val ans2 = countMemo.valuesIterator.sum

//
