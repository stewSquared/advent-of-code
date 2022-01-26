val input = io.Source.fromResource("2020/day-10.txt").getLines.toList

val joltages = input.map(_.toInt).sorted

val adjPairs = joltages.zip(joltages.tail)
val oneDifferences = adjPairs.count(_ + 1 == _)
val threeDifferences = adjPairs.count(_ + 3 == _)

val ans1 = (oneDifferences + 1) * (threeDifferences + 1)

joltages.size

val memo = collection.mutable.Map[(Int, List[Int]), Long]()

def arrangements(current: Int, remaining: List[Int]): Long =
  memo.getOrElseUpdate(
    (current, remaining),
    remaining match
      case last :: Nil if last <= current + 3 => 1L
      case j :: js if j <= current + 3 =>
        arrangements(current, js) + arrangements(j, js)
      case _ => 0L
  )

val ans2 = arrangements(0, joltages)
