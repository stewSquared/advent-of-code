val input = io.Source.fromResource("2024/day-05.txt").getLines.toList

val pageOrderingRules = input.collect:
  case s"$before|$after" => before.toInt -> after.toInt

val dependencies: Map[Int, Set[Int]] =
  pageOrderingRules.map(_.swap)
    .groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap.withDefaultValue(Set.empty)

val updates = input.collect:
  case line if line.contains(",") =>
    line.split(",").map(_.toInt).toList

def correctOrder(pages: List[Int]) =
  pages.reverse.tails.forall:
    case Nil => true
    case page :: before =>
      before.forall(dependencies(page))

def middlePage(pages: List[Int]) =
  assert(pages.size % 2 == 1)
  pages(pages.size / 2)

val ans1 = updates.filter(correctOrder).map(middlePage).sum

def reSort(pages: List[Int]): List[Int] =
  pages.sorted:
    Ordering.fromLessThan: (a, b) =>
      dependencies(b).contains(a)

val ans2 = updates.filterNot(correctOrder)
  .map(reSort)
  .map(middlePage)
  .sum
