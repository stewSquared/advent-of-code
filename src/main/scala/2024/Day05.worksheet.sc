val input = io.Source.fromResource("2024/day-05.txt").getLines.toList

val pageOrderingRules = input.collect:
  case s"$before|$after" => before.toInt -> after.toInt

val dependencies: Map[Int, Set[Int]] =
  pageOrderingRules
    .groupMap(_._2)(_._1)
    .view.mapValues(_.toSet)
    .toMap.withDefaultValue(Set.empty)

val updates = input.collect:
  case line if line.contains(",") =>
    line.split(",").map(_.toInt).toList

def correctOrder(pages: List[Int]) =
  pages.reverse.tails.forall:
    case Nil => true
    case page :: before =>
      before.forall(dependencies(page))

def middlePage(pages: List[Int]) =
  pages(pages.size / 2)

val ans1 = updates.filter(correctOrder).map(middlePage).sum

val ans2 = updates.filterNot(correctOrder)
  .map(_.sortWith((b, a) => dependencies(a).contains(b)))
  .map(middlePage)
  .sum
