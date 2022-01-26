
// val expenses = List(1721, 979, 366, 299, 675, 1456)
val expenses = io.Source.fromResource("2020/day-01-1.txt").getLines.map(_.toInt).toList

expenses.size

val ans1 = expenses.sorted.combinations(2).collectFirst {
  case xs if xs.sum == 2020 => xs.product
}

val ans2 = expenses.sorted.combinations(3).collectFirst {
  case xs if xs.sum == 2020 => xs.product
}
