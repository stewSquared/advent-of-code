val input = io.Source.fromResource("2020/day-02.txt").getLines.toList

val ans = input.count {
  case s"$low-$high $char: $password" =>
    (low.toInt to high.toInt).contains(password.count(_ == char.head))
}

val ans2 = input.count {
  case s"$a-$b $char: $password" =>
    val indices = List(a, b).map(_.toInt - 1)
    indices.count(password.charAt(_) == char.head) == 1
}
