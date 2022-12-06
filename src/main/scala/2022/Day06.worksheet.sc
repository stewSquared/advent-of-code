val msg = io.Source.fromResource("2022/day-06.txt").getLines().mkString

val ans1 = 4 + msg.sliding(4).indexWhere { g =>
  g.distinct.size == g.size
}

val ans2 = 14 + msg.sliding(14).indexWhere { g =>
  g.distinct.size == g.size
}
