val input = io.Source.fromResource("2020/day-05.txt").getLines.toList

val ids = input.map {
  seat =>
    val row =
      val bin = seat.take(7).map(c => if c == 'F' then '0' else '1')
      Integer.parseInt(bin, 2)
    val col =
      val bin = seat.drop(7).map(c => if c == 'R' then '1' else '0')
      Integer.parseInt(bin, 2)
    row * 8 + col
}

val ans = ids.max

ids.size

val ans2 = ids.sorted
  .dropWhile(_ < 8)
  .sliding(2)
  .collectFirst {
    case Seq(l, r) if l + 2 == r => l + 1
  }
