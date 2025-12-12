val input = io.Source.fromResource("2025/day-12.txt").getLines

val lowerBound = input.collect:
  case s"${w}x${l}: $counts" =>
    val spots = (w.toInt/3) * (l.toInt/3)
    spots -> counts.split(' ').map(_.toInt).toList
.count:
  case (spots, counts) => counts.sum <= spots

val ans1 = lowerBound
