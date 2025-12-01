val input = io.Source.fromResource("2025/day-01.txt").getLines().toList

val (positions, counts) = input.scanLeft(50, 0):
  case ((pos, count), n) =>
    val delta = n match
      case s"L$d" => -d.toInt
      case s"R$d" => d.toInt
    val next = pos + delta
    val clicks =
      if pos == 0 && next < 0 then next / -100
      else if next <= 0 then next / -100 + 1
      else if next >= 100 then next / 100
      else 0
    val normalized = (next + 10000) % 100

    (normalized, count + clicks)
.unzip

val ans1 = positions.count(_ == 0)

val ans2 = counts.last
