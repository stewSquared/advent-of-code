val input = io.Source.fromResource("2025/day-01.txt").getLines().toList

val (positions, counts) = input.scanLeft(50, 0):
  case ((pos, count), s"R$d") =>
    val next = pos + d.toInt
    (math.floorMod(next, 100), count + next / 100)
  case ((pos, count), s"L$d") =>
    val next = pos - d.toInt
    val clicks =
      if pos == 0 then next / -100
      else if next <= 0 then next / -100 + 1
      else 0

    (math.floorMod(next, 100), count + clicks)
.unzip

val ans1 = positions.count(_ == 0)

val ans2 = counts.last
