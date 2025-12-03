val input = io.Source.fromResource("2025/day-01.txt").getLines().toList

val rotations = input.collect:
  case s"R$n" => n.toInt
  case s"L$n" => -n.toInt

val (positions, clicks) = rotations.scanLeft(50, 0):
  case ((pos, _), rot) =>
    val next = pos + rot
    val zeroClick = pos != 0 && next <= 0
    val clicks = next.abs / 100 + (if zeroClick then 1 else 0)

    (math.floorMod(next, 100), clicks)
.unzip

val ans1 = positions.count(_ == 0)
val ans2 = clicks.sum
