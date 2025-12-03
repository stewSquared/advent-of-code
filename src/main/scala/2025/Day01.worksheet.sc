val input = io.Source.fromResource("2025/day-01.txt").getLines().toList

val (positions, clicks) = input.scanLeft(50, 0):
  case ((pos, _), rot) =>
    val sign = if rot.head == 'R' then 1 else -1
    val next = pos + rot.tail.toInt * sign
    val zeroClick = pos != 0 && next <= 0
    val clicks = (next / 100).abs + (if zeroClick then 1 else 0)

    (math.floorMod(next, 100), clicks)
.unzip

val ans1 = positions.count(_ == 0)
val ans2 = clicks.sum
