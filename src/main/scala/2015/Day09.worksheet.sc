val input = io.Source.fromResource("2015/day-09.txt").getLines().toList

val distance = Map.from[(String, String), Int]:
  input.flatMap:
    case s"$a to $b = $dist" =>
      List((a, b) -> dist.toInt, (b, a) -> dist.toInt)

val cities = distance.keys.flatMap:
  case (a, b) => List(a, b)
.toList

val ans1 = cities.permutations.map: p =>
  p.sliding(2).map:
    case List(a, b) => distance(a -> b)
  .sum
.max

//
