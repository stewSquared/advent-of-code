val input = io.Source.fromResource("2015/day-13.txt").getLines().toList

val prefs = Map.from[(String, String), Int]:
  input.collect:
    case s"$alice would $gain $unit happiness units by sitting next to $bob." =>
      (alice, bob) -> (if gain == "gain" then unit.toInt else -unit.toInt)
.withDefaultValue(0)

val names = prefs.keys.map(_._1)

def happiness(seats: List[String]): Int =
  seats.zip(seats.last :: seats).map:
    case (a, b) => prefs(a,b) + prefs(b,a)
  .sum

val ans1 = names.toList.permutations.map(happiness).max
val ans2 = ("stew" :: names.toList).permutations.map(happiness).max
