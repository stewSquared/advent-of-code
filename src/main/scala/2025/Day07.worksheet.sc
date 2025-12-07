val input = io.Source.fromResource("2025/day-07.txt").getLines().toList

val start = input.head.indexOf("S")

val (ans1, finalBeams) = input.tail.foldLeft(0, Map(start -> 1L).withDefaultValue(0L)):
  case ((splitCount, beams), line) =>
    val splits = beams.filter:
      case (i, c) => line(i) == '^'
    val newBeams = splits.foldLeft(beams):
      case (acc, (i, c)) =>
        acc.removed(i)
          .updated(i + 1, acc(i + 1) + c)
          .updated(i - 1, acc(i - 1) + c)

    (splitCount + splits.size, newBeams)

val ans2 = finalBeams.values.sum
