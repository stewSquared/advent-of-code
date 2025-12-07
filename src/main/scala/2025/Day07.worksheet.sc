val input = io.Source.fromResource("2025/day-07.txt").getLines().toList

val start = input.head.indexOf("S")

val beamStates = input.tail.scanLeft(Map(start -> 1L).withDefaultValue(0L)):
  case (beams, line) =>
    beams.foldLeft(beams):
      case (acc, (i, c)) if line(i) == '^' =>
        acc.removed(i)
          .updated(i + 1, acc(i + 1) + c)
          .updated(i - 1, acc(i - 1) + c)
      case (acc, _) => acc

val ans1 = beamStates.zip(input.drop(2)).map:
  case (beams, line) => beams.keys.count(line(_) == '^')
.sum

val ans2 = beamStates.last.values.sum
