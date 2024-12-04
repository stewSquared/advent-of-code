val input = io.Source.fromResource("2024/day-03.txt").getLines().toList

val mulRegex = "mul\\(\\d+,\\d+\\)".r

val ans1 = mulRegex.findAllIn(input.mkString).map:
  case s"mul($x,$y)" => x.toInt * y.toInt
.sum

val doRegex = "do\\(\\)".r
val dontRegex = "don't\\(\\)".r
val instRegex = s"(${mulRegex}|${doRegex}|${dontRegex})".r

val (ans2, _) = instRegex.findAllIn(input.mkString).foldLeft(0, true):
  case ((sum, enabled), instruction) =>
    instruction match
      case "do()" => (sum, true)
      case "don't()" => (sum, false)
      case s"mul($x,$y)" if enabled => (sum + x.toInt * y.toInt, enabled)
      case _ => (sum, enabled)
