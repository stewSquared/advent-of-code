import util.Using

val input = io.Source.fromResource("2020/day-07.txt").getLines.toList

val adj = input.map {
  case s"$outerColor bags contain no other bags" => outerColor -> Nil
  case s"$outerColor bags contain $innerBags" =>
    val innerColors = innerBags.split(", ").collect {
      case s"$n $color bag$plural" => color
    }
    outerColor -> innerColors.toList
}.toMap[String, List[String]]

adj foreach println

val rev = adj.toList
  .flatMap {case (outer, inners) => inners.map(_ -> outer)}
  .groupMap(_._1)(_._2)

rev foreach println

// rev("shiny gold")

def canContain(inner: String): Set[String] =
  rev.get(inner).map {
    direct =>
      direct.toSet union direct.toSet.flatMap(canContain)
  }.getOrElse(Set.empty)

val ans1 = canContain("shiny gold").size

val adj2: Map[String, List[(Long, String)]] = input.map {
  case s"$outerColor bags contain no other bags." => outerColor -> Nil
  case s"$outerColor bags contain $innerBags." =>
    val innerColors = innerBags.split(", ").collect {
      case s"1 $color bag" => 1L -> color
      case s"$n $color bags" => n.toLong -> color
    }
    outerColor -> innerColors.toList
}.toMap

def containsCount(outer: String): Long =
  println(s"checking $outer")
  adj2(outer).map {
    case (n, inner) => n + n * containsCount(inner)
  }.sum

val ans2 = containsCount("shiny gold")
