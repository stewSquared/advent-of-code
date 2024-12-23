import util.chaining.*

val input = io.Source.fromResource("2024/day-23.txt").getLines.toList


val adj: Map[String, List[String]] = input.collect:
  case s"$a-$b" => if a < b then a -> b else b -> a
.groupMap(_._1)(_._2)
.withDefaultValue(Nil)

val trips: List[List[String]] = adj.toList.flatMap:
  case (a, vs) => vs.combinations(2).filter:
    case List(b, c) => adj(b).contains(c) || adj(c).contains(b)
  .map(a :: _)

val ans1 = trips.count(_.exists(_.startsWith("t")))


//
