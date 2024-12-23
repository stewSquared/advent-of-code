import util.chaining.*

val input = io.Source.fromResource("2024/day-23.txt").getLines.toList

val adj: Map[String, List[String]] = input.collect:
  case s"$a-$b" => List(a -> b, b -> a)
.flatten
.groupMap(_._1)(_._2)
.withDefaultValue(Nil)

adj foreach println

adj.values.toList.map(_.size).sorted.reverse

val adjSet: Map[String, Set[String]] =
  adj.view.mapValues(_.toSet).toMap.withDefaultValue(Set.empty)

val trips: List[List[String]] = adj.toList.flatMap:
  case (a, vs) => vs.combinations(2).collect:
    case List(b, c) if adj(b).contains(c) => List(a,b,c)

val ans1 = trips.count(_.exists(_.startsWith("t")))

def connected12(nodes: List[String]): Boolean =
  nodes.combinations(12).exists: vs =>
    vs.combinations(2).forall:
      case List(a, b) => adjSet(a)(b)

val ans2 = adj.collect:
  case (k, vs) if connected12(vs) => k
.toList.sorted.mkString(",")
