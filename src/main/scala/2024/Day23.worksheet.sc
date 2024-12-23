val input = io.Source.fromResource("2024/day-23.txt").getLines.toList

val adj: Map[String, List[String]] = input.flatMap:
  case s"$a-$b" => List(a -> b, b -> a)
.groupMap(_._1)(_._2)
.withDefaultValue(Nil)

val trips: List[List[String]] = adj.toList.flatMap:
  case (a, vs) => vs.combinations(2).collect:
    case List(b, c) if adj(b).contains(c) => List(a,b,c)

val ans1 = trips.count(_.exists(_.startsWith("t")))

def connected12(nodes: List[String]): Option[List[String]] =
  nodes.combinations(12).find: vs =>
    vs.combinations(2).forall:
      case List(a, b) => adj(a).contains(b)

val ans2 = adj.view.mapValues(connected12).collectFirst:
  case (k, Some(vs)) => (k::vs).sorted.mkString(",")
.get
