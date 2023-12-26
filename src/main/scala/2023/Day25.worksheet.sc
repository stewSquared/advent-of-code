val input = io.Source.fromResource("2023/day-25.txt").getLines.toList

val adj: Map[String, Set[String]] =
  val edges = input.toSet.flatMap:
    case s"$c: $cs" =>
      cs.split(" ").flatMap(c2 => Set(c -> c2, c2 -> c))
  edges.groupMap(_._1)(_._2)

def search(connected: Set[String], reachable: Set[String]): Set[String] =
  if reachable.size == 3 then connected else
    val (next, newOptions) = reachable.iterator.map: n =>
      n -> adj(n).diff(connected)
    .minBy(_._2.size)
    search(connected + next, (reachable - next) union newOptions)

val ans1 =
  val s = search(Set.empty, Set("klg"))
  s.size * (adj.size - s.size)
