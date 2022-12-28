val input = io.Source.fromResource("2022/day-16.txt").getLines().toList

input.size

val flowRate: Map[String, Int] = input.collect {
  case s"Valve $valve has flow rate=$rate; $_" =>
    valve -> rate.toInt
}.toMap

val adj: Map[String, List[String]] = input.collect {
  case s"Valve $valve has $_ to valve$_ $valves" =>
    valve -> valves.split(", ").toList
}.toMap

val majorValves = adj.keySet.filter(v => flowRate(v) > 0 || v == "AA")

def distances(start: String): Map[String, Int] =
  val floodFill = Iterator.unfold(Set.empty[String], Set(start)) {
    (visited, visiting) =>
      Option.when(visiting.nonEmpty) {
        val next = visiting.flatMap(adj).diff(visited)
        visiting -> (visited ++ visiting, next)
      }
  }
  floodFill.zipWithIndex.flatMap { (valves, distance) =>
    valves.intersect(majorValves).map(_ -> distance)
  }.toMap

val costs = majorValves.map(v => v -> distances(v)).toMap

def maxPressure(
    valve: String,
    time: Int,
    pressure: Int,
    opened: Set[String],
    indent: Int = 0
): Int =
  val paths = costs(valve).collect {
      case (dest, cost) if !opened(dest) && time - cost - 1 > 0 =>
        println(s"${"." * indent}$valve to $dest at $cost with $time left")
        maxPressure(
          valve = dest,
          time = time - 1 - cost,
          pressure = pressure + flowRate(valve) * (time - 1),
          opened = opened + dest,
          indent + 1
        )
    }
  paths.maxOption.getOrElse {
    println(s"${"." * indent}staying at $valve with $time left (pressure: $pressure)")
    pressure
  }

costs("AA").map {
  case (dest, cost) =>
    maxPressure(dest, 30 - cost, 0, Set("AA", dest))
}.max
