val input = io.Source.fromResource("2022/day-16.txt").getLines().toList

input.size

val flowRate: Map[String, Int] = input.collect {
  case s"Valve $valve has flow rate=$rate; $_" =>
    valve -> rate.toInt
}.toMap

val adj: Map[String, List[String]] = input.collect {
  case s"Valve $valve has $_ valves $valves" =>
    valve -> valves.split(", ").toList // maybe sort
  case s"Valve $valve has $_ valve $other" =>
    valve -> List(other)
}.toMap


def prune(room: String, canVisit: Map[String, Map[String, Int]]) =
  val adj = canVisit(room).keys.toVector

  val removed = canVisit
    .removed(room)
    .view
    .mapValues(_.removed(room))
    .toMap

  val costs = for
    ia <- adj.indices
    ib <- 0 until ia
    a = adj(ia)
    b = adj(ib)
    oldCost = canVisit(a).get(b).getOrElse(Int.MaxValue)
    newCost = canVisit(room)(a) + canVisit(room)(b)
  yield
    assert((canVisit(a).get(b)) == (canVisit(b).get(a)))
    (a, b) -> (newCost min oldCost)

  costs.foldLeft(removed) { case (acc, ((a, b), c)) =>
    acc
      .updated(a, acc(a).updated(b, c))
      .updated(b, acc(b).updated(a, c))
  }

val empty = flowRate.collect {
  case (k, f) if f == 0 && k != "AA" => k
}

val bigAdj: Map[String, Map[String, Int]] =
  val start = adj.view.mapValues(_.map(a => a -> 1).toMap).toMap
  empty.foldLeft(start)((acc, a) => prune(a, acc))

val bigNodes = bigAdj.keySet

def fill(startNode: String): Map[String, Int] =
  import collection.mutable.{PriorityQueue, Map}
  // var visited = startCosts.keys
  val cost = Map[String, Int](bigAdj(startNode).toSeq*)
  val toVisit = PriorityQueue.empty[String](Ordering.by(cost)).reverse
  toVisit.enqueue(cost.keys.toSeq*)
  var visiting = toVisit.dequeue()

  while cost.size < bigNodes.size - 1 do
    for
      (n, c) <- bigAdj(visiting)
      // if !cost.contains(n)
      newCost = cost(visiting) + c
      if cost.get(n).forall(newCost < _)
      if n != startNode
    do
      println(s"setting cost of $startNode -> $n as ${cost(visiting)}($visiting) + $c")
      cost(n) = cost(visiting) + c
      toVisit.enqueue(n)

    visiting = toVisit.dequeue()

  cost.toMap

val fullCosts = bigNodes.iterator.map {
  n => n -> fill(n)
}.toMap

bigAdj("QE")("AA")

fullCosts("AA")

fill("QE")

bigAdj("QN")("BM")
bigAdj("BM")("SZ")
bigAdj("SZ")("QE")

fullCosts("QN")("QE")
fullCosts("QE")("QN")


for
  a <- bigNodes
  b <- bigNodes
  if a != b
  a2b = fullCosts(a)(b)
  b2a = fullCosts(b)(a)
  if a2b != b2a
do
  println(s"$a $b asymmetric $a2b $b2a")




//
