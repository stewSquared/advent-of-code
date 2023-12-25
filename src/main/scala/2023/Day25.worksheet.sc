import aoc.*

val input = io.Source.fromResource("2023/day-25.txt").getLines.toList

// val adj = Map.from[String, List[String]]:
//   input.map:
//     case s"$c: $cs" =>
//       c -> cs.split(" ").toList

val adj: Map[String, List[String]] =
  val pairs = input.flatMap:
    case s"$c: $cs" =>
      cs.split(" ").toList.flatMap(c2 => List(c -> c2, c2 -> c))
  pairs.groupMap(_._1)(_._2)

adj.size

adj.collect:
  case (k, vs) if vs.size == 4 => k

adj("mvd")


adj.values.map(_.size)

adj.values.map(_.size).toList.sorted.take(10)

adj.values.map(_.size).toList.sorted.takeRight(10)


// def search(subGraph: Set[String], visiting: Set[String]): Set[String] =
//   assert(visiting.size != 0)
//   if visiting.size == 3 then subGraph
//   else
//     val next = visiting.flatMap(adj).diff(subGraph)
//     search(subGraph union visiting, next)

def search(subGraph: Set[String], options: Set[String]): Set[String] =
  if options.size == 3 then subGraph
  else
    val next = options.minBy(adj(_).toSet.diff(subGraph).size)
    val newOptions = adj(next).toSet.diff(subGraph)
    search(subGraph + next, (options - next) union newOptions)

val ans1 =
  val s = search(Set.empty, Set("klg"))
  s.size * (adj.size - s.size)

  // val next = option.min


// def search: Set[String] =
//   import collections.mutable.{PriorityQueue, Set}

//   val visited = Set("klg")

//   if options.size == 3 then subGraph
//   else
//     val next = options


// val s1 = search(Set.empty, Set("klg"))
// val s = s1.size
// val ans1 = s * (adj.size - s)


def connected(seen: Set[String], c: String): Set[String] =
  if seen.contains(c) then seen
  else adj(c).foldLeft(seen + c)(connected(_, _))

connected(Set.empty, "klg").size

// input.size

// adj.keySet
// adj.values.flatten.toSet

// adj.keySet.size
// adj.values.flatten.toSet.size

// val components = adj.keySet.union(adj.values.flatten.toSet)

// val links = adj.keySet.intersect(adj.values.flatten.toSet)

// components.size

// links.size

// adj.foreach:
//   case (k, vs) => vs.foreach: v =>
//     println(s""" "$k" -- "$v";""")

// links.combinations(3).toList.size



//
