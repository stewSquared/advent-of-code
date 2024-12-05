val input = io.Source.fromResource("2018/day-02.txt").getLines().toList

def counts(s: String): Map[Char, Int] = s.groupMapReduce(identity)(_ => 1)(_ + _)

val exactlyTwo = input.count(counts(_).values.toSet.contains(2))
val exactlyThree = input.count(counts(_).values.toSet.contains(3))

val ans1 = exactlyTwo * exactlyThree

def dist(s1: String, s2: String): Int =
  s1.zip(s2).count { case (a, b) => a != b }

input.size

val candidates = for
  s1 <- input
  s2 <- input
  if dist(s1, s2) == 1
yield s1.zip(s2).collect:
  case (a, b) if a == b => a

val ans2 = candidates.head.mkString
