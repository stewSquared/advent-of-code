val sacks = io.Source.fromResource("2022/day-03.txt").getLines().toList

def prio(c: Char) =
  if c.isUpper then c.toInt - 65 + 27
  else c.toInt - 97 + 1

val ans1 = sacks.map { sack =>
  val (left, right) = sack.splitAt(sack.size / 2)
  prio(left.intersect(right).distinct.head)
}.sum

val ans2 = sacks.grouped(3).map { group =>
  prio(group.reduce(_ intersect _).head)
}.sum
