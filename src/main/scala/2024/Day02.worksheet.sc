val input = io.Source.fromResource("2024/day-02.txt").getLines()
  .toList.map(_.split(" ").map(_.toInt).toList)

def deltas(levels: List[Int]) =
  levels.zip(levels.tail).map((a, b) => b - a)

def safe(levels: List[Int]): Boolean =
  val d = deltas(levels)
  d.forall((1 to 3).contains) || d.forall((-1 to -3 by -1).contains)

def safeDampened(levels: List[Int]): Boolean =
  safe(levels) || levels.indices.exists: i =>
    val (left, right) = levels.splitAt(i)
    safe(left ++ right.tail)

val ans1 = input.count(safe)
val ans2 = input.count(safeDampened)
