val input = io.Source.fromResource("2025/day-02.txt").getLines().next()

val ranges = input.split(',').map:
  case s"$a-$b" => a.toLong to b.toLong

def invalid(n: Long) =
  val s = n.toString
  val k = s.length
  (1 until k).filter(m => k % m == 0).exists: d =>
    s.take(d) * (k / d) == s

val ans1 = ranges.flatten.filter(invalid).sum
