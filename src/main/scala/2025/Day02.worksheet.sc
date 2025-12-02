val input = io.Source.fromResource("2025/day-02.txt").getLines().next()

val ranges = input.split(',').map:
  case s"$a-$b" => a.toLong to b.toLong

def invalid(id: Long) = """(\d+)\1""".r.matches(id.toString)
def invalid2(id: Long) = """(\d+)\1+""".r.matches(id.toString)

val ans1 = ranges.iterator.flatten.filter(invalid).sum
val ans2 = ranges.flatten.filter(invalid2).sum
