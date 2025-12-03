val input = io.Source.fromResource("2015/day-08.txt").getLines().toList

def count(s: String): Int = s match
  case "" => 0
  case s"\"$rem" => count(rem)
  case s"\\\"$rem" => 1 + count(rem)
  case s"\\\\$rem" => 1 + count(rem)
  case s"\\x$rem" => 1 + count(rem.drop(2))
  case _ => 1 + count(s.drop(1))

def encode(s: String): String = s.flatMap:
  case '"' => "\\\""
  case '\\' => "\\\\"
  case c => c.toString
.mkString("\"", "", "\"")

val ans1 = input.map(s => s.length - count(s)).sum
val ans2 = input.map(s => encode(s).length - s.length).sum
