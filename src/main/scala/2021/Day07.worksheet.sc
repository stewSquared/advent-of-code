val positions = io.Source.fromResource("2021/day-07-1.txt")
  .getLines.toList.head
  .split(",").map(_.toInt).toList

def linearCost(n: Int) = positions.map(p => (n - p).abs).sum

def triangularCost(n: Int) = positions.map { p =>
    val d = (n - p).abs
    d * (d + 1) / 2
  }.sum

// naieve brute force for input size of 1000 is fine
val ans1 = positions.indices.map(linearCost).min
val ans2 = positions.indices.map(triangularCost).min
