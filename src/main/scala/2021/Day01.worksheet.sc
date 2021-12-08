val sweep = io.Source.fromResource("2021/day-01-1.txt").getLines.map(_.toInt).toList

val ans1 = sweep.zip(sweep.tail).count(_ < _)
val ans2 = sweep.zip(sweep.drop(3)).count(_ < _)
