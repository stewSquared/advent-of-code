val lines = io.Source.fromResource("2022/day-01.txt").getLines()
val lb = collection.mutable.ListBuffer.empty[Int]
while lines.hasNext do
  lb += lines.takeWhile(_.nonEmpty).map(_.toInt).sum
val calories = lb.result()
val ans1 = calories.max
val ans2 = calories.sorted.takeRight(3).sum
