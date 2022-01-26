import util.Using

val groups = Using(io.Source.fromResource("2020/day-06.txt")){ source =>
  val lines = source.getLines
  val lb = collection.mutable.ListBuffer[List[String]]()
  while lines.hasNext
  do lb += lines.takeWhile(_.nonEmpty).toList
  lb.result()
}.get

val ans = groups.map(g => g.map(_.toSet).reduce(_ union _).size).sum

val ans2 = groups.map(g => g.map(_.toSet).reduce(_ intersect _).size).sum
