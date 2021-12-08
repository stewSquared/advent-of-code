import util.chaining.*

val input = io.Source.fromResource("2021/day-06-1.txt").getLines.next()
val start = input.split(",").map(_.toInt)

val memo = collection.mutable.Map.empty[Int, Long]

def descendants(days: Int): Long =
  memo.get(days).getOrElse {
    val offspring = (days - 9) to 0 by -7
    (offspring.size + offspring.map(descendants).sum).tap(memo(days) = _)
  }

val ans1 = start.map(days => descendants(80 + 8 - days)).sum + start.size
val ans2 = start.map(days => descendants(256 + 8 - days)).sum + start.size
