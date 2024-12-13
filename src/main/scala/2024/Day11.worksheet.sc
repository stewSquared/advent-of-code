val stones = io.Source.fromResource("2024/day-11.txt")
  .getLines.mkString
  .split(" ").map(_.toLong).toVector

import collection.mutable.Map
val memo = Map.empty[(Long, Int), Long]

def blink(stone: Long, times: Int): Long =
  lazy val calculate =
    if times == 0 then 1L else
      stone match
        case 0L => blink(1L, times - 1)
        case n if n.toString.size % 2 == 0 =>
          val s = n.toString
          val (l, r) = s.splitAt(s.length/2)
          blink(l.toLong, times - 1) + blink(r.toLong, times - 1)
        case n => blink(n * 2024, times - 1)

  memo.getOrElseUpdate((stone, times), calculate)

stones.map(blink(_, 25)).sum
stones.map(blink(_, 75)).sum
