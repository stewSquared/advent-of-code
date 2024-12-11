val stones = io.Source.fromResource("2024/day-11.txt")
  .getLines.mkString
  .split(" ").map(_.toLong).toVector

def blink(stone: Long): List[Long] = stone match
  case 0 => List(1L)
  case n if n.toString.size % 2 == 0 =>
    val s = n.toString
    val (l, r) = s.splitAt(s.length/2)
    List(l.toLong, r.toLong)
  case n => List(n * 2024)

val stoneCounts = stones.map(s => s -> 1L).toMap

// def mergeCountMaps(m1: Map[Long, Int], m2: Map[Long, Int]): Map[Long, Int] =
//   m1.foldLeft(m2):
//     case (acc, (k, v)) =>
//       acc.updated(k, v + acc.getOrElse(k, 0))

def next(stoneCounts: Map[Long, Long]): Map[Long, Long] =
  stoneCounts
    .toList
    .flatMap:
      case (stone, count) =>
        blink(stone).map(_ -> count)
    .groupMapReduce(_._1)(_._2)(_ + _)

List(2 -> 1, 2 -> 3).groupMapReduce(_._1)(_._2)(_ + _)

next(Map(0L -> 1))

def series = Iterator.iterate(stoneCounts)(next)

val ans1 = series.drop(25).next.values.sum
val ans2 = series.drop(75).next.values.map(_.toLong).sum
// 91840012 too low
// 3718667568

// Iterator.iterate(Map(0L -> 1))(next).take(15) foreach println

stones.size
stones.head
stones(1)

// blink(8)
// Iterator.iterate(Vector(28L))(_.flatMap(blink)).take(15) foreach println
// Iterator.iterate(Vector(4L))(_.flatMap(blink)).take(15) foreach println
// Iterator.iterate(Vector(0L))(_.flatMap(blink)).take(15) foreach println

// keep all values in vector? unlikely
// memo
// cycle search
// predict splits without calculating values

// val ans2 = Iterator.iterate(stones)(_.flatMap(blink))
//   .drop(75).next.size

//
