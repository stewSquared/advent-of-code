val input = io.Source.fromResource("2020/day-13.txt").getLines.toList

val currentTime = input(0).toInt
val buses = input(1).split(",").flatMap(_.toIntOption).toList


def next(time: Int, bus: Int): Int =
  time / bus * bus + bus

val nextBus = buses.minBy(next(currentTime, _))

val ans = nextBus * (next(currentTime, nextBus) - currentTime)


val buses2 = input(1).split(",").map(_.toIntOption).toList


println((13 to 13 * 17 by 13).mkString(","))
println((17 to 13 * 17 by 17).mkString(","))

def xCount(bs: List[Option[Int]]): (Int, List[Option[Int]]) =
  println(bs)
  val (wildcards, remaining) = bs.tail.span(_.isEmpty)
  wildcards.size -> remaining

List(Some(10), None, None, Some(13)).exists(_.isEmpty)
xCount(List(Some(10), None, None, Some(13)))

xCount(List(Some(10)))


val deltas =
  Iterator.unfold(buses2)(bs =>
    Option(xCount(bs)).filter(_._2.nonEmpty)
  ).toList

buses2.flatten
deltas

buses2.flatten.size
deltas.size

def foo(b1: Int, b2: Int, gap: Int): (Int, Int) =
  def adj(b1s: List[Int], b2s: List[Int]): (Int, Int) = (b1s, b2s) match
    case (l :: _, r :: _) if l + gap == r => (l, r)
    case (l :: ls, r :: rs) if l + gap < r => adj(ls, b2s)
    case (l :: ls, r :: rs) if l + gap > r => adj(b1s, rs)
    case _ => ???

  adj((b1 to b1 * b2 by b1).toList, (b2 to b1 * b2 by b2).toList)

foo(41, 37, 34)
41 * 34

foo(37, 379, 5)

37 * 379

// def solve(buses: List[Int], deltas: List[Int])
