val input = io.Source.fromResource("2018/day-01.txt").getLines().map(_.toInt).toList

input.size

val ans1 = input.sum

lazy val deltas: LazyList[Int] = LazyList.fromSpecific(input) #::: deltas

val frequencies = deltas.scanLeft(0)(_ + _)

// object for tailrec
object Foo:
  val offset = input.sum
  def search(seen: Set[Int], toCheck: LazyList[Int]): Int =
    toCheck match
      case f #:: fs =>
        val positive = f + offset
        if seen(positive) then f
        else search(seen + positive, fs)

val ans2 = Foo.search(Set.empty, frequencies)
