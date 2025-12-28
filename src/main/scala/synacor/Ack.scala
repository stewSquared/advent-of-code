package synacor
import synacor.numbers.*
import collection.mutable.Map


@main def ackrun(): Unit =

  def ack(m: Int, n: Int): Int = (m, n) match
    case (0, n) => n + 1
    case (m, 0) => ack(m - 1, 1)
    case (m, n) => ack(m - 1, ack(m, n - 1))

  val memo = Map.empty[(Int, Int), Int]
  def ackInt(m: Int, n: Int, r8: Int = 1.toInt): Int =
    lazy val calc = (m, n) match
      case (0, n) => n + 1.toInt
      case (m, 0) => ackInt(m + 0x7FFF.toInt, r8, r8)
      case (m, n) => ackInt(m + 0x7FFF.toInt, ackInt(m, n + 0x7FFF.toInt, r8), r8)
    memo.getOrElseUpdate((m, n), calc)

  var ans = ack(1.toInt,1.toInt)
  println(ans)
  var r8 = 1.toInt
  while ans != 6.toInt do
    r8 += 1.toInt
    memo.clear()
    println(s"trying r8: $r8")
    ans = ack(4.toInt, 1.toInt)
    println(s"  $ans")

  println(s"Found R8 = $r8")
