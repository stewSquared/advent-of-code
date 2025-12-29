package synacor
import synacor.numbers.*
import collection.mutable.Map


@main def ackrun(): Unit =

  def ack(m: Int, n: Int, r8: Int): Int = (m, n) match
    case (0, n) => n + 1
    case (m, 0) => ack(m - 1, r8, r8)
    case (m, n) => ack(m - 1, ack(m, n - 1, r8), r8)

  // val zero = 0.toLit
  val one = 1.toLit
  val two = 2.toLit

  val memo = Map.empty[(Lit, Lit), Lit]
  def ackOpt(m: Lit, n: Lit, r8: Lit): Lit =
    inline def calc = (m, n) match
      case (m, n) if m == 0.toLit => n + one
      case (m, n) if m == 1.toLit => n + r8 + one
      case (m, n) if m == 2.toLit => (r8+one)*n + (r8*two + one)
      // case (3, n) if r8==2 => ackOpt(3,n-1,r8)*3 + 5
      // case (m, n) if m == 3.toLit => ackOpt(3,n-1,r8)*(r8+1) + (2*r8+1)
      case (m, n) if n == 0.toLit => ackOpt(m + 0x7FFF.toLit, one, r8)
      case (m, n)                 => ackOpt(m + 0x7FFF.toLit, ackOpt(m, n + 0x7FFF.toLit, r8), r8)
    memo.getOrElseUpdate((m,n), calc)

  println:
    (0 to 10).map(n => ack(2,n,4)).mkString(" ")

  println:
    (0 to 10).map(n => ackOpt(2.toLit,n.toLit,4.toLit)).mkString(" ")

  var r8: Lit = 0.toLit
  var ans = 0.toLit
  while !(ans == 6.toLit) && r8.toInt < 0x7FFF do
    r8+=1.toLit
    ans = ackOpt(4.toLit,1.toLit,r8)
    if ans == 6.toLit then
      println(s"Found r8=${r8} giving ack(4,1)=${ans}")
    memo.clear()
    // if (r8.toInt % 1000) == 1 then
    println(s"A(4,1,$r8) == $ans")

  // println(s"r8=$r8 gives ack(4,1)=$ans")
