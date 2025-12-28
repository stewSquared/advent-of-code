import synacor.numbers.*
import collection.mutable.Map

// def ackLit(m: Lit, n: Lit, r8: Lit = 1.toLit): Lit = (m, n) match
//   case (m, n) if m == 0.toLit => n + 1.toLit
//   case (m, n) if n == 0.toLit => ackLit(m + 0x7FFF.toLit, r8, r8)
//   case (m, n)                 => ackLit(m + 0x7FFF.toLit, ackLit(m, n + 0x7FFF.toLit, r8), r8)

// def ackLit(m: Lit, n: Lit, r8: Lit = 1.toLit): Lit =
//   var r1 = m
//   var r2 = n
//   while r2 != 0 do

//   r2 + 1

//   (m, n) match
//   case (m, n) if m == 0.toLit => n + 1.toLit
//   case (m, n) if n == 0.toLit => ackLit(m + 0x7FFF.toLit, r8, r8)
//   case (m, n)                 => ackLit(m + 0x7FFF.toLit, ackLit(m, n + 0x7FFF.toLit, r8), r8)




val memo = Map.empty[(Int, Int), Int]

def ack(m: Int, n: Int): Int =
  lazy val calc = (m, n) match
    case (0, n) => n + 1
    case (m, 0) => ack(m - 1, 1)
    case (m, n) => ack(m - 1, ack(m, n - 1))
  memo.getOrElseUpdate((m, n), calc)

val memo2 = Map.empty[(Int, Int), Int]

def ack2(m: Int, n: Int, r8: Int = 2): Int =
  lazy val calc = (m, n) match
    case (0, r2) => r2 + 1
    case (r1, 0) => ack2(r1 - 1, r8)
    case (r1, r2) => ack2(r1 - 1, ack2(r1, r2 - 1))
  memo2.getOrElseUpdate((m, n), calc)

ack(2,1)
ack2(2,1)
memo.size
memo2.size



//
