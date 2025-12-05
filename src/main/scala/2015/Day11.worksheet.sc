val input = "vzbxkghb"

def hasStraight(pw: String) =
  pw.sliding(3).exists:
    s => s(0) + 1 == s(1) && s(1) + 1 == s(2)

def hasTwoPair(pw: String) =
  val pairs = pw.zip(pw.tail)
  val i = pw.zip(pw.tail).indexWhere(_ == _)
  pairs.indexWhere(_ == _, i + 2) != -1

def increment(c: Char): (Char, Boolean) =
   val c2 = ((c + 1 - 97) % 26 + 97).toChar match
    case 'i' => 'j'
    case 'o' => 'p'
    case 'l' => 'm'
    case c => c
   c2 -> (c2 == 'a')

def increment(pw: String): String =
  val (ones, carry) = increment(pw.last)

  val (next, overflow) = pw.init.foldRight(ones.toString, carry):
    case (digit, (acc, carry)) =>
      if carry then
        val (d, c) = increment(digit)
        (d +: acc, c)
      else
        (digit +: acc, carry)
  next

val ans1 = Iterator.iterate(input)(increment).find:
  pw => hasStraight(pw) && hasTwoPair(pw)
.get

val ans2 = Iterator.iterate(ans1)(increment).drop(1).find:
  pw => hasStraight(pw) && hasTwoPair(pw)
.get
