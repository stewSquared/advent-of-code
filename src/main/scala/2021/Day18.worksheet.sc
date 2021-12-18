val input = io.Source.fromResource("2021/day-18-1.txt").getLines.toList

enum Num:
  case N(n: Int)
  case P(a: Num, b: Num)

  override def toString = this match
    case N(n)    => s"$n"
    case P(a, b) => s"[$a,$b]"

  def swap: Num = this match
    case N(n)    => N(n)
    case P(a, b) => P(b, a)

  def depth: Int = this match
    case N(n)    => 0
    case P(a, b) => 1 + (a.depth max b.depth)

  def magnitude: Int = this match
    case N(n)    => n
    case P(a, b) => 3 * a.magnitude + 2 * b.magnitude

  def addR(x: Int): Num = this match
    case N(n)    => N(n + x)
    case P(a, b) => P(a, b.addR(x))

  def addL(x: Int): Num = this match
    case N(n)    => N(n + x)
    case P(a, b) => P(a.addL(x), b)

  def split: Option[Num] = this match
    case N(n) if n >= 10 => Some(P(N(n / 2), N((n + 1) / 2)))
    case N(_)            => None
    case P(a, b) =>
      a.split.map(P(_, b)) orElse b.split.map(P(a, _))

  def add(that: Num): Num = P(this, that).reduce

  def reduce: Num = explode
    .map(_.reduce)
    .orElse(split.map(_.reduce))
    .getOrElse(this)

  def explode: Option[Num] = explode(0).map(_.newNum)

  import Num.Explode

  def explode(nested: Int): Option[Explode] = this match
    case N(_) => None
    case P(N(a), N(b)) if nested == 4 =>
      Some(Explode(incL = Some(a), newNum = N(0), incR = Some(b)))
    case p @ P(a, b) =>
      a.explode(nested + 1).map { case res @ Explode(_, newA, incB) =>
        val newB = incB.map(b.addL).getOrElse(b)
        res.copy(newNum = P(newA, newB), incR = None)
      } orElse b.explode(nested + 1).map { case res @ Explode(incA, newB, _) =>
        val newA = incA.map(a.addR).getOrElse(a)
        res.copy(incL = None, newNum = P(newA, newB))
      }

object Num:
  case class Explode(incL: Option[Int], newNum: Num, incR: Option[Int])

  object D:
    def unapply(s: String): Option[N] = s.toIntOption.map(N(_))

  def apply(str: String) = this.parsePair(str)

  def parsePair(str: String): Num = str match
    case s"[${D(a)},${D(b)}]" => P(a, b)
    case s"[${D(a)},${pair}]" => P(a, parsePair(pair))
    case s"[$inner]" =>
      splitMatching(inner) match
        case (pair, s",${D(b)}") => P(parsePair(pair), b)
        case (left, s",$right")  => P(parsePair(left), parsePair(right))

  def splitMatching(str: String, from: Int = 0): (String, String) =
    val counts = str.scanLeft((0, 0)) {
      case ((open, close), '[') => (open + 1, close)
      case ((open, close), ']') => (open, close + 1)
      case ((open, close), c)   => (open, close)
    }
    val balanced = counts.indexWhere(_ == _, from = 1)
    str.splitAt(balanced)

import Num.*

val snailNums = input.map(Num.parsePair)

val finalSum = snailNums.reduce((a, b) => a.add(b).reduce)

val ans1 = finalSum.magnitude

val ans2 = snailNums
  .combinations(2)
  .flatMap(p => p.zip(p.reverse))
  .map((a, b) => a.add(b).magnitude)
  .max

// test parsing

def parse(raw: String): Num =
  def p(o: List[Num], s: String): Num = s match
    case s"[${D(a)},$rest" => p(a :: o, rest)
    case s"[$rest"         => p(o, rest)
    case s",$rest"         => p(o, rest)
    case s"$b]$rest" =>
      (o, b) match
        case (a :: tail, D(b))    => p(P(a, b) :: tail, rest)
        case (b :: a :: tail, "") => p(P(a, b) :: tail, rest)
        case _ => throw Exception(s"early close with $s for $o")
    case unparsed =>
      o match
        case num :: Nil if unparsed.isEmpty => num
        case open => throw Exception(s"failed parsing $unparsed after $open")
  p(Nil, raw)

def parse2(raw: String): Num =
  def p(s: String): List[Num] = s match
    case ""               => Nil
    case s"${D(a)},$rest" => a :: p(rest)
    case s"${D(b)}]$rest" => b :: p(rest)
    case s"[$rest" =>
      p(rest) match
        case a :: b :: tail => P(a, b) :: tail
        case _              => throw Exception(s"no match in: $rest")
    case s",$rest" => p(rest)
    case s"]$rest" => p(rest)
    case remaining => throw Exception(s"can't parse: $remaining")
  p(raw) match
    case List(num) => num
    case Nil       => throw Exception(s"nothing parsed: $raw")
    case open      => throw Exception(s"unmatched remain: $open")

def c(s: String) = parse2(s)

val exampleNums = """[1,2]
[[1,2],3]
[9,[8,7]]
[[1,9],[8,5]]
[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]""".linesIterator
  .map(_.strip)
  .toList

c(exampleNums(0))
c(exampleNums(1))
c(exampleNums(2))
c(exampleNums(3))
c("[[1,[2,3]],4]")
c("[1,[[2,3],4]]")
c("[[[1,2],3],4]")
c("[[[[0,1],2],3],4]")
c("[[[2,[0,1]],3],4]")
c("[[3,[[0,1],2]],4]")
c("[4,[[[0,1],2],3]]")
c(exampleNums(4))
c(exampleNums(5))
c(exampleNums(6))

// tests explosions

val ex1 = Num("[[[[[9,8],1],2],3],4]").explode
assert(ex1.contains(Num("[[[[0,9],2],3],4]")))

val ex2 = Num("[7,[6,[5,[4,[3,2]]]]]").explode
assert(ex2.contains(Num("[7,[6,[5,[7,0]]]]")))

val ex3 = Num("[[6,[5,[4,[3,2]]]],1]").explode
assert(ex3.contains(Num("[[6,[5,[7,0]]],3]")))

val ex4 = Num("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]").explode
assert(ex4.contains(Num("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")))

val ex5 = Num("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]").explode
assert(ex5.contains(Num("[[3,[2,[8,0]]],[9,[5,[7,0]]]]")))

// test addition

val add1 = Num("[[[[4,3],4],4],[7,[[8,4],9]]]").add(Num("[1,1]"))
assert(add1 == Num("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))
