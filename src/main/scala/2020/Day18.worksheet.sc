val input = io.Source.fromResource("2020/day-18.txt").getLines.toList

def time(r: String, switch: Int): Int =
  val (scooter, foot) = r.splitAt(switch)
  val st = scooter.count(_ == 'A') * 5 + scooter.count(_ == 'S') * 40
  val ft = foot.count(_ == 'A') * 20 + foot.count(_ == 'S') * 30
  ft + st

time("ASAASS", 1)


def eval(evaluated: Option[Int], raw: String): (Int, String) = raw match
  case s"$n $rest" if n.toIntOption.isDefined => eval(Some(n.toInt), rest)
  case s"$n " => ???
//   case s"* ($rest" => evaluated.map


enum Expr:
  case Add(l: Expr, r: Expr)
  case Mul(l: Expr, r: Expr)
  case Num(n: Int)

// def parse(raw: String): (Option[Expr], String) = raw match
//     case "" => None -> ""
//     case _ => ???

import Expr.*

val s"$left$right" = "12345"
println(left)

"1234".head.isDigit

def parse(raw: String): (Option[Expr], String) =
  def p(left: List[Expr], remaining: String): (Option[Expr], String) =
    remaining match
      case "" => left.headOption -> "" // assert left has no more than one
      case s"$d $r" if d.toIntOption.isDefined =>
        p(Num(d.toInt) :: left, r)
      case s"* $d $r" if d.toIntOption.isDefined =>
        p(Mul(left.head, Num(d.toInt)) :: left.tail, r)
      case s"* ($d " => ???
      // case "* $r" => val (nextExp, rest) = p(Nil, r)
      case _ => ???
  p(Nil, raw)

// def parse(raw: String, depth: Int = 0): Expr = raw match
//     case "($str" =>
//         // val (left, remaining) = parse(str, depth + 1)
//         // ???
//     case s"$left * $right" => Mul(Num(left.toInt), Num(right.toInt))

// def parse(unmatched: List[Expr], raw: String): Expr = raw match
//     case s"* $right" =>

// def parse(raw: String): Expr =
//     def p1(raw: String, open: List[(Expr, Char)]) = raw match
//         case s"($rest" => ???
//         case s"$a $op $rest" => p1(a, Nil)
//         case s""

// def parse(rawExpression: String): Expr =
//     def p1(raw: String, depth: Int): Expr = raw match
//         case s"$d + $rest" => Add(p1(rest), Num(d1.toInt))
//         case s"$d * $rest" => Mul(p1(rest), Num(d1.toInt))
//         // case s")$rest" => p1()
