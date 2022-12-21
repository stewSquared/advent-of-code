val input = io.Source.fromResource("2022/day-21.txt").getLines().toVector

val yells: Map[String, String] = input.map {
  case s"$name: $op" => name -> op
}.toMap

def eval(name: String): Long = yells(name) match
  case s"$a + $b" => eval(a) + eval(b)
  case s"$a - $b" => eval(a) - eval(b)
  case s"$a * $b" => eval(a) * eval(b)
  case s"$a / $b" => eval(a) / eval(b)
  case n => n.toLong

val ans1 = eval("root")

def parse(name: String): Exp =
  if name == "humn" then Var else yells(name) match
    case s"$a + $b" => Add(parse(a), parse(b))
    case s"$a - $b" => Sub(parse(a), parse(b))
    case s"$a * $b" => Mul(parse(a), parse(b))
    case s"$a / $b" => Div(parse(a), parse(b))
    case n => Val(n.toLong)

sealed trait Exp:
  override def toString: String = this match
    case Add(a, b) => s"($a + $b)"
    case Sub(a, b) => s"($a - $b)"
    case Mul(a, b) => s"($a * $b)"
    case Div(a, b) => s"($a / $b)"
    case Val(n) => n.toString
    case Var => "???"

  def hasVar: Boolean = this match
    case Add(a, b) => a.hasVar || b.hasVar
    case Sub(a, b) => a.hasVar || b.hasVar
    case Mul(a, b) => a.hasVar || b.hasVar
    case Div(a, b) => a.hasVar || b.hasVar
    case Val(n) => false
    case Var => true

  def partialEval: Long = this match
    case Add(a, b) => a.partialEval + b.partialEval
    case Sub(a, b) => a.partialEval - b.partialEval
    case Mul(a, b) => a.partialEval * b.partialEval
    case Div(a, b) => a.partialEval / b.partialEval
    case Val(n) => n
    case Var => ???

  def simplify: Exp = this match
    case Add(a, b) if !hasVar => Val(this.partialEval)
    case Sub(a, b) if !hasVar => Val(this.partialEval)
    case Mul(a, b) if !hasVar => Val(this.partialEval)
    case Div(a, b) if !hasVar => Val(this.partialEval)
    case Add(a, b) => Add(a.simplify, b.simplify)
    case Sub(a, b) => Sub(a.simplify, b.simplify)
    case Mul(a, b) => Mul(a.simplify, b.simplify)
    case Div(a, b) => Div(a.simplify, b.simplify)
    case func => func

case class Add(a: Exp, b: Exp) extends Exp
case class Sub(a: Exp, b: Exp) extends Exp
case class Mul(a: Exp, b: Exp) extends Exp
case class Div(a: Exp, b: Exp) extends Exp
case class Val(n: Long) extends Exp
case object Var extends Exp

def solve(e: Exp, r: Long): Long = e match
  case Add(Val(n), l) => solve(l, r - n)
  case Sub(Val(n), l) => solve(l, n - r)
  case Mul(Val(n), l) => solve(l, r / n)
  case Div(Val(n), l) => solve(l, n / r)
  case Add(l, Val(n)) => solve(l, r - n)
  case Sub(l, Val(n)) => solve(l, r + n)
  case Mul(l, Val(n)) => solve(l, r / n)
  case Div(l, Val(n)) => solve(l, r * n)
  case Val(n) => ???
  case Var => r

val s"$left + $right" = yells("root"): @unchecked

val ans2 = solve(parse(left).simplify, parse(right).partialEval)
