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

  def eval: Long = this match
    case Add(a, b) => a.eval + b.eval
    case Sub(a, b) => a.eval - b.eval
    case Mul(a, b) => a.eval * b.eval
    case Div(a, b) => a.eval / b.eval
    case Val(n) => n
    case Var => ???

case class Add(a: Exp, b: Exp) extends Exp
case class Sub(a: Exp, b: Exp) extends Exp
case class Mul(a: Exp, b: Exp) extends Exp
case class Div(a: Exp, b: Exp) extends Exp
case class Val(n: Long) extends Exp
case object Var extends Exp

case class Eqn(lhs: Exp, rhs: Exp):
  def solve = Iterator.iterate(this) { case Eqn(lhs, rhs) =>
    (lhs: @unchecked) match
      case Add(a, b) if a.hasVar => Eqn(a, Sub(rhs, b))
      case Sub(a, b) if a.hasVar => Eqn(a, Add(rhs, b))
      case Mul(a, b) if a.hasVar => Eqn(a, Div(rhs, b))
      case Div(a, b) if a.hasVar => Eqn(a, Mul(rhs, b))
      case Add(a, b) if b.hasVar => Eqn(b, Sub(rhs, a))
      case Sub(a, b) if b.hasVar => Eqn(b, Sub(a, rhs))
      case Mul(a, b) if b.hasVar => Eqn(b, Div(rhs, a))
      case Div(a, b) if b.hasVar => Eqn(b, Div(a, rhs))
      case Var => Eqn(lhs, rhs)
      case _ if rhs.hasVar => Eqn(rhs, lhs)
  } collectFirst { case Eqn(Var, rhs) => rhs.eval }

val s"$left $_ $right" = yells("root"): @unchecked

val ans2 = Eqn(parse(left), parse(right)).solve.get
