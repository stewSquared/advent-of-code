val input = io.Source.fromResource("2022/day-21.txt").getLines().toVector

enum Yell:
  case Eql(a: String, b: String)
  case Add(a: String, b: String)
  case Sub(a: String, b: String)
  case Mul(a: String, b: String)
  case Div(a: String, b: String)
  case Num(n: Long)
  case Hum

extension(yell: Yell)
  def toExp: Exp = yell match
    case Yell.Eql(a, b) => ???
    case Yell.Add(a, b) => Add(adj(a).toExp, adj(b).toExp)
    case Yell.Sub(a, b) => Sub(adj(a).toExp, adj(b).toExp)
    case Yell.Mul(a, b) => Mul(adj(a).toExp, adj(b).toExp)
    case Yell.Div(a, b) => Div(adj(a).toExp, adj(b).toExp)
    case Yell.Num(n) => Val(n)
    case Yell.Hum => Var

val adj: Map[String, Yell] = input.map {
  case s"root: $a + $b" => "root" -> Yell.Eql(a, b)
  case s"$name: $a + $b" => name -> Yell.Add(a, b)
  case s"$name: $a - $b" => name -> Yell.Sub(a, b)
  case s"$name: $a * $b" => name -> Yell.Mul(a, b)
  case s"$name: $a / $b" => name -> Yell.Div(a, b)
  case s"humn: $n" => "humn" -> Yell.Hum
  case s"$name: $n" => name -> Yell.Num(n.toLong)
}.toMap

def eval(name: String)(h: Long): Long =
  adj(name) match
    case Yell.Add(a, b) => eval(a)(h) + eval(b)(h)
    case Yell.Sub(a, b) => eval(a)(h) - eval(b)(h)
    case Yell.Mul(a, b) => eval(a)(h) * eval(b)(h)
    case Yell.Div(a, b) => eval(a)(h) / eval(b)(h)
    case Yell.Hum => h
    case Yell.Num(n) => n
    case Yell.Eql(_, _) => ???

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

val Yell.Eql(left, right) = adj("root")

println(adj(left).toExp.simplify)
println(adj(right).toExp.simplify)
// val eqn = Eqn(adj(left).toExp.simplify, adj(right).toExp.simplify)

def balance(e: Exp, r: Long): Long = e match
  case Add(Val(n), l) => balance(l, (r - n))
  case Sub(Val(n), l) => balance(l, -(r - n))
  case Mul(Val(n), l) => balance(l, (r / n))
  case Div(Val(n), l) => balance(l, n / r)
  case Add(l, Val(n)) => balance(l, (r - n))
  case Sub(l, Val(n)) => balance(l, (r + n))
  case Mul(l, Val(n)) => balance(l, (r / n))
  case Div(l, Val(n)) => balance(l, r * n)
  case Val(n) => ???
  case Var => r

val Val(r) = adj(right).toExp.simplify

val ans2 = balance(adj(left).toExp.simplify, r)

// val ans1 = eval("root").toLong
// val adj("root")

def eql(human: Long): Long =
  val Yell.Eql(left, right) = adj("root")
  eval(left)(human) - eval(right)(human)



// eql(200_000_000_000L)



//
