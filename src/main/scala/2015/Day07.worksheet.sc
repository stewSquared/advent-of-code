val input = io.Source.fromResource("2015/day-07.txt").getLines().toList

enum Ref:
  case Constant(value: Int)
  case Wire(label: String)

import Ref.*

enum Gate:
  case AND(a: Ref, b: Ref)
  case OR(a: Ref, b: Ref)
  case LSHIFT(a: Ref, n: Int)
  case RSHIFT(a: Ref, n: Int)
  case NOT(a: Ref)

import Gate.*

def arg(s: String): Ref =
  s.toIntOption match
    case Some(n) => Constant(n)
    case None => Wire(s)

given circuit: Map[Wire, Gate | Ref] = Map.from[Wire, Gate | Ref]:
  input.collect:
    case s"$a AND $b -> $c" => Wire(c) -> AND(arg(a), arg(b))
    case s"$a OR $b -> $c" => Wire(c) -> OR(arg(a), arg(b))
    case s"$a LSHIFT $n -> $c" => Wire(c) -> LSHIFT(arg(a), n.toInt)
    case s"$a RSHIFT $n -> $c" => Wire(c) -> RSHIFT(arg(a), n.toInt)
    case s"NOT $a -> $c" => Wire(c) -> NOT(arg(a))
    case s"$a -> $b" => Wire(b) -> arg(a)

val memo = collection.mutable.Map.empty[Ref, Int]

def eval(a: Ref)(using circuit: Map[Wire, Gate | Ref]): Int =
  lazy val calc = a match
    case Constant(n) => n
    case w: Wire =>
      circuit(w) match
        case ref: Ref => eval(ref)
        case Gate.AND(a, b) => (eval(a) & eval(b))
        case Gate.OR(a, b) => (eval(a) | eval(b))
        case LSHIFT(a, n) => (eval(a) << n)
        case RSHIFT(a, n) => (eval(a) >>> n)
        case NOT(a) => (~eval(a))
  memo.getOrElseUpdate(a, calc)

val ans1 = eval(Wire("a"))

memo.clear()

val circuit2 = circuit.updated(Wire("b"), Constant(ans1))

val ans2 = eval(Wire("a"))(using circuit2)

// eval("d") // 72
// eval("e") // 507
// eval("f") // 492
// eval("g") // 114
// eval("h") // 65412
// eval("i") // 65079
// eval("x") // 123
// eval("y") // 456
