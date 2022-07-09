val input = io.Source.fromResource("2020/day-18.txt").getLines.toList

enum Exp:
  case Num(n: Int)
  case Mul(a: Exp, b: Exp)
  case Add(a: Exp, b: Exp)

  def eval: Long = this match
    case Num(n) => n.toLong
    case Mul(a, b) => a.eval * b.eval
    case Add(a, b) => a.eval + b.eval

import Exp.{Num, Mul, Add}

enum Tree:
  case Tok(c: Char)
  case Paren(toks: List[Tree])

import Tree.{Tok, Paren}

def splitMatching(raw: String): (String, String) =
  val counts = raw.scanLeft((0, 0)) {
    case ((open, close), '(') => (open + 1, close)
    case ((open, close), ')') => (open, close + 1)
    case ((open, close), c) => (open, close)
  }

  val matchingIndex = counts.indexWhere({case (o, c) => o == c}, from = 1)
  val (left, right) = raw.splitAt(matchingIndex)
  left.tail.init -> right

val (inside, outside) = splitMatching("(aoeu()htns)trailing")

def tokenize(rawExp: String): List[Tree] =
  rawExp match
    case flat if !flat.contains("(") =>
      flat.replaceAll(" ", "").map(Tok.apply).toList
    case nested =>
      val (leading, toSplit) = nested.splitAt(nested.indexOf("("))
      val (paren, trailing) = splitMatching(toSplit)
      tokenize(leading) ::: Paren(tokenize(paren)) :: tokenize(trailing)

tokenize("3 * (4 + (2 * 1) + 5) + 6")

def parse(tokens: List[Tree]): Exp =
  def parseTerm(tok: Tree): Exp = tok match
    case Paren(toks) => parse(toks)
    case Tok(c) if c.isDigit => Num(c.asDigit)
    case Tok(c) => throw Exception(s"expected digit, found: $c")

  val first: Exp = tokens.head match
    case Paren(toks) => parse(toks)
    case Tok(c) if c.isDigit => Num(c.asDigit)
  tokens.tail.grouped(2).foldLeft(first){
    case (left, List(op, tok)) =>
      val right = tok match
        case Paren(toks) => parse(toks)
        case Tok(c) if c.isDigit => Num(c.asDigit)
        case Tok(c) => throw Exception(s"expected digit: $c")
      op match
        case Tok('*') => Mul(left, right)
        case Tok('+') => Add(left, right)
        case Tok(c) => throw Exception(s"unexpected operator: $c")
  }

def parse2(tokens: List[Tree]): Exp =
  def sum(addends: List[Tree]): Exp =
    addends.collect {
      case Tok(d) if d.isDigit => Num(d.asDigit)
      case Paren(toks) => parse2(toks)
    }.reduceLeft(Add.apply)

  def product(toks: List[Tree]): Exp =
    val (term, rest) = toks.span(_ != Tok('*'))
    val firstSum = sum(term)
    if rest.isEmpty then firstSum
    else Mul(firstSum, product(rest.tail))

  product(tokens)


  // def buildProduct: Exp =
  // val ms = tokens.spl



tokenize("3 + 4")
parse(tokenize("3 + 4"))

parse(tokenize("(3 + 4) * 5"))

parse(tokenize("2 * (3 + 4)"))

parse(tokenize("2 * (3 + 4)")).eval

parse(tokenize("2 + 3 * 4"))

parse(tokenize("2 + 3 * 4")).eval

val ans1 = input.map(raw => parse(tokenize(raw)).eval).sum

def eval1(raw: String) =
  parse(tokenize(raw)).eval

def eval2(raw: String) =
  parse2(tokenize(raw)).eval

eval2("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")

val ans2 = input.map(raw => parse2(tokenize(raw)).eval).sum

// def parse(rawExp: String): Exp = rawExp match
//   case "a * rest" =>
