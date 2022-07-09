val input = io.Source.fromResource("2020/day-18.txt").getLines.toList

enum Exp:
  case Num(n: Int)
  case Mul(a: Exp, b: Exp)
  case Add(a: Exp, b: Exp)

  def eval: Long = this match
    case Num(n)    => n.toLong
    case Mul(a, b) => a.eval * b.eval
    case Add(a, b) => a.eval + b.eval

import Exp.{Num, Mul, Add}

enum Tree:
  case Tok(c: Char)
  case Paren(toks: List[Tree])

import Tree.{Tok, Paren}

def splitMatching(raw: String): (String, String) =
  val nestingDepth = raw.scanLeft(0) {
    case (depth, '(') => depth + 1
    case (depth, ')') => depth - 1
    case (depth, c)   => depth
  }
  val matchingIndex = nestingDepth.indexWhere(_ == 0, from = 1)
  val (inside, outside) = raw.splitAt(matchingIndex)
  inside.tail.init -> outside

def tokenize(rawExp: String): List[Tree] =
  rawExp match
    case flat if !flat.contains("(") =>
      flat.replaceAll(" ", "").map(Tok.apply).toList
    case nested =>
      val (leading, toSplit) = nested.splitAt(nested.indexOf("("))
      val (paren, trailing) = splitMatching(toSplit)
      tokenize(leading) ::: Paren(tokenize(paren)) :: tokenize(trailing)

def parse(tokens: List[Tree]): Exp =
  def parseTerm(tok: Tree): Exp = tok match
    case Paren(toks)         => parse(toks)
    case Tok(c) if c.isDigit => Num(c.asDigit)
    case Tok(c)              => throw Exception(s"expected digit, found: $c")

  val first = parseTerm(tokens.head)

  tokens.tail.grouped(2).foldLeft(first) { case (left, List(op, tok)) =>
    val right = parseTerm(tok)
    op match
      case Tok('*') => Mul(left, right)
      case Tok('+') => Add(left, right)
      case Tok(c)   => throw Exception(s"unexpected operator: $c")
  }

def parse2(tokens: List[Tree]): Exp =
  def sum(addends: List[Tree]): Exp =
    addends
      .collect {
        case Tok(d) if d.isDigit => Num(d.asDigit)
        case Paren(toks)         => parse2(toks)
      }
      .reduceLeft(Add.apply)

  Iterator
    .unfold(tokens) { remaining =>
      Option.when(remaining.nonEmpty) {
        val (term, next) = remaining.span(_ != Tok('*'))
        sum(term) -> next.drop(1)
      }
    }
    .reduceLeft(Mul.apply)

val ans1 = input.map(raw => parse(tokenize(raw)).eval).sum

val ans2 = input.map(raw => parse2(tokenize(raw)).eval).sum

tokenize("3 * (4 + (2 * 1) + 5) + 6")

tokenize("3 + 4")
parse(tokenize("3 + 4"))

parse(tokenize("(3 + 4) * 5"))

parse(tokenize("2 * (3 + 4)"))

parse(tokenize("2 * (3 + 4)")).eval

parse(tokenize("2 + 3 * 4"))

parse(tokenize("2 + 3 * 4")).eval

(tokenize andThen parse2)("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2").eval
