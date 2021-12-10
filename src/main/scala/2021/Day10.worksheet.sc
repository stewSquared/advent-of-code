import io.Source
import util.Using
import util.chaining.*

val input = Using(Source.fromResource("2021/day-10-1.txt")) {
  _.getLines.toList
}.get

val example = "{([(<{}[<>[]}>{[]{[(<()>"

"{([(<{}[<>[]}>{[]{[(<()>"
"{([(<{}[<>[]}>{[]{[(<()>"
"{([(<{}[<>[]}>{[]{[(<()>"

val score: Map[Char, Int] = Map(
  ')' -> 3,
  ']' -> 57,
  '}' -> 1197,
  '>' -> 25137
)
val score2: Map[Char, Int] = Map(
  ')' -> 1,
  ']' -> 2,
  '}' -> 3,
  '>' -> 4
)



val isClose = ">]})".toSet
val isOpen = "({[<".toSet

def matching(open: Char, close: Char) = (open, close) match {
  case ('[', ']') => true
  case ('<', '>') => true
  case ('{', '}') => true
  case ('(', ')') => true
  case _ => false
}

def findError(chunks: String): Option[Char] = {
  def recur(open: List[Char], remaining: List[Char]): Option[Char] = {
    println(open.mkString + " - " + remaining.mkString)
    val x = remaining match {
      case Nil =>
        println("yay we done")
        None
      case next :: rest if isOpen(next) => recur(next :: open, rest)
      case next :: rest =>
        open.headOption match {
          case Some(o) if matching(o, next) => recur(open.tail, rest)
          case Some(o) => Some(next)
          case _ => None
        }
    }
    println(x)
    x
  }
  recur(Nil, chunks.toList)
}
findError(example)

findError("{([(<{}[<>[]}>{[]{[(<()>")
findError("[[<[([]))<([[{}[[()]]]")
findError("[{[{({}]{}}([{[{{{}}([]")
findError("[<(<(<(<{}))><([]([]()")
findError("<{([([[(<>()){}]>(<<{{")



findError("<>")
findError("()")

input.size
input.flatMap(findError).size

val ans = input.flatMap(findError).map(score).sum

input.filter(findError(_).nonEmpty) foreach println

import util.{Either, Right, Left}

def findUnmatched(chunks: String): Either[Char, List[Char]] = {
  def recur(open: List[Char], remaining: List[Char]): Either[Char, List[Char]] = {
    println(open.mkString + " - " + remaining.mkString)
    remaining match {
      case Nil => Right(open)
      case next :: rest if isOpen(next) => recur(next :: open, rest)
      case next :: rest =>
        open.headOption match {
          case Some(o) if matching(o, next) => recur(open.tail, rest)
          case Some(o) => Left(next)
          case _ => Right(Nil)
        }
    }
  }
  recur(Nil, chunks.toList)
}

val incomplete = input
  .map(findUnmatched)
  .filterNot(_.isLeft)
  .flatMap(_.toOption)

incomplete.size

val matches = Map[Char, Char](
  '[' -> ']',
  '<' -> '>',
  '{' -> '}',
  '(' -> ')'
)

incomplete.map(_.mkString) foreach println

val completions = incomplete.map(_.map(matches))

completions.map(_.mkString) foreach println

def scoring(closing: List[Char]): Long = {
  closing.foldLeft(0L)((score, next) =>
    score * 5 + score2(next).toLong
  )
}

scoring(completions.last)

completions.size

completions.map(c => c.mkString -> scoring(c)).foreach((c,s) => println(s"$c - $s"))

val ans2 = completions.map(scoring).sorted.drop(completions.size / 2).head

println(ans2)

incomplete.size
