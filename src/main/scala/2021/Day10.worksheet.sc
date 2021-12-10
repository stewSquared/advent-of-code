import io.Source
import util.Using
import util.chaining.*
import util.{Either, Right, Left}

object Token:
  opaque type Open = Char
  opaque type Close = Char
  type Chunk = Either[Close, List[Close]]

  val closeOf: Map[Open, Close] =
    Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

  object Open:
    def unapply(c: Char): Option[Open] = closeOf.keys.find(_ == c)

  object Close:
    def unapply(c: Char): Option[Close] = closeOf.values.find(_ == c)

  val errorScore: Map[Close, Int] = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  def completionScore(completion: List[Close]): Long =
    completion.foldLeft(0L) { (s, c) =>
      s.toLong * 5 + " )]}>".indexOf(c)
    }

import Token.*

val chunks: List[Chunk] = Using(Source.fromResource("2021/day-10-1.txt")) {
  _.getLines.toList.map(parseLine)
}.get

def parseLine(line: String) = parse(line.toList, Nil)

def parse(tokens: List[Char], completion: List[Close]): Chunk = tokens match
  case Nil                => Right(completion)
  case Open(open) :: ts   => parse(ts, closeOf(open) :: completion)
  case Close(close) :: ts => completion match
    case `close` :: unmatched => parse(ts, unmatched)
    case _                    => Left(close)
  case invalid :: ts => parse(ts, completion)

val ans1 = chunks.flatMap(_.swap.toOption).map(errorScore).sum

val completions = chunks.flatMap(_.toOption)
val ans2 = completions.map(completionScore).sorted.apply(completions.size / 2)

//////////////// test ////////////////

val corrupted = chunks.filter(_.isLeft)
corrupted foreach println

def error(chunk: String) = parseLine(chunk).swap.toOption.get

println(error("{([(<{}[<>[]}>{[]{[(<()>"))
println(error("[[<[([]))<([[{}[[()]]]"))
println(error("[{[{({}]{}}([{[{{{}}([]"))
println(error("[<(<(<(<{}))><([]([]()"))
println(error("<{([([[(<>()){}]>(<<{{"))

val incomplete = chunks.filter(_.isRight)
incomplete foreach println

def completion(chunk: String) = parseLine(chunk).toOption.get

println(completion("[({(<(())[]>[[{[]{<()<>>").mkString)
println(completion("[(()[<>])]({[<{<<[]>>(").mkString)
println(completion("(((({<>}<{<{<>}{[]{[]{}").mkString)
println(completion("{<[[]]>}<{[{[{[]{()[[[]").mkString)
println(completion("<{([{{}}[<[[[<>{}]]]>[]]").mkString)

incomplete.flatMap(_.toSeq).map(completionScore)
