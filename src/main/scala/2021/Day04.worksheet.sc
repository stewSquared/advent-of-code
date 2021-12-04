import io.Source
import util.Using

val input = Using(Source.fromResource("2021/day-04-1.txt")) {
  _.getLines.toVector
}.get

val numbers = input.head.split(",").map(_.toInt).toList

type Board = Vector[Vector[Int]]

val boards: List[Board] = input.tail.filter(_.nonEmpty).grouped(5)
  .map(_.map(_.strip.split(" ").flatMap(_.toIntOption).toVector))
  .toList

val prefixes = numbers.scanLeft(List[Int]())(_ :+ _)

def bingo(board: Board, marked: List[Int]) = {
  val rowBingo = board.exists(_.forall(marked.contains))
  val colBingo = board.transpose.exists(_.forall(marked.contains))
  rowBingo || colBingo
}

def bingoNumbers(board: Board) = prefixes.dropWhile(!bingo(board, _)).head

def score(board: Board, marked: List[Int]) = {
  board.flatten.filterNot(marked.contains).sum * marked.last
}

val ans1 = boards
  .map(b => b -> bingoNumbers(b))
  .minByOption(_._2.length)
  .map(score)

val ans2 = boards
  .map(b => b -> bingoNumbers(b))
  .maxByOption(_._2.length)
  .map(score)
