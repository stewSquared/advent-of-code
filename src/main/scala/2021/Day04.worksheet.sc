val input = io.Source.fromResource("2021/day-04-1.txt").getLines.toVector

type Board = Vector[Vector[Int]]

val numbers = input.head.split(",").map(_.toInt).toList

val boards: List[Board] = input.tail
  .filter(_.nonEmpty)
  .grouped(5)
  .map(_.map(_.strip.split(" ").flatMap(_.toIntOption).toVector))
  .toList

extension (board: Board)
  def bingo(marked: List[Int]): Boolean = {
    val rowBingo = board.exists(_.forall(marked.contains))
    val colBingo = board.transpose.exists(_.forall(marked.contains))
    rowBingo || colBingo
  }

val prefixes = numbers.scanLeft(List[Int]())(_ :+ _)

def score(board: Board): Option[(Int, Int)] = {
  prefixes.find(board.bingo).map { marked =>
    board.flatten.filterNot(marked.contains).sum * marked.last -> marked.size
  }
}

val ans1 = boards.flatMap(score).minBy(_._2)._1
val ans2 = boards.flatMap(score).maxBy(_._2)._1
