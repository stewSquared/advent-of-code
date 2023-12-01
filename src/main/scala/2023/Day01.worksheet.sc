val input = io.Source.fromResource("2023/day-01.txt").getLines().toList

def digitIndices(s: String): Seq[(Int, Int)] =
  s.zipWithIndex.collect:
    case (c, i) if c.isDigit => (c.asDigit, i)

val digitWords = Vector("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
def wordToDigit(word: String): Int = digitWords.indexOf(word)

def wordIndices(s: String): Seq[(Int, Int)] =
  digitWords.flatMap: word =>
    val occurences = Iterator.unfold(0): index =>
      val i = s.indexOf(word, index)
      Option.when(i != -1):
        (wordToDigit(word), i) -> (i + 1)
    occurences.toVector

val calibrationNumbers = input.map: line =>
  val indices = (wordIndices(line) ++ digitIndices(line))
  val (first, _) = indices.minBy(_._2)
  val (last, _) = indices.maxBy(_._2)
  first * 10 + last

val ans = calibrationNumbers.sum
