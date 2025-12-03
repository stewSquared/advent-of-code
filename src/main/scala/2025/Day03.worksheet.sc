val input = io.Source.fromResource("2025/day-03.txt").getLines().toList

val banks = input.map(s => IArray.from(s.iterator.map(_.asDigit)))

def joltage(bank: IArray[Int], batteries: Int): Long =
  val indices = (batteries to 1 by -1).scanLeft(-1):
    case (i, b) => (i + 1 to bank.length - b).maxBy(bank)
  indices.tail.map(bank).mkString.toLong

val ans1 = banks.map(joltage(_, 2)).sum
val ans2 = banks.map(joltage(_, 12)).sum
