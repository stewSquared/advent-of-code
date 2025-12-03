val input = io.Source.fromResource("2025/day-03.txt").getLines().toList

val banks = input.map(s => IArray.from(s.iterator.map(_.asDigit)))

def joltage(bank: IArray[Int], batteries: Int): Long =
  def search(start: Int, batteries: Int, acc: Long): Long =
    if batteries == 0 then acc else
      val index = (start to bank.length - batteries).maxBy(bank)
      search(index + 1, batteries - 1, acc * 10 + bank(index))
  search(0, batteries, 0L)

val ans1 = banks.map(joltage(_, 2)).sum
val ans2 = banks.map(joltage(_, 12)).sum
