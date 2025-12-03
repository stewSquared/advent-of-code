val banks = io.Source.fromResource("2025/day-03.txt").getLines().toList

def joltage(bank: String, batteries: Int, acc: Long = 0L): Long =
  if batteries == 0 then acc else
    val digit = bank.take(bank.length - batteries + 1).map(_.asDigit).max
    val index = bank.indexOf(digit.toString)
    joltage(bank.drop(index + 1), batteries - 1, acc * 10 + digit)

val ans1 = banks.map(joltage(_, 2)).sum
val ans2 = banks.map(joltage(_, 12)).sum
