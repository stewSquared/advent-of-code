// val input = util.Using(io.Source.fromResource("2023/day-01.txt")): source =>
//   source.getLines().toList
// val lines = input.get

val input = io.Source.fromResource("2023/day-01.txt").getLines().toList

val digitWords = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

def firstDigit(s: String): Int =
  val digitIndex = s.indexOf(s.find(_.isDigit).get)
  val word = digitWords.map{ word =>
      word -> s.indexOf(word)
    }.filterNot(_._2 == -1)
    .minByOption(_._2)

  if word.exists(tup => tup._2 < digitIndex) then digitWords.indexOf(word.get._1)
  else s(digitIndex).asDigit

firstDigit("4qbbmlpmjx5fiveninepkcnqgqgdjsrzkgxjsxnkqnd2")
firstDigit("eight23n1teight2")

def lastDigit(s: String): Int =
  val digitIndex = s.lastIndexOf(s.findLast(_.isDigit).get)
  val word = digitWords.map{ word =>
      word -> s.lastIndexOf(word)
    }.filterNot(_._2 == -1)
    .maxByOption(_._2)

  println(digitIndex)
  println(word)
  if word.exists(tup => tup._2 > digitIndex) then digitWords.indexOf(word.get._1)
  else s(digitIndex).asDigit

firstDigit("two1nine")
lastDigit("two1nine")
firstDigit("abcone2threexyz")
lastDigit("abcone2threexyz")
firstDigit("4nineeightseven2")
lastDigit("4nineeightseven2")

input.map {
  line =>
    val first = line.find(_.isDigit).get
    val last = line.reverse.find(_.isDigit).get
    println(first)
    println(last)
    val num = first + last
} foreach println


val calNumbers = input.map {
  line =>
    val first = firstDigit(line)
    val last = lastDigit(line)
    s"$first$last".toInt
}

val ans2 = calNumbers.sum

input foreach println

// Notes: tried to add chars instead of concat string
// struggled to generalize reverse
// didn't fix everything in copy paste




//
