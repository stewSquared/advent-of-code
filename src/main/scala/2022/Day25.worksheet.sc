val input = io.Source.fromResource("2022/day-25.txt").getLines().toVector

def toDigit(c: Char): Int = c match
  case '-' => -1
  case '=' => -2
  case d => d.asDigit

def toNum(snafu: String): Long =
  snafu.reverse.zipWithIndex.map {
    case (c, i) => math.pow(5, i).toLong * toDigit(c)
  }.sum

def toSnafu(long: Long): String =
  val (number -> carry) = BigInt(long).toString(5).reverse.foldLeft("" -> false) {
    case ((number, carry), digit) =>
      println(digit)
      digit.asDigit + (if carry then 1 else 0) match
        case 5 => ("0" + number) -> true
        case 4 => ("-" + number) -> true
        case 3 => ("=" + number) -> true
        case n => (n.toString + number) -> false
  }
  if carry then "1" + number else number

input.map(toNum).sum
val ans = toSnafu(input.map(toNum).sum)
toNum("122-0==-=211==-2-200")
// 122-0==-=211==-2-200

var n = 976
toSnafu(n)
toNum(toSnafu(n))
BigInt(n).toString(5)
