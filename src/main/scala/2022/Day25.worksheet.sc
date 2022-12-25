val input = io.Source.fromResource("2022/day-25.txt").getLines().toVector

val snits = "=-012"
def toSnit(digit: Int): Char = snits(digit + 2)
def toDigit(snit: Char): Int = snits.indexOf(snit) + 2

def toNum(snafu: String): Long =
  snafu.foldLeft(0L)((n, s) => n * 5 + toDigit(s))

def toSnafu(num: Long): String =
  val snitsLE = Iterator.unfold(num) { n =>
    Option.when(n != 0) {
      val s = math.floorMod(n + 2, 5).toInt - 2
      toSnit(s) -> (n - s) / 5
    }
  }
  if snitsLE.isEmpty then "0"
  else snitsLE.mkString.reverse

val ans = toSnafu(input.map(toNum).sum)
