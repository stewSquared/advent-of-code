val input = io.Source.fromResource("2022/day-25.txt").getLines().toVector

def mod(n: Long) = (n + 2) % 5 - 2

def toNum(snafu: String): Long =
  snafu.foldLeft(0L)((n, c) => n * 5 + mod("012=-".indexOf(c)))

def toSnafu(num: Long): String =
  val snits = Iterator.unfold(num) { n =>
    Option.when(n > 0) {
      "012=-"((n % 5).toInt) -> (n - mod(n)) / 5
    }
  }
  snits.mkString.reverse

val ans = toSnafu(input.map(toNum).sum)
