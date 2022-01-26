val input = io.Source.fromResource("2020/day-03.txt").getLines.toList

val width = input(0).length

def cols = Iterator.from(0).map(_ * 3).map(_ % width)

cols.take(10) foreach println

val ans = input.zip(cols).count {
  case (row, col) => row(col) == '#'
}

println(ans)

val slopes = List(1, 3, 5, 7)
val ans2 =
  slopes.map { s =>
    val cols = Iterator.from(0).map(_ * s).map(_ % width)
    input.zip(cols).count {
      case (row, col) => row(col) == '#'
    }
  }.map(_.toLong).product * {
    val everyOther = input.zipWithIndex.collect {
      case (row, i) if i%2 == 0 => row
    }
    val cols = Iterator.from(0).map(_ % width)
    everyOther.zip(cols).count {
      case (row, col) => row(col) == '#'
    }.toLong
  }
