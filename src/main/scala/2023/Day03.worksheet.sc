import aoc.*

val grid = io.Source.fromResource("2023/day-03.txt").getLines().toVector

val area = Area(grid)

val symbols = Map.from[Point, Char]:
  area.pointsIterator.map(p => p -> grid(p)).filterNot:
    case (p, c) => c.isDigit || c == '.'

def numberRanges(row: String): List[(Int, Range)] =
  val nums = Iterator.unfold(row, 0): (rest, index) =>
    Option.when(rest.nonEmpty && rest.exists(_.isDigit)):
      val (pre, numStart) = rest.span(!_.isDigit)
      val (digits, post) = numStart.span(_.isDigit)
      val num = digits.toInt
      val numMinIndex = index + pre.length
      val numMaxIndex = numMinIndex + digits.length
      val range = numMinIndex until numMaxIndex
      (num, range) -> (post, numMaxIndex)
  nums.toList

val numberAreas = for
    (row, y) <- grid.zipWithIndex
    (num, range) <- numberRanges(row)
  yield
    num -> Area(range, y to y).expand(1)

val partNumbers = numberAreas.collect:
  case (num, area) if symbols.keysIterator.exists(area.contains) => num

val ans1 = partNumbers.sum

val gears = symbols.filter(_._2 == '*').keysIterator.toList

val ratios = gears.map: point =>
  numberAreas.collect:
    case (num, area) if area.contains(point) => num

val ans2 = ratios.filter(_.size == 2).map(_.product).sum
