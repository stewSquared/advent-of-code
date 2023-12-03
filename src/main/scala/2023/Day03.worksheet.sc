import aoc.*

val grid = io.Source.fromResource("2023/day-03.txt").getLines().toList

val symbols: Map[Point, Char] =
  val points = for
    (row, y) <- grid.zipWithIndex
    (char, x) <- row.zipWithIndex
    if !char.isDigit && char != '.'
  yield
    Point(x, y) -> char
  points.toMap

def numberRanges(row: String): List[(Int, Range)] =
  val nums = Iterator.unfold(row, 0): (rest, index) =>
    Option.when(rest.exists(_.isDigit)):
      val (pre, numStart) = rest.span(!_.isDigit)
      val (digits, post) = numStart.span(_.isDigit)
      val numStartIndex = index + pre.length
      val nextIndex = numStartIndex + digits.length
      (digits.toInt, numStartIndex until nextIndex) -> (post, nextIndex)
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
