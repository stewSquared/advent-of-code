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
  def search(num: Int, rest: String, index: Int, start: Option[Int]): List[(Int, Range)] =
    if rest.isEmpty then start.map(start => List(num -> (start until index))).getOrElse(Nil)
    else if rest.head.isDigit then
      search(num * 10 + rest.head.asDigit, rest.tail, index + 1, start.orElse(Some(index)))
    else
      val range = start.map(start => num -> (start until index))
      val continue = search(0, rest.tail, index + 1, None)
      range.map(_ :: continue).getOrElse(continue)
  search(0, row, 0, None)

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
