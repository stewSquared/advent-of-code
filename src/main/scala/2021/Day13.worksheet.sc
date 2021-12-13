case class Point(x: Int, y: Int):
  def foldX(line: Int): Point = copy(x = if x > line then 2 * line - x else x)
  def foldY(line: Int): Point = copy(y = if y > line then 2 * line - y else y)

val input = io.Source.fromResource("2021/day-13-1.txt").getLines.toList

val dots = input.collect { case s"$x,$y" => Point(x.toInt, y.toInt) }.toSet

val instructions = input.collect[Point => Point] {
  case s"fold along x=$line" => _.foldX(line.toInt)
  case s"fold along y=$line" => _.foldY(line.toInt)
}.toList

val ans1 = dots.map(instructions.head).size

val code = instructions.foldLeft(dots)(_ map _)

val ans2 = for y <- 0 to code.map(_.y).max yield
  for x <- 0 to code.map(_.x).max
  yield if code(Point(x, y)) then "#" else "."

ans2.foreach(row => println(row.mkString))
// PERCGJPB
