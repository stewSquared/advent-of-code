import collection.immutable.BitSet

val input = io.Source.fromResource("2021/day-20-1.txt").getLines.toVector

val alg = BitSet.fromSpecific(
  input.head.iterator.zipWithIndex.collect { case ('#', i) => i }
)

val rawImage = input.drop(2)

case class Area(left: Int, right: Int, top: Int, bot: Int):
  def expand(n: Int): Area =
    copy(left - n, right + n, top - n, bot + n)
  val xRange = left until right
  val yRange = top until bot
  val width = right - left
  val height = top - bot

  def pointToInt(x: Int, y: Int): Option[Int] =
    val inBounds = xRange.contains(x) && yRange.contains(y)
    if !inBounds then None
    else Some((y - top) * width + (x - left))

case class Point(x: Int, y: Int):
  def toInt(a: Area) = a.pointToInt(x, y)
  def nine: Seq[Point] =
    for
      dy <- -1 to 1
      dx <- -1 to 1
    yield Point(x + dx, y + dy)

case class Image(grid: BitSet, inverted: Boolean, area: Area):
  def litAt(p: Point) = p.toInt(area).exists(grid) ^ inverted

  def numLit = if inverted then Int.MaxValue else grid.size
  def numDark = if inverted then grid.size else Int.MaxValue

  def show: String =
    val lines = for y <- area.yRange yield
      val line =
        for x <- area.xRange
        yield if litAt(Point(x, y)) then '#' else '.'
      line.mkString
    lines.mkString("\n")

  def index(p: Point): Int =
    val bits = p.nine.map(q => if litAt(q) then '1' else '0')
    Integer.parseInt(bits.mkString, 2)

  def enhance: Image =
    val nextInverted = if inverted then alg(511) else alg(0)
    val nextArea = area.expand(1)
    val nextPoints = for
      y <- nextArea.yRange
      x <- nextArea.xRange
      p = Point(x, y)
      if alg(index(p)) ^ nextInverted
    yield p.toInt(nextArea).get
    val nextGrid = BitSet.fromSpecific(nextPoints)
    Image(nextGrid, nextInverted, nextArea)

val image0: Image =
  val area = Area(0, rawImage(0).size, 0, rawImage.size)
  val points = for
    y <- area.yRange.iterator
    x <- area.xRange.iterator
    if rawImage(y)(x) == '#'
  yield area.pointToInt(x, y).get
  val grid = BitSet.fromSpecific(points)
  Image(grid, inverted = false, area)

val images = LazyList.iterate(image0, 51)(_.enhance)

val ans1 = images(2).numLit
val ans2 = images(50).numLit

// tests

val a0 = Area(0, 10, 0, 10)
Point(a0.xRange(0), a0.yRange(0)).toInt(a0)
Point(a0.xRange.last, a0.yRange(0)).toInt(a0)
Point(a0.xRange(0), a0.yRange(1)).toInt(a0)

val a50 = a0.expand(50 * 2)
Point(a50.xRange(0), a50.yRange(0)).toInt(a50)
Point(a50.xRange.last, a50.yRange(0)).toInt(a50)
Point(a50.xRange(0), a50.yRange(1)).toInt(a50)

image0.inverted
image0.numLit
image0.numDark
println(image0.show)

val image1 = images(1)
image1.inverted
image1.numLit
image1.numDark
println(image1.show)

val image2 = images(2)
image2.inverted
image2.numLit
image2.numDark
println(image2.show)
