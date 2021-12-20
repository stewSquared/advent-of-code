import collection.immutable.BitSet

val input = io.Source.fromResource("2021/day-20-1.txt").getLines.toVector

val alg = BitSet.fromSpecific(
  input.head.iterator.zipWithIndex.collect { case ('#', i) => i }
)

val rawImage = input.drop(2)

final val MAX_ITERS = 50
final val MAX_WIDTH = rawImage(0).size + MAX_ITERS * 2

case class Area(left: Int, right: Int, top: Int, bot: Int):
  def expand(n: Int): Area =
    copy(left - n, right + n, top - n, bot + n)
  val xRange = left until right
  val yRange = top until bot

case class Point(x: Int, y: Int):
  def toInt = Point.toInt(x, y)
  def nine: Seq[Point] =
    for
      dy <- -1 to 1
      dx <- -1 to 1
    yield Point(x + dx, y + dy)

object Point:
  def toInt(x: Int, y: Int): Int =
    (y + MAX_ITERS) * (MAX_WIDTH) + (x + MAX_ITERS)

  def fromInt(n: Int): Point =
    Point(n / MAX_WIDTH - MAX_ITERS, n % MAX_WIDTH - MAX_ITERS)

  extension (image: BitSet) def apply(p: Point) = image(p.toInt)

case class Image(grid: BitSet, inverted: Boolean, area: Area):
  def apply(p: Point) = litAt(p)

  def litAt(p: Point): Boolean = grid(p) ^ inverted

  def numLit: Int =
    if inverted then Int.MaxValue else grid.size

  def numDark: Int =
    if inverted then grid.size else Int.MaxValue

  def show: String =
    val col = for y <- area.yRange yield
      val row =
        for x <- area.xRange
        yield if litAt(Point(x, y)) then '#' else '.'
      row.mkString
    col.mkString("\n")

  def index(p: Point): Int =
    val bits = p.nine.map(q => if litAt(q) then '1' else '0')
    Integer.parseInt(bits.mkString, 2)

  def enhance: Image =
    val nextInverted = if inverted then alg(511) else alg(0)
    val newArea = area.expand(1)
    val newPoints = for
      y <- newArea.yRange
      x <- newArea.xRange
      p = Point(x, y)
      if alg(index(p)) ^ nextInverted
    yield p.toInt
    val newGrid = BitSet.fromSpecific(newPoints)
    Image(newGrid, nextInverted, newArea)

val image0: Image =
  val area = Area(0, rawImage(0).size, 0, rawImage.size)
  val pointIndices = for
    y <- area.yRange.iterator
    x <- area.xRange.iterator
    if rawImage(y)(x) == '#'
  yield Point.toInt(x, y)
  val grid = BitSet.fromSpecific(pointIndices)
  Image(grid, inverted = false, area)

val images = LazyList.iterate(image0, 51)(_.enhance)

val ans1 = images(2).numLit
val ans2 = images(50).numLit

// tests

Point(-MAX_ITERS, -MAX_ITERS).toInt
Point(MAX_WIDTH - MAX_ITERS - 1, -MAX_ITERS).toInt
Point(-MAX_ITERS, -MAX_ITERS + 1).toInt
Point(MAX_WIDTH - MAX_ITERS - 1, rawImage.size - MAX_ITERS - 1).toInt

Point.fromInt(Point(0, 0).toInt)

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
