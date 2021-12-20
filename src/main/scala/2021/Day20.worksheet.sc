import collection.immutable.BitSet

val input = io.Source.fromResource("2021/day-20-1.txt").getLines.toList

val alg = BitSet.fromSpecific(
  input.head.iterator.zipWithIndex.filter(_._1 == '#').map(_._2)
)

val rawImage = input.drop(2)

val width = rawImage(0).size
val height = rawImage.size

val maxOff = 25 * 4 + 2
val maxWidth =
  width + maxOff * 2

case class Point(x: Int, y: Int):
  def toInt = (y+maxOff)*(maxWidth) + (x+maxOff)

Point(-maxOff,-maxOff).toInt
Point(width+maxOff-1, -maxOff).toInt
Point(-maxOff,-maxOff + 1).toInt
Point(width+maxOff-1,height+maxOff+1).toInt

object Point:
  def fromInt(n: Int) =
    Point(n / maxWidth - maxOff, n%maxWidth - maxOff)

Point.fromInt(Point(0,0).toInt)

extension (image: BitSet)
  def apply(p: Point) = image(p.toInt)

def nine(p: Point): Seq[Point] =
  for
    dy <- -1 to 1
    dx <- -1 to 1
  yield Point(p.x + dx, p.y + dy)

def next(p: Point, image: BitSet): Boolean =
  val bits = nine(p).map(q => if image(q) then '1' else '0')
  val index = Integer.parseInt(bits.mkString, 2)
  alg(index)

// .......#
// #####???
// ...?????
// double enhance changes 4 out
// it needs an intermediate step that is 6 out

def doubleEnhance(image: BitSet, iters: Int): BitSet =
  val left = 0 - iters * 4
  val right = width + iters * 4
  val top = 0 - iters * 4
  val bot = height + iters * 4

  val enhance1 =
    val o = 2
    val points = for
      y <- ((top - o) until (bot + o)).iterator
      x <- ((left - o) until (right + o)).iterator
      p = Point(x,y)
      if next(p, image)
    yield p.toInt
    BitSet.fromSpecific(points)

  val newPoints = for
    y <- (top until bot).iterator
    x <- (left until right).iterator
    p = Point(x,y)
    if next(p, enhance1)
  yield p.toInt
  BitSet.fromSpecific(newPoints)

val image0: BitSet =
  val pointIndices = for {
    (line, y) <- rawImage.zipWithIndex.iterator
    case ('#', x) <- line.zipWithIndex.iterator
  } yield Point(x,y).toInt
  BitSet.fromSpecific(pointIndices)

for y <- 0 until 5 do
  val points = for x <- 0 until 5 yield(Point(x,y))
  val pixels = points.map(p =>if image0(p) then '#' else '.')
  println(pixels.mkString)

val image1 =
  val image = image0
  val iters = 1

  val left = 0 - iters * 5
  val right = width + iters * 5
  val top = 0 - iters * 5
  val bot = height + iters * 5

  val points = for
    y <- ((top + 2) until (bot - 2)).iterator
    x <- ((left + 2) until (right - 2)).iterator
    p = Point(x,y)
    if next(p, image)
  yield p.toInt
  BitSet.fromSpecific(points)

for y <- -3 until 8 do
  val points = for x <- -3 until 8 yield(Point(x,y))
  val pixels = points.map(p =>if image1(p) then '#' else '.')
  println(pixels.mkString)

val image2 = doubleEnhance(image0, 1)

for y <- -5 until 10 do
  val points = for x <- -5 until 10 yield(Point(x,y))
  val pixels = points.map(p =>if image2(p) then '#' else '.')
  println(pixels.mkString)

def images() = Iterator.iterate(image0 -> 1) {
  (image, i) => doubleEnhance(image, i) -> (i+1)
}.map(_._1)

assert(images().drop(1).next() == image2)

val ans1 = image2.size
val ans2 = images().drop(25).next().size
