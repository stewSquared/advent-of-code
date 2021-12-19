import util.Using
import io.Source
import collection.mutable.ListBuffer

val scanners = Using(Source.fromResource("2021/day-19-1.txt")) { source =>
  val lines = source.getLines()
  val lb = new ListBuffer[Scanner]
  while lines.hasNext do
    val s"---$label---" = lines.next()
    val points = lines
      .takeWhile(_.nonEmpty)
      .collect { case s"$x,$y,$z" =>
        Point(x.toInt, y.toInt, z.toInt)
      }
      .toList
    lb += Scanner(name = label.strip, beacons = points.toSet)
  lb.result
}.get

case class Point(x: Int, y: Int, z: Int):
  def rotX: Point = copy(y = z, z = -y)
  def rotY: Point = copy(z = x, x = -z)
  def rotZ: Point = copy(x = y, y = -x)
  def translate(p: Point): Point =
    copy(x = x + p.x, y = y + p.y, z = z + p.z)

  def -(p: Point): Point =
    Point(x - p.x, y - p.y, z - p.z)

  def manhattanDist(p: Point): Int =
    (x - p.x).abs + (y - p.y).abs + (z - p.z).abs

case class Scanner(
    name: String,
    beacons: Set[Point],
    origin: Point = Point(0, 0, 0)
):
  def rotX = copy(beacons = beacons.map(_.rotX), origin = origin.rotX)
  def rotY = copy(beacons = beacons.map(_.rotY), origin = origin.rotY)
  def rotZ = copy(beacons = beacons.map(_.rotZ), origin = origin.rotZ)

  def translate(t: Point) =
    copy(beacons = beacons.map(_.translate(t)), origin = origin.translate(t))

  def intersections(that: Scanner): Set[Point] =
    this.beacons intersect that.beacons

  def rotations: Iterator[Scanner] =
    for
      p <- List(this, rotX, rotX.rotX, rotX.rotX.rotX).iterator
      q <- List(p, p.rotZ.rotZ)
      r <- List(q, q.rotY, q.rotZ)
    yield r

  def locate(that: Scanner): Option[Scanner] =
    val translations = for
      s2r <- that.rotations
      b1 <- beacons
      b2 <- s2r.beacons
    yield s2r.translate(b1 - b2)
    translations.find(_.beacons.intersect(beacons).sizeIs >= 12)

val distances = scanners.map { s =>
  s.name -> s.beacons.toList
    .combinations(2)
    .collect { case List(p, q) => p.manhattanDist(q) }
    .toSet
}.toMap

def overlap(s1: Scanner, s2: Scanner): Boolean =
  distances(s1.name).intersect(distances(s2.name)).sizeIs >= 12 * 11 / 2

val oriented =
  val search = Iterator.iterate(scanners.splitAt(1)) { (from, toCheck) =>
    toCheck.partitionMap { c =>
      from.find(overlap(_, c)).flatMap(_.locate(c)).toLeft(c)
    }
  }
  search.map(_._1).takeWhile(_.nonEmpty).toList.flatten

val beacons = oriented.map(_.beacons).reduce(_ union _)
val ans1 = beacons.size

val origins = oriented.map(_.origin)

val ans2 = origins
  .combinations(2)
  .collect { case List(p, q) => p.manhattanDist(q) }
  .max
