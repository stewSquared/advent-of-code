val steps =
  io.Source.fromResource("2021/day-22-1.txt").getLines.toList.collect {
    case s"$step x=${Range(xs)},y=${Range(ys)},z=${Range(zs)}" =>
      Step(step == "on", Area(xs, ys, zs))
  }

case class Step(on: Boolean, area: Area):
  def countLastTouched(future: List[Step]): Long =
    val overlaps = future.flatMap(_.area.intersect(area))
    val touchedLater = Area.unionSize(overlaps)
    area.size - touchedLater

case class Range(min: Int, max: Int):
  override def toString = s"$min..$max"

  def size = max + 1 - min
  def contains(n: Int) = min <= n && n <= max
  def supersetOf(r: Range) = min <= r.min && r.max <= max

  def intersect(r: Range): Option[Range] =
    if supersetOf(r) then Some(r)
    else if r contains min then Some(copy(max = max.min(r.max)))
    else if r contains max then Some(copy(min = min.max(r.min)))
    else None

  def diff(r: Range): List[Range] =
    intersect(r).fold(List(this)) { o =>
      List(
        Option.when(min < o.min)(min to (o.min - 1)),
        Option.when(o.max < max)((o.max + 1) to max)
      ).flatten
    }

object Range:
  def apply(n1: Int, n2: Int): Range =
    new Range(n1 min n2, n1 max n2)

  def unapply(s: String) = s match
    case s"$n1..$n2" => n1.toIntOption.zip(n2.toIntOption).map(apply)

extension (n1: Int) def to(n2: Int): Range = Range(n1, n2)

case class Area(xs: Range, ys: Range, zs: Range):
  def size: Long = xs.size.toLong * ys.size.toLong * zs.size.toLong

  def intersect(other: Area): Option[Area] =
    for
      xo <- xs intersect other.xs
      yo <- ys intersect other.ys
      zo <- zs intersect other.zs
    yield Area(xo, yo, zo)

  def diff(other: Area): List[Area] =
    intersect(other).fold(List(this)) { o =>
      for
        xr <- o.xs :: (xs diff other.xs)
        yr <- o.ys :: (ys diff other.ys)
        zr <- o.zs :: (zs diff other.zs)
        a = Area(xr, yr, zr)
        if a != o
      yield a
    }

object Area:
  def unionSize(areas: Seq[Area]): Long =
    val union = areas.sortBy(_.size).foldLeft[List[Area]](Nil) {
      (disjoint, a) => a :: disjoint.flatMap(_ diff a)
    }
    union.map(_.size).sum

def suffixMap[A](elems: List[A]): Map[A, List[A]] =
  val suffixes = elems.scanRight[List[A]](Nil)(_ :: _)
  elems.zip(suffixes.tail).toMap

def countLit(steps: List[Step]) =
  val onStepToFuture = suffixMap(steps).filter(_._1.on)
  onStepToFuture.transform(_ countLastTouched _).values.sum

val initArea = Area(-50 to 50, -50 to 50, -50 to 50)
val initSteps =
  steps.flatMap(s => s.area.intersect(initArea).map(a => s.copy(area = a)))

val ans1 = countLit(initSteps)
val ans2 = countLit(steps)

// tests

(3 to 7) intersect (1 to 5)
(3 to 7) intersect (3 to 7)
(3 to 7) intersect (5 to 9)
(3 to 7) intersect (1 to 9)
(3 to 7) intersect (4 to 6)
(3 to 7) intersect (1 to 2)
(3 to 7) intersect (8 to 9)

(3 to 7) diff (1 to 5)
(3 to 7) diff (3 to 7)
(3 to 7) diff (5 to 9)
(3 to 7) diff (1 to 9)
(3 to 7) diff (4 to 6)
(3 to 7) diff (1 to 2)
(3 to 7) diff (8 to 9)

def Cube(r: Range) = Area(r, r, r)
def dim(a: Area) =
  val dims = List(a.xs, a.ys, a.zs)
  dims.map(_.size).sorted.mkString("x")

val cube4 = Cube(1 to 4)
val corner = Cube(3 to 10)
val center = Cube(2 to 3)

cube4 intersect cube4
cube4 intersect corner
cube4 intersect center

val cornerDiff = cube4 diff corner
val centerDiff = cube4 diff center

cube4 diff cube4

cornerDiff map dim
cornerDiff.size
cornerDiff.map(_.size).sum

4 * 4 * 4 - 2 * 2 * 2

centerDiff.groupMapReduce(dim)(_ => 1)(_ + _)
centerDiff.size
centerDiff.map(_.size).sum

val areas =
  for
    xs <- List(1 to 5, 3 to 7, 5 to 9)
    ys <- List(1 to 5, 3 to 7, 5 to 9)
    zs <- List(1 to 5, 3 to 7, 5 to 9)
  yield Area(xs, ys, zs)

Area.unionSize(areas)
Area(1 to 9, 1 to 9, 1 to 9).size
