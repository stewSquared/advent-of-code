import aoc.*

val grid = io.Source.fromResource("2023/day-14.txt").getLines().toVector

val area = Area(grid)
area.width

val rounded: Set[Point] = area.pointsIterator.filter(grid(_) == 'O').toSet
val squared: Set[Point] =
  val real = area.pointsIterator.filter(grid(_) == '#').toSet
  val fake =
    val outer = area.expand(1)
    (outer.topBorder.points ++ outer.botBorder.points ++
      outer.leftBorder.points ++ outer.rightBorder.points).toSet
  real union fake

val areaSouth: Map[Point, Area] = Map.from:
  val areas = squared.map: squarePoint =>
    val points: Set[Point] = Iterator.iterate(squarePoint)(_.move(Dir.S)).drop(1)
      .takeWhile(area.contains)
      .takeWhile(!squared(_))
      .toSet
    Option.when(points.nonEmpty)(squarePoint -> Area.bounding(points))
  areas.flatten

val areaNorth: Map[Point, Area] = Map.from:
  val areas = squared.map: squarePoint =>
    val points: Set[Point] = Iterator.iterate(squarePoint)(_.move(Dir.N)).drop(1)
      .takeWhile(area.contains)
      .takeWhile(!squared(_))
      .toSet
    squarePoint -> (if points.nonEmpty then Area.bounding(points) else Area(0 until 0, 0 until 0))
    Option.when(points.nonEmpty)(squarePoint -> Area.bounding(points))
  areas.flatten

val areaEast: Map[Point, Area] = Map.from:
  val areas = squared.map: squarePoint =>
    val points: Set[Point] = Iterator.iterate(squarePoint)(_.move(Dir.E)).drop(1)
      .takeWhile(area.contains)
      .takeWhile(!squared(_))
      .toSet
    squarePoint -> (if points.nonEmpty then Area.bounding(points) else Area(0 until 0, 0 until 0))
    Option.when(points.nonEmpty)(squarePoint -> Area.bounding(points))
  areas.flatten

val areaWest: Map[Point, Area] = Map.from:
  val areas = squared.map: squarePoint =>
    val points: Set[Point] = Iterator.iterate(squarePoint)(_.move(Dir.W)).drop(1)
      .takeWhile(area.contains)
      .takeWhile(!squared(_))
      .toSet
    squarePoint -> (if points.nonEmpty then Area.bounding(points) else Area(0 until 0, 0 until 0))
    Option.when(points.nonEmpty)(squarePoint -> Area.bounding(points))
  areas.flatten

def roll(dir: Dir, p: Point, rounded: Set[Point]): Point =
  val occpuied = rounded union squared
  assert(rounded(p))
  Iterator.iterate(p)(_.move(dir)).drop(1)
    .takeWhile(area.contains)
    .takeWhile(!squared(_))
    .filter(!occpuied(_))
    .toList.lastOption.getOrElse(p)

def rollAll(start: Set[Point], dir: Dir): Set[Point] =
  // start.foldLeft(start):
  //   case (rounded, p) => rounded - p + roll(dir, p, rounded)
  dir match
    case Dir.N =>
      areaSouth.foldLeft(Set.empty[Point]):
        case (acc, (p, a)) =>
          val movingPoints = a.pointsIterator.toSet
          val movingCount = movingPoints.intersect(start).size
          val newPoints = (p.s.y to p.y + movingCount).map(Point(p.x, _)).toSet
          acc.union(newPoints)
    case Dir.E =>
      areaWest.foldLeft(Set.empty[Point]):
        case (acc, (p, a)) =>
          val movingPoints = a.pointsIterator.toSet
          val movingCount = movingPoints.intersect(start).size
          val newPoints = (p.w.x to p.x - movingCount by -1).map(Point(_, p.y)).toSet
          acc.union(newPoints)
    case Dir.W =>
      areaEast.foldLeft(Set.empty[Point]):
        case (acc, (p, a)) =>
          val movingPoints = a.pointsIterator.toSet
          val movingCount = movingPoints.intersect(start).size
          val newPoints = (p.e.x to p.x + movingCount).map(Point(_, p.y)).toSet
          acc.union(newPoints)
    case Dir.S =>
      areaNorth.foldLeft(Set.empty[Point]):
        case (acc, (p, a)) =>
          val movingPoints = a.pointsIterator.toSet
          val movingCount = movingPoints.intersect(start).size
          val newPoints = (p.n.y to p.y - movingCount by -1).map(Point(p.x, _)).toSet
          acc.union(newPoints)

show(rounded)
// areaSouth(Point(area.right, 1))

// areaSouth(Point(5, 0))
// areaSouth

def cycle(start: Set[Point]) =
  List(Dir.N, Dir.W, Dir.S, Dir.E).foldLeft(start)(rollAll)

def load(points: Set[Point]): Int =
  points.iterator.map(p => area.height - p.y).sum

show(rollAll(rounded, Dir.N))
rollAll(rounded, Dir.N).size
show(rollAll(rounded, Dir.S))
rollAll(rounded, Dir.S).size
show(rollAll(rounded, Dir.E))
rollAll(rounded, Dir.E).size
show(rollAll(rounded, Dir.W))
rollAll(rounded, Dir.W).size
2 + 2

val ans1 = load(rollAll(rounded, Dir.N))

val cycles = LazyList.iterate(rounded)(cycle)

val loads = cycles.map(load)

loads.take(100) foreach println

val hashes = cycles.map(_.hashCode())

val seenHashes = hashes.scanLeft(Set.empty[Int]):
  case (seen, hash) => (seen + hash)

val repeatIndex = seenHashes.zipWithIndex.collectFirst:
  case (hashes, index) if hashes.size < index => index

val repeatedCycle = cycles(repeatIndex.get)

val firstAppearance = hashes.indexWhere(_ == repeatedCycle.hashCode())

val cycleLength = repeatIndex.get - firstAppearance


(1_000_000_000 - firstAppearance) % cycleLength

val congruentIndex =
  firstAppearance + (1_000_000_000 - firstAppearance) % cycleLength

val ans2 = loads(congruentIndex)

// loads take 100 foreach println

def show(toShow: Set[Point]): Unit =
  println:
    area.draw: p =>
      if toShow(p) then 'O'
      else if squared(p) then '#' else '.'

// var current = rounded
// current = rollAll(current, Dir.N)
// show(current)
// current = rollAll(current, Dir.W)
// show(current)
// current = rollAll(current, Dir.S)
// show(current)
// current = rollAll(current, Dir.E)
// show(current)

// show(rollAll(rollAll(rollAll(rollAll(rounded, Dir.N), Dir.W), Dir.S), Dir.E))

show(cycles(1))
show(cycles(2))
show(cycles(3))

// val pushedWest = rollAll(rounded, Dir.W)
// val pushedEast = rollAll(rounded, Dir.E)
// val pushedSouth = rollAll(rounded, Dir.S)

// show(rollAll(rollAll(rounded, Dir.E), Dir.E))

// pushedWest == pushedEast
// pushedEast == pushedSouth

// show(pushedWest)
// show(pushedEast)
// show(pushedSouth)

// show(rounded)
// show(finalRounded)
