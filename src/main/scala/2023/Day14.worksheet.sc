import aoc.*

val grid = io.Source.fromResource("2023/day-14.txt").getLines().toVector
val area = Area(grid)

val rounded: Set[Point] = area.pointsIterator.filter(grid(_) == 'O').toSet
val squared: Set[Point] =
  val real = area.pointsIterator.filter(grid(_) == '#').toSet
  val outer = area.expand(1).diff(area).flatMap(_.pointsIterator).toSet
  real union outer

val openArea = Map.from[Dir, Map[Point, Area]]:
  Dir.values.map: dir =>
    dir -> Map.from:
      squared.flatMap: squarePoint =>
        val points = Set.from:
          Iterator.iterate(squarePoint)(_.move(dir)).drop(1)
            .takeWhile(area.contains)
            .takeWhile(!squared(_))
        Option.when(points.nonEmpty)(squarePoint -> Area.bounding(points))

def rollAll(start: Set[Point], dir: Dir): Set[Point] =
  openArea.apply(dir.reverse).foldLeft(Set.empty[Point]):
    case (acc, (p, a)) =>
      val movingCount = a.pointsIterator.count(start)
      acc union Set.from:
        dir match
          case Dir.N => (p.s.y to p.y + movingCount).map(Point(p.x, _))
          case Dir.W => (p.e.x to p.x + movingCount).map(Point(_, p.y))
          case Dir.S => (p.n.y to p.y - movingCount by -1).map(Point(p.x, _))
          case Dir.E => (p.w.x to p.x - movingCount by -1).map(Point(_, p.y))

def cycle(start: Set[Point]) =
  List(Dir.N, Dir.W, Dir.S, Dir.E).foldLeft(start)(rollAll)

def load(points: Set[Point]): Int =
  points.iterator.map(p => area.height - p.y).sum

val ans1 = load(rollAll(rounded, Dir.N))

val cycles = LazyList.iterate(rounded)(cycle)
val hashes = cycles.map(_.hashCode())

val seenHashes = hashes.scanLeft(Set.empty[Int]):
  case (seen, hash) => (seen + hash)

val repeat = seenHashes.zip(hashes).indexWhere((seen, hash) => seen(hash))
val first = hashes.indexWhere(_ == hashes(repeat))
val period = repeat - first

val congruent = first + (1_000_000_000 - first) % period
val ans2 = load(cycles(congruent))
