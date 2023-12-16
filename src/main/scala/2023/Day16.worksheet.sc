import aoc.*

val input = io.Source.fromResource("2023/day-16.txt").getLines.toVector

val area = Area(input)

val mirrors: Map[Point, Char] = Map.from:
  area.pointsIterator.filter(input(_) != '.').map:
    p => p -> input(p)

val seen = collection.mutable.Set.empty[(Point, Dir)]

def path(from: Point, dir: Dir): Set[Point] =
  if seen((from, dir)) then Set.empty
  else
    seen += ((from, dir))
    val (energized, continue) = Iterator.iterate(from)(_.move(dir))
      .drop(1)
      .takeWhile(area.contains)
      .span(!mirrors.contains(_))
    val energizedSet = energized.toSet
    println(s"energized $energizedSet")
    continue.nextOption.fold(energizedSet): p =>
      println(s"moving $dir into $p")
      (mirrors(p), dir) match
        case ('/', Dir.E | Dir.W) =>
          energizedSet union path(p, dir.turnLeft) + p
        case ('\\', Dir.E | Dir.W) =>
          energizedSet union path(p, dir.turnRight) + p
        case ('/', Dir.N | Dir.S) =>
          energizedSet union path(p, dir.turnRight) + p
        case ('\\', Dir.N | Dir.S) =>
          energizedSet union path(p, dir.turnLeft) + p
        case ('|', Dir.N | Dir.S) =>
          energizedSet union path(p, dir) + p
        case ('-', Dir.N | Dir.S) =>
          energizedSet union path(p, dir.turnRight) union path(p, dir.turnLeft) + p
        case ('-', Dir.E | Dir.W) =>
          energizedSet union path(p, dir) + p
        case ('|', Dir.E | Dir.W) =>
          energizedSet union path(p, dir.turnRight) union path(p, dir.turnLeft) + p

val last = path(Point.origin.w, Dir.E)
val ans1 = last.count(area.contains)

val l = area.leftBorder.points.map: p =>
  seen.clear()
  path(p.w, Dir.E).size

val r = area.rightBorder.points.map: p =>
  seen.clear()
  path(p.e, Dir.W).size

val t = area.topBorder.points.map: p =>
  seen.clear()
  path(p.n, Dir.S).size

val b = area.botBorder.points.map: p =>
  seen.clear()
  path(p.s, Dir.N).size

val ans2 = List(l.max, r.max, t.max, b.max).max

println:
  area.draw:
    case p if last(p) => '#'
    case p if mirrors.contains(p) => mirrors(p)
    case _ => '.'

// val ans1 = path(Point.origin.w, Dir.E).size


//
