import aoc.*

val grid = io.Source.fromResource("2023/day-14-ex.txt").getLines().toVector

val area = Area(grid)
area.width

val rounded: Set[Point] = area.pointsIterator.filter(grid(_) == 'O').toSet
val squared: Set[Point] = area.pointsIterator.filter(grid(_) == '#').toSet

def roll(dir: Dir, p: Point, rounded: Set[Point]): Point =
  val occpuied = rounded union squared
  assert(rounded(p))
  Iterator.iterate(p)(_.move(dir)).drop(1)
    .takeWhile(area.contains)
    .takeWhile(!occpuied(_))
    .toList.lastOption.getOrElse(p)

def sortByDir(points: List[Point], dir: Dir) = dir match
  case Dir.E => points.sortBy(-_.x)
  case Dir.S => points.sortBy(-_.y)
  case Dir.W => points.sortBy(_.x)
  case Dir.N => points.sortBy(_.y)

def rollAll(start: Set[Point], dir: Dir) =
  sortByDir(start.toList, dir).foldLeft(start):
    case (rounded, p) => rounded - p + roll(dir, p, rounded)

def cycle(start: Set[Point]) =
  List(Dir.N, Dir.W, Dir.S, Dir.E).foldLeft(start)(rollAll)

def load(points: Set[Point]): Int =
  points.iterator.map(p => area.height - p.y).sum

val ans1 = load(rollAll(rounded, Dir.N))

val cycles = LazyList.iterate(rounded)(cycle)

val loads = cycles.map(load)

loads.take(100) foreach println

cycleLength(loads.drop(1))

def cycleLength(nums: LazyList[Int]): Int = {
  def findRepeats(remainders: LazyList[Int], seen: List[Int]): Int = {
    remainders match {
      case r #:: rs if seen.contains(r) => seen.indexOf(r) + 1
      case r #:: rs => findRepeats(rs, r :: seen)
      case _ => 0
    }
  }

  findRepeats(nums, Nil)
}

def detectCycle(loads: LazyList[Int]): Int =
  ???

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
