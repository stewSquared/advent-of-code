val input1 = Array(
  "X.....>",
  "..v..X.",
  ".>..X..",
  "A......"
)

val input2 = Array(
  "...Xv",
  "AX..^",
  ".XX..."
)

val input3 = Array(
  ".....",
  "A...<",
  "....."
)

val input4: Array[String] = ("A" + "." * 10000) +: Array.fill[String](100)("." * 10001) //, "." * 1001)
// val input4 = Array("." * 1001, "A" + "." * 1000)

case class Point(x: Int, y: Int) {
  def u: Point = copy(y = y - 1)
  def d: Point = copy(y = y + 1)
  def r: Point = copy(x = x + 1)
  def l: Point = copy(x = x - 1)
}

class Grid(input: Array[String]) {
  val xRange = 0 until input(0).length
  val yRange = 0 until input.length

  val allPoints = for {
    x <- xRange
    y <- yRange
  } yield Point(x, y)

  def charAt(p: Point): Char = {
    input(p.y)(p.x)
  }

  def isFloor(p: Point): Boolean = charAt(p) == '.'
  def hasGuard(p: Point): Boolean = "^v><".contains(charAt(p))
  def isSolid(p: Point): Boolean = hasGuard(p) || charAt(p) == 'X'

  def inBounds(p: Point): Boolean = p match {
    case Point(x, y) => xRange.contains(x) && yRange.contains(y)
  }

  def observedBy(pos: Point): Iterator[Point] = {
    val sightLine = charAt(pos) match {
      case '^' => Iterator.iterate(pos)(_.u).drop(1)
      case 'v' => Iterator.iterate(pos)(_.d).drop(1)
      case '>' => Iterator.iterate(pos)(_.r).drop(1)
      case '<' => Iterator.iterate(pos)(_.l).drop(1)
      case _ => Iterator.empty
    }
    sightLine.takeWhile(p => inBounds(p) && isFloor(p))
  }

  val offLimits: Set[Point] = {
    val observed = allPoints.filter(hasGuard).flatMap(observedBy)
    val blocked = allPoints.filter(isSolid).toSet
    observed.toSet.union(blocked)
  }

  def adjacent(p: Point): Set[Point] = Set(p.u, p.d, p.r, p.l).filter(inBounds)

  def targetReachable: Boolean = {
    val start = {
      val y = input.indexWhere(_.contains("A"))
      val x = input(y).indexWhere(_ == 'A')
      Point(x, y)
    }

    val target = Point(xRange.max, yRange.max)

    val reachable: Iterator[Set[Point]] =
      Iterator.unfold((Set.empty[Point], Set(start))){
        case (previous, current) => Option.when(current.nonEmpty) {
          val next = current.flatMap(adjacent).diff(previous).filterNot(offLimits)
          current -> (current, next)
        }
      }

    reachable.exists(_.contains(target))

    // def search(previous: Set[Point], current: Set[Point]): Boolean = {
    //   current.nonEmpty && (current.contains(target) || {
    //     val next: Set[Point] = current.flatMap(adjacent).diff(previous).filterNot(offLimits)
    //     search(current, next)
    //   })
    // }

    // !offLimits(start) && search(Set.empty, Set(start))
  }
}

val grid1 = Grid(input1)
val grid2 = Grid(input2)
val grid3 = Grid(input3)
val grid4 = Grid(input4)

grid1.targetReachable

grid2.targetReachable

grid3.targetReachable

3 + 3

grid4.targetReachable

grid4.charAt(Point(0,0))
grid4.offLimits
grid4.adjacent(Point(0,0))
grid4.yRange

grid1.charAt(Point(1,2))

grid1.offLimits
grid1.observedBy(Point(2,1)).toList
grid1.observedBy(Point(1,2)).toList
