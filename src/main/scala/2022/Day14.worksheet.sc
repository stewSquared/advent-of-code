val input = io.Source.fromResource("2022/day-14.txt").getLines().toList

case class Point(x: Int, y: Int)

val paths = input.map {
  _.split(" -> ").map { case s"$x,$y" => Point(x.toInt, y.toInt) }.toList
}

val rocks = paths.flatMap {
  _.sliding(2).flatMap { case List(p1, p2) =>
    val dx = p2.x - p1.x
    val dy = p2.y - p1.y

    if dx == 0 then (p1.y to p2.y by dy.sign).map(Point(p1.x, _))
    else (p1.x to p2.x by dx.sign).map(Point(_, p1.y))
  }
}.toSet

val source = Point(500, 0)
val lowestRock = rocks.map(_.y).max
val floor = lowestRock + 2

case class SearchState(path: List[Point], sand: Set[Point]):
  def air(p: Point) = !sand(p) && !rocks(p)

  def next: Option[SearchState] = path.headOption.map { case pos@Point(x, y) =>
    val d = Option(Point(x, y + 1)).filter(air)
    val dl = Option(Point(x - 1, y + 1)).filter(air)
    val dr = Option(Point(x + 1, y + 1)).filter(air)

    val fallPos = (d orElse dl orElse dr).filter(_.y < floor)
    fallPos.fold(copy(path.tail, sand + pos))(p => copy(p :: path))
  }

def states = LazyList.unfold(SearchState(List(source), Set.empty)) {
  _.next.map(s => s -> s)
}

val ans1 = states.takeWhile(_.path.head.y < lowestRock).last.sand.size

val ans2 = states.last.sand.size
