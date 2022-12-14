val input = io.Source.fromResource("2022/day-14.txt").getLines().toList

case class Point(x: Int, y: Int):
  def d = copy(y = y + 1)
  def dl = copy(x = x - 1, y = y + 1)
  def dr = copy(x = x + 1, y = y + 1)

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

val source: Point = Point(500, 0)
val abyss: Int = rocks.map(_.y).max + 1

def fall(occupied: Set[Point]): LazyList[Point] = LazyList.unfold(source) {
  pos =>
    val d = Option(pos.d).filterNot(occupied)
    val dl = Option(pos.dl).filterNot(occupied)
    val dr = Option(pos.dr).filterNot(occupied)

    val nextPos = d.orElse(dl).orElse(dr)
    nextPos.map(p => p -> p)
}

def sandPoints1 = Iterator.unfold(rocks) { occupied =>
  fall(occupied)
    .takeWhile(_.y <= abyss)
    .lastOption
    .filterNot(_.y == abyss)
    .map(resting => resting -> occupied.incl(resting))
}

val ans1 = sandPoints1.size

val floor = abyss + 1

def sandPoints2 = Iterator.unfold(rocks) { occupied =>
  fall(occupied)
    .takeWhile(_.y < floor)
    .lastOption
    .map(resting => resting -> occupied.incl(resting))
}

val ans2 = sandPoints2.size + 1
