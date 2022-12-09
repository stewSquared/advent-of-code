val input = io.Source.fromResource("2022/day-09.txt").getLines().toList

case class Point(x: Int, y: Int)
type Rope = List[Point]
type Move = Point => Point

def chase(h: Point, t: Point): Move =
  val dx = h.x - t.x
  val dy = h.y - t.y
  val noGap = dx.abs <= 1 && dy.abs <= 1
  if noGap then identity
  else p => Point(p.x + dx.sign, p.y + dy.sign)

extension (rope: Rope)
  def move(f: Move): Rope = rope match
    case h :: n :: tail =>
      f(h) :: (n :: tail).move(chase(f(h), n))
    case r => r.map(f)

val movements: List[Move] = input.flatMap {
  case s"U $n" => List.fill(n.toInt)(p => p.copy(y = p.y + 1))
  case s"D $n" => List.fill(n.toInt)(p => p.copy(y = p.y - 1))
  case s"R $n" => List.fill(n.toInt)(p => p.copy(x = p.x + 1))
  case s"L $n" => List.fill(n.toInt)(p => p.copy(x = p.x - 1))
}

def ans(rope: Rope) =
  val states = movements.scanLeft(rope)(_ move _)
  states.map(_.last).toSet.size

val ans1 = ans(List.fill(2)(Point(0, 0)))
val ans2 = ans(List.fill(10)(Point(0, 0)))
