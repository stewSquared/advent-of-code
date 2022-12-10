val input = io.Source.fromResource("2022/day-10.txt").getLines().toList

case class Point(x: Int, y: Int)

case class State(clock: Int, x: Int, pixel: Point, crt: Map[Point, Boolean]):
  def nextPixel = pixel match
    case Point(39, 5) => Point(0, 0)
    case Point(39, y) => Point(0, y + 1)
    case Point(x, y) => Point(x + 1, y)

  def tick = copy(clock + 1, pixel = nextPixel)
  def draw = copy(crt = crt.updated(pixel, (pixel.x - x).abs <= 1))
  def addX(v: Int) = copy(x = x + v.toInt)

  def step(inst: String): State = inst match
    case "noop" => draw.tick
    case s"addx $v" => draw.tick.draw.addX(v.toInt).tick

def signalStrengths(cycles: List[Int], states: List[State], prevX: Int, ans: List[Int]): List[Int] =
  (cycles, states) match
    case (Nil, _) => ans
    case (c::cs, s::ss) if s.clock == c =>
      signalStrengths(cs, ss, prevX = s.x, (s.x * c) :: ans)
    case (c::cs, s::ss) if s.clock > c =>
      signalStrengths(cs, ss, prevX = s.x, (prevX * c) :: ans)
    case (_, s::ss) =>
       signalStrengths(cycles, ss, prevX = s.x, ans)
    case (_, Nil) => ans

val start = State(clock = 1, x = 1, pixel = Point(0,0), crt = Map.empty)
val states = input.scanLeft(start)(_ step _)
val cycles = List(20, 60, 100, 140, 180, 220)

val ans1 = signalStrengths(cycles, states, 1, Nil).sum

val finalCrt = states.last.crt

for y <- 0 to 5 do
  for x <- 0 to 39 do
    val p = Point(x, y)
    print(if finalCrt(p) then '\u2588' else ' ')
  println()
