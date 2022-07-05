val input = io.Source.fromResource("2020/day-12.txt").getLines.toList

enum Dir: 
    case N, E, S, W

case class Point(x: Int, y: Int):
    def n(d: Int) = copy(y = y + d)
    def s(d: Int) = copy(y = y - d)
    def e(d: Int) = copy(x = x + d)
    def w(d: Int) = copy(x = x - d)

    def manhattan = x.abs + y.abs


case class State(dir: Dir, pos: Point):
    def rightTurn(deg: Int): State = 
        copy(dir = Dir.fromOrdinal((dir.ordinal + deg / 90) % 4))

    def move(inst: String): State = inst match
        case s"N$d" => copy(pos = pos.n(d.toInt))
        case s"S$d" => copy(pos = pos.s(d.toInt))
        case s"E$d" => copy(pos = pos.e(d.toInt))
        case s"W$d" => copy(pos = pos.w(d.toInt))
        case s"R$d" => rightTurn(d.toInt)
        case s"L$d" => rightTurn(d.toInt * 3)
        case s"F$d" => move(s"$dir$d")


case object State:
    val start = State(Dir.E, Point(0, 0))

val end = input.foldLeft(State.start)(_ move _)

val ans1 = end.pos.manhattan

case class State2(pos: Point, way: Point):
    def quarterTurn: State2 =
        copy(way = Point(x = way.y, y = -way.x))

    def rightTurn(deg: Int): State2 =
        val quarters = (deg / 90) % 4
        Iterator.iterate(this)(_.quarterTurn).drop(quarters).next()

    def move(inst: String): State2 = inst match
        case s"N$d" => copy(way = way.n(d.toInt))
        case s"S$d" => copy(way = way.s(d.toInt))
        case s"E$d" => copy(way = way.e(d.toInt))
        case s"W$d" => copy(way = way.w(d.toInt))
        case s"R$d" => rightTurn(d.toInt)
        case s"L$d" => rightTurn(d.toInt * 3)
        case s"F$d" => 
            val times = d.toInt
            copy(pos = pos.copy(
                x = pos.x + way.x * times,
                y = pos.y + way.y * times)
            )

object State2:
    val start = State2(Point(0, 0), Point(10, 1))

val end2 = input.foldLeft(State2.start)(_ move _)

val ans2 = end2.pos.manhattan

State2.start
    .move("F10")
    .move("N3")
    .move("F7")
    .move("R90")
    