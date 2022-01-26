val input = io.Source.fromResource("2020/day-12.txt").getLines.toList

case class Point(x: Int, y: Int)

enum Dir:
  case N, E, S, W
  def rotate(degrees: Int): Dir =
    util.Try(Dir.fromOrdinal((this.ordinal + ((degrees + 360) / 90)) % 4))
      .getOrElse(throw Exception(s"couldn't handle $degrees"))

case class State(pos: Point, dir: Dir):
  def move(action: String): State = action match
    case s"N$d" => copy(pos.copy(y = pos.y + d.toInt))
    case s"S$d" => copy(pos.copy(y = pos.y - d.toInt))
    case s"E$d" => copy(pos.copy(x = pos.x + d.toInt))
    case s"W$d" => copy(pos.copy(x = pos.x - d.toInt))
    case s"R$d" => copy(dir = dir.rotate(d.toInt))
    case s"L$d" => copy(dir = dir.rotate(- d.toInt))
    case s"F$d" => move(s"$dir$d")

val start = State(Point(0, 0), Dir.E)

val last = input.foldLeft(start)(_ move _)

val ans = last.pos.x.abs + last.pos.y.abs

case class State2(pos: Point, dir: Dir, wayPoint: Point):
  def rotate90: State2 = wayPoint match
    case Point(x, y) => copy(wayPoint = Point(x = y, y = -x))

  def move(action: String): State2 = action match
    case s"N$d" => copy(wayPoint = wayPoint.copy(y = wayPoint.y + d.toInt))
    case s"S$d" => copy(wayPoint = wayPoint.copy(y = wayPoint.y - d.toInt))
    case s"E$d" => copy(wayPoint = wayPoint.copy(x = wayPoint.x + d.toInt))
    case s"W$d" => copy(wayPoint = wayPoint.copy(x = wayPoint.x - d.toInt))
    case s"R$d" =>
      val quarterTurns = d.toInt / 90 % 4
      Iterator.iterate(this)(_.rotate90).drop(quarterTurns).next()
    case s"L$d" =>
      val quarterTurns = (360 - d.toInt) / 90 % 4
      Iterator.iterate(this)(_.rotate90).drop(quarterTurns).next()
    case s"F$d" =>
      val times = d.toInt
      copy(pos = pos.copy(
        x = pos.x + wayPoint.x * times,
        y = pos.y + wayPoint.y * times)
      )

val start2 = State2(pos = Point(0, 0), dir = Dir.E, wayPoint = Point(10, 1))

val last2 = input.foldLeft(start2)(_ move _)

val ans2 = last2.pos.x.abs + last2.pos.y.abs

start2
  .move("F10")

start2
  .move("F10")
  .move("N3")
  .move("F7")

start2.rotate90
start2.rotate90.rotate90
start2.rotate90.rotate90.rotate90
start2.rotate90.rotate90.rotate90.rotate90

// LazyList.iterate(start)()
