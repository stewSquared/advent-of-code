import aoc.*

val input = io.Source.fromResource("2023/day-18.txt").getLines.toList

def parse(instruction: String): (Dir, Int) =
  instruction match
    case s"$rawDir $n (#$rawHex)" =>
      val distance = n.toInt
      val dir = rawDir match
        case "U" => Dir.N
        case "D" => Dir.S
        case "R" => Dir.E
        case "L" => Dir.W
      dir -> distance

def parse2(instruction: String): (Dir, Int) =
  instruction match
    case s"$rawDir $n (#$rawHex)" =>
      val distance = Integer.parseInt(rawHex.take(5), 16)
      val dir = rawHex(5) match
        case '0' => Dir.E
        case '1' => Dir.S
        case '2' => Dir.W
        case '3' => Dir.N
      dir -> distance

val parsed = input.map(parse2)

val maxRight = Integer.parseInt("FFFFF", 16) * input.size / 4

val (p, nsArea) = parsed.foldLeft((Point.origin, 0L)):
  case ((pos, area), (newDir, distance)) =>
    val newPos = pos.move(newDir, distance)
    val line = Line(pos, newPos)
    newDir match
      case Dir.E | Dir.W => newPos -> area
      case Dir.N =>
        val a = Area(line.xRange.min to maxRight, line.yRange)
        newPos -> (area + a.size[Long])
      case Dir.S =>
        val a = Area(line.xRange.min + 1 to maxRight, line.yRange)
        newPos -> (area - a.size[Long])

val ewPrevNext =
  val prev = (parsed.last :: parsed)
  val ew = parsed
  val next = (parsed.tail :+ parsed.head)
  prev.zip(ew).zip(next)

val (pew, ewArea) = ewPrevNext.foldLeft(Point.origin -> 0L):
  case ((pos, area), (((prevDir, _), (newDir, distance)), (nextDir, _))) =>
    val newPos = pos.move(newDir, distance)
    val line = Line(pos, newPos)
    println(s"$pos: $prevDir $newDir $distance $nextDir")
    newDir match
      case Dir.N | Dir.S => newPos -> area
      case Dir.E => (prevDir, nextDir) match
        case (Dir.N, Dir.N) =>
          val a = Area(newPos.x to maxRight, pos.y to pos.y)
          newPos -> (area - a.size[Long])
        case (Dir.N, Dir.S) =>
          newPos -> area
        case (Dir.S, Dir.S) =>
          val a = Area(pos.e.x to maxRight, pos.y to pos.y)
          newPos -> (area + a.size[Long])
        case (Dir.S, Dir.N) =>
          val a = Area(pos.e.x to newPos.w.x, pos.y to pos.y)
          newPos -> (area + a.size[Long])
      case Dir.W => (prevDir, nextDir) match
        case (Dir.N, Dir.N) =>
          val a = Area(pos.x to maxRight, pos.y to pos.y)
          println(s"subtracting $a -${a.size[Long]}")
          newPos -> (area - a.size[Long])
        case (Dir.N, Dir.S) =>
          val a = Area(newPos.e.x to pos.w.x, pos.y to pos.y)
          println(s"adding $a +${a.size[Long]}")
          newPos -> (area + a.size[Long])
        case (Dir.S, Dir.S) =>
          val a1 = Area(newPos.e.x to maxRight, pos.y to pos.y)
          newPos -> (area + a1.size[Long])
        case (Dir.S, Dir.N) =>
          newPos -> area

val ans2 = nsArea + ewArea
