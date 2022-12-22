val input = io.Source.fromResource("2022/day-22.txt").getLines().toVector

val mapInput = input.takeWhile(_.nonEmpty)

mapInput foreach println

val map: Map[Point, Char] =
  (for
    (row, y) <- mapInput.zipWithIndex
    (char, x) <- row.zipWithIndex
    if char != ' '
  yield Point(x + 1, y + 1) -> char).toMap

val path = input.drop(mapInput.size + 1).head

// val pathSteps: List[Int | Turn] =
//   Iterator.unfold(path) { remaining =>
//     Option.when(remaining.nonEmpty) {
//       val (nextSteps, afterStep) = remaining.span(_.isDigit)
//       if nextSteps.nonEmpty then
//         (nextSteps.toInt -> afterStep): (Int | Turn, String)
//       else
//         // val nextTurn: Turn = _.turn(remaining.head)
//         // assert("RL".contains(nextTurn), nextTurn)
//         // val d = Dir.fromOrdinal("ESWN".indexOf(nextTurn))
//         val nextTurn = Turn.fromOrdinal("RL".indexOf(remaining.head))
//         (nextTurn -> remaining.tail): (Int | Turn, String)
//     }
//   }.toList

// pathSteps.collect {
//   case n: Int => n
// } foreach println

// val turns = pathSteps.collect {
//   case t: Turn => t
// }

// turns.foldLeft(Dir.E)(_.turn(_))
// Dir.E.turn(turns.head)

// pathSteps foreach println

enum Dir:
  case E, S, W, N

  def turnRight = this match
    case Dir.E => S
    case Dir.S => W
    case Dir.W => N
    case Dir.N => E

  def turnLeft = this match
    case Dir.E => N
    case Dir.S => E
    case Dir.W => S
    case Dir.N => W

case class Point(x: Int, y: Int):
  def e = copy(x = x + 1)
  def s = copy(y = y + 1)
  def w = copy(x = x - 1)
  def n = copy(y = y - 1)

  def move(d: Dir) = d match
    case Dir.E => e
    case Dir.S => s
    case Dir.W => w
    case Dir.N => n

def move(p: Point, d: Dir): Point =
  val next = p.move(d)
  val wrapped: Point = if map.contains(next) then next else
    println("wrapping around")
    d match
      case Dir.E => map.keys.filter(_.y == next.y).minBy(_.x)
      case Dir.S => map.keys.filter(_.x == next.x).minBy(_.y)
      case Dir.W => map.keys.filter(_.y == next.y).maxBy(_.x)
      case Dir.N => map.keys.filter(_.x == next.x).maxBy(_.y)

  // println(s"attempting to move to $wrapped")

  map(wrapped) match
    case '#' => p
    case '.' => wrapped

val startPoint = map.keys.filter(_.y == 1).minBy(_.x)

val states = LazyList.unfold((startPoint, Dir.E, path)) {
  case (pos, dir, "") => None
  case (pos, dir, remaining) if remaining.startsWith("R") =>
    println("turning right")
    val nextDir = dir.turnRight
    Some((pos, nextDir) -> (pos, nextDir, remaining.tail))
  case (pos, dir, remaining) if remaining.startsWith("L") =>
    println("turning left")
    val nextDir = dir.turnLeft
    Some((pos, nextDir) -> (pos, nextDir, remaining.tail))
  case (pos, dir, remaining) if remaining.head.isDigit =>
    val (nextSteps, afterStep) = remaining.span(_.isDigit)
    println(s"moving $dir $nextSteps from $pos")
    val nextPos = List.iterate(pos, nextSteps.toInt + 1)(move(_, dir)).last
    println(s"landed at $nextPos")
    Some((nextPos, dir) -> (nextPos, dir, afterStep))
  case _ => ???
}

val (finalPos, finalDir) = states.last

println(path)

states foreach println

val ans1 = 1000 * finalPos.y + 4 * finalPos.x + finalDir.ordinal


// pathSteps foreach println

// pathSteps.map[Int | Turn] { inst =>
//   inst match
//     case n: Int => n
//     case t: Turn => t
// }

// turns.foldLeft(Dir.E)(_.turn(_))
// Dir.E.turn(turns.head)

// pathSteps.foldLeft(Point(0,0) -> Dir.E) { case((pos, dir), inst) =>
//   inst match
//     case steps: Int =>
//       pos -> dir
//       // val newPos: Point = List.iterate(pos, steps)(move(_, dir)).last
//       // newPos -> dir
//     case t: Turn =>
//       // pos -> dir
//       pos -> dir.turn(t)
// }


// pathSteps.foldLeft[(Point, Dir)](startPoint -> Dir.E) { case ((pos, dir), inst) =>
//   inst match
//     case steps: Int =>
//       val newPos: Point = List.iterate(pos, steps)(move(_, dir)).last
//       newPos -> dir
//     case t: Turn =>
//       pos -> dir.turn(t)

// }

// val (finalPos, finalDir) = pathSteps.foldLeft[(Point, Dir)](startPoint -> Dir.E){
//   case ((pos, dir), inst) =>
//     inst match
//       case steps: Int =>
//         val newPos: Point = List.iterate(pos, steps)(move(_, dir)).last
//         newPos -> dir
//       case t: Turn =>
//         pos -> dir.turn(t)
//       // case other =>
//       //   throw Exception(s"other $other")
// }


//
