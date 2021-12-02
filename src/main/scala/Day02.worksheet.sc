val commands = io.Source.fromResource("day-02-1.txt").getLines.toList

case class Pos(horiz: Int, depth: Int, aim: Int):
  def move(command: String) = command match
    case s"up $x"      => copy(aim = aim - x.toInt)
    case s"down $x"    => copy(aim = aim + x.toInt)
    case s"forward $x" => copy(horiz + x.toInt, depth + aim * x.toInt)

val finalPos = commands.foldLeft(Pos(0, 0, 0))(_ move _)

val ans1 = finalPos.horiz * finalPos.aim
val ans2 = finalPos.horiz * finalPos.depth
