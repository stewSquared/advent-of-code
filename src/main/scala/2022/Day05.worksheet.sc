type State = Vector[String]

val lines = io.Source.fromResource("2022/day-05.txt").getLines()
val drawing = lines.takeWhile(_.nonEmpty).toVector
val steps = lines.toList

val start: State = drawing.init.transpose.collect {
  case col if col.exists(_.isLetter) => col.mkString.stripLeading
}

def move(state: State, step: String, multi: Boolean): State = step match
  case s"move $c from $s to $d" =>
    val count = c.toInt
    val source = s.toInt - 1
    val dest = d.toInt - 1
    val moving = state(source).take(count)

    state
      .updated(source, state(source).drop(count))
      .updated(dest, (if multi then moving else moving.reverse) ++ state(dest))

val finalState1 = steps.foldLeft(start)(move(_, _, multi = false))
val ans1 = finalState1.map(_.head).mkString

val finalState2 = steps.foldLeft(start)(move(_, _, multi = true))
val ans2 = finalState2.map(_.head).mkString
