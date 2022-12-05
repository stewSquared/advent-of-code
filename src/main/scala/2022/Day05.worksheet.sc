type State = Map[Int, Vector[Char]]

val lines = io.Source.fromResource("2022/day-05.txt").getLines()
val drawing = lines.takeWhile(_.nonEmpty).toVector
val stackNames = drawing.last.split(' ').flatMap(_.toIntOption).toList
val steps = lines.toList

val stackRows: Vector[Vector[Option[Char]]] = drawing.init.map {
  _.grouped(4).map(_(1)).map(c => Option.unless(c.isSpaceChar)(c)).toVector
}

val stacks = stackRows.transpose.map(_.flatten.reverse)

val start: State = stackNames.zip(stacks).toMap

def move(state: State, step: String, preserve: Boolean): State = step match {
  case s"move $c from $s to $d" =>
    val count = c.toInt
    val source = s.toInt
    val dest = d.toInt

    val moving = state(source).takeRight(count)

    state
      .updated(source, state(source).dropRight(count))
      .updated(
        dest,
        state(dest) ++ (if preserve then moving else moving.reverse)
      )
}

val finalState1 = steps.foldLeft(start)(move(_, _, false))
val ans1 = finalState1.toList.sortBy(_._1).map(_._2.last).mkString

val finalState2 = steps.foldLeft(start)(move(_, _, true))
val ans2 = finalState2.toList.sortBy(_._1).map(_._2.last).mkString
