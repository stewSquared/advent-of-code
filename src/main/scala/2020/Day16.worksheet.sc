val input = io.Source.fromResource("2020/day-16.txt").getLines.toList

case class Rule(field: String, r1: Range, r2: Range):
  def valid(value: Int): Boolean = r1.contains(value) || r2.contains(value)

object Rule:
  def parse(raw: String): Rule = raw match
    case s"$field: $x1-$x2 or $y1-$y2" =>
      Rule(field, x1.toInt to x2.toInt, y1.toInt to y2.toInt)


type Ticket = Vector[Int]

val (rules: List[Rule], your: Ticket, nearby: List[Ticket]) =
  util.Using(io.Source.fromResource("2020/day-16.txt")){ s =>
    val lines = s.getLines()
    val rules = lines.takeWhile(_.nonEmpty).map(Rule.parse).toList
    def ticket(line: String) = line.split(",").map(_.toInt).toVector
    val your = ticket(lines.drop(1).next())
    val nearby = lines.drop(2).toList.map(ticket)

    (rules, your, nearby)
  }.get

def completelyInvalid(value: Int): Boolean = !rules.exists(_.valid(value))

val ans1 = nearby.flatten.filter(completelyInvalid).sum

val validTickets = nearby.filterNot(_.exists(completelyInvalid))

val fieldValues = validTickets.transpose

val possibleFieldIndices = rules.map { rule =>
  val possibleIndices = fieldValues.zipWithIndex.collect {
    case (values, index) if values.forall(rule.valid) => index
  }
  rule.field -> possibleIndices
}

val fieldIndices = possibleFieldIndices.sortBy(_._2.length).foldLeft(Map.empty[String, Int]){
  case (solved, (field, indices)) =>
    solved + (field -> indices.diff(solved.values.toSeq).head)
}

val ans2 = fieldIndices.collect {
  case (field, i) if field.startsWith("departure") => your(i).toLong
}.product
