val input = io.Source.fromResource("2023/day-02.txt").getLines().toList

case class Game(id: Int, hands: List[Map[String, Int]])

val games = input.map:
  case s"Game $id: $handsStr" =>
    val hands = handsStr.split("; ").toList.map: str =>
      str.split(", ").collect {
        case s"$n blue" => "blue" -> n.toInt
        case s"$n red" => "red" -> n.toInt
        case s"$n green" => "green" -> n.toInt
      }.toMap.withDefaultValue(0)
    Game(id.toInt, hands)

val possibleGames = games
  .filter(_.hands.map(_("red")).max <= 12)
  .filter(_.hands.map(_("green")).max <= 13)
  .filter(_.hands.map(_("blue")).max <= 14)

def power(game: Game): Int =
  game.hands.map(_("red")).max * game.hands.map(_("green")).max * game.hands.map(_("blue")).max

val ans = possibleGames.map(_.id).sum
val ans2 = games.map(power).sum
