val guide = io.Source.fromResource("2022/day-02.txt").getLines().toList

def score1(round: String): Int =
  val choice = round match
    case s"$opponent X" => 1
    case s"$opponent Y" => 2
    case s"$opponent Z" => 3
  val outcome = round match
    case "A X" | "B Y" | "C Z" => 3
    case "A Y" | "B Z" | "C X" => 6
    case _ => 0
  choice + outcome

def score2(round: String): Int =
  val outcome = round match
    case s"$opponent X" => 0
    case s"$opponent Y" => 3
    case s"$opponent Z" => 6
  val choice = round match
    case "A Y" | "B X" | "C Z" => 1
    case "A Z" | "B Y" | "C X" => 2
    case _ => 3

  choice + outcome

val ans1 = guide.map(score1).sum
val ans2 = guide.map(score2).sum
