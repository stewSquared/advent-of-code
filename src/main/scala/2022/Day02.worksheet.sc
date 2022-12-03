val guide = io.Source.fromResource("2022/day-02.txt").getLines().toList

enum Outcome:
  case Loss, Draw, Win
  val score = this.ordinal * 3

import Outcome.*

enum Hand:
  case Rock, Paper, Scissors
  val score = this.ordinal + 1

  def outcomeAgainst(other: Hand) =
    val diff = (this.ordinal - other.ordinal + 3) % 3
    if diff == 1 then Win
    else if diff == 2 then Loss
    else Draw

  def choiceFor(outcome: Outcome) = outcome match
    case Draw => this
    case Win => Hand.fromOrdinal((this.ordinal + 1) % 3)
    case Loss => Hand.fromOrdinal((this.ordinal + 2) % 3)

val ans1 = guide.map {
  case s"$o $c" =>
    val opponent = Hand.fromOrdinal("ABC".indexOf(o))
    val choice = Hand.fromOrdinal("XYZ".indexOf(c))
    val outcome = choice.outcomeAgainst(opponent)
    choice.score + outcome.score
}.sum

val ans2 = guide.map {
  case s"$o $c" =>
    val opponent = Hand.fromOrdinal("ABC".indexOf(o))
    val outcome = Outcome.fromOrdinal("XYZ".indexOf(c))
    val choice = opponent.choiceFor(outcome)
    choice.score + outcome.score
}.sum
