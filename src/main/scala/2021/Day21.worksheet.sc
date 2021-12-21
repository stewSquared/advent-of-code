// val start = Game(Player(4, 0), Player(8, 0), turn = 0)
val start = Game(Player(4, 0), Player(3, 0), turn = 0)

case class Player(pos: Int, score: Int):
  def move(n: Int): Player =
    val newPos = (pos - 1 + n) % 10 + 1
    copy(newPos, score + newPos)

case class Game(player1: Player, player2: Player, turn: Int):
  def p1turn = turn % 2 == 0
  def next(rolled: Int): Game =
    if p1turn then copy(player1.move(rolled), player2, turn + 1)
    else copy(player1, player2.move(rolled), turn + 1)

  def hasScored(n: Int) = player1.score >= n || player2.score >= n

def practiceRolls = Iterator.from(0).map(_ % 100 + 1).grouped(3).map(_.sum)

def practiceStates = practiceRolls.scanLeft(start)(_ next _)

val practiceEnd = practiceStates.find(_ hasScored 1000).get

val ans1 =
  import practiceEnd.*
  val loserScore = if p1turn then player1.score else player2.score
  loserScore * turn * 3

type Tally = (Long, Long)

extension (wins: Tally)
  def +(w2: Tally): Tally = (wins._1 + w2._1, wins._2 + w2._2)
  def *(n: Long): Tally = (wins._1 * n, wins._2 * n)
  def inc1(n: Long): Tally = (wins._1 + n, wins._2)
  def inc2(n: Long): Tally = (wins._1, wins._2 + n)

val diracRolls =
  val rs = for
    d1 <- 1 to 3
    d2 <- 1 to 3
    d3 <- 1 to 3
  yield d1 + d2 + d3
  rs.groupMapReduce(identity)(_ => 1L)(_ + _)

object DR:
  def countDiracWins(game: Game): Tally =
    if game.player1.score >= 21 then (1L, 0L)
    else if game.player2.score >= 21 then (0L, 1L)
    else
      diracRolls.foldLeft[Tally](0L, 0L) { case (tally, (roll, frequency)) =>
        tally + countDiracWins(game.next(roll)) * frequency
      }
  end countDiracWins

  def ans2 =
    val (w1, w2) = countDiracWins(start)
    w1 max w2

println(DR.ans2)

object DI:
  def diracEndings(game: Game): Iterator[(Game, Long)] =
    import game.*
    if game.hasScored(21) then Iterator(game -> 1L)
    else
      diracRolls.iterator.flatMap { case (roll, frequency) =>
        diracEndings(game.next(roll)).map { case (g, f) =>
          (g -> f * frequency)
        }
      }

  def tally = diracEndings(start).foldLeft[Tally]((0L, 0L)) {
    case (tally, (game, frequency)) =>
      tally + (if game.p1turn then (0L, 1L) else (1L, 0L)) * frequency
  }
  def ans2 =
    val (w1, w2) = tally
    w1 max w2

// println(DI.ans2)

object TR:
  @annotation.tailrec
  def countDiracWins(games: List[(Game, Long)], tally: Tally): Tally =
    games match
      case Nil => tally
      case (game, freq) :: tail if game.hasScored(21) =>
        if game.p1turn then countDiracWins(tail, tally.inc2(freq))
        else countDiracWins(tail, tally.inc1(freq))
      case (game, freq) :: tail =>
        val copies =
          for (roll, dieFreq) <- diracRolls.toList
          yield game.next(roll) -> (dieFreq * freq)
        countDiracWins(copies ::: tail, tally)
  end countDiracWins

  def ans2 =
    val (w1, w2) = countDiracWins(List(start -> 1L), (0L, 0L))
    w1 max w2

// println(TR.ans2)
