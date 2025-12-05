val input = io.Source.fromResource("2015/day-14.txt").getLines().toList

case class Deer(speed: Int, endurance: Int, rest: Int)

val deer = input.collect:
  case s"$name can fly $speed km/s for $endurance seconds, but then must rest for $rest seconds." =>
    Deer(speed.toInt, endurance.toInt, rest.toInt)

def distance(deer: Deer, time: Int): Int =
  val cycleTime = deer.endurance + deer.rest
  val cycleDist = deer.speed * deer.endurance
  val numFullCycles = time / cycleTime

  val timeRemaining = time - numFullCycles * cycleTime
  val lastLegRunTime = deer.endurance.min(timeRemaining)
  val lastLegDistance = lastLegRunTime * deer.speed

  cycleDist * numFullCycles + lastLegDistance

val ans1 = deer.map(distance(_, 2503)).max

val scoreCard = (1 to 2503).map: time =>
  val positions = deer.map(distance(_, time))
  val ahead = positions.max
  positions.map(p => if p == ahead then 1 else 0)

val ans2 = scoreCard.transpose.map(_.sum).max
