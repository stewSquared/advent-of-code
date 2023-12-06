val input = io.Source.fromResource("2023/day-06.txt").getLines()

val rawTime = input.next()
val rawDistance = input.next()

def waysToWin(timeLimit: Long, recordDistance: Long): Long =
  val z1 = (timeLimit + math.sqrt(timeLimit.toDouble * timeLimit - 4 * recordDistance)) / 2
  val z2 = (timeLimit - math.sqrt(timeLimit.toDouble * timeLimit - 4 * recordDistance)) / 2

  val max = (z1 max z2).ceil.toLong
  val min = (z1 min z2).floor.toLong
  println(min to max)

  (min to max).size - 2

val timeLimits = rawTime.stripPrefix("Time:").trim.split(" +").map(_.toLong).toList
val recordDistances = rawDistance.stripPrefix("Distance:").trim().split(" +").map(_.toLong).toList

timeLimits.zip(recordDistances).map(waysToWin)
val ans1 = timeLimits.zip(recordDistances).map(waysToWin).product

val timeLimit = rawTime.stripPrefix("Time:").split(" ").mkString.toLong
val recordDistance = rawDistance.stripPrefix("Distance:").split(" ").mkString.toLong

val ans2 = waysToWin(timeLimit, recordDistance)
