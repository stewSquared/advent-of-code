// val input = io.Source.fromResource("2023/day-06.txt").getLines()

// val timeLimits =
//   val s"Time: $nums" = input.next()
//   nums.split(" ").filter(_.nonEmpty).map(_.toInt).toList

// val distances =
//   val s"Distance: $nums" = input.next()
//   nums.split(" ").filter(_.nonEmpty).map(_.toInt).toList

// val races = timeLimits.zip(distances)

// races foreach println

// def raceTime(held: Int, timeLimit: Int): Int =
//   val speed = held
//   println((timeLimit - held))
//   (timeLimit - held) * speed

// raceTime(4, 7)

// val ways = races.map:
//   case (timeLimit, recordDistance) =>
//     (1 to timeLimit).count:
//       raceTime(_, timeLimit) > recordDistance

// val ans1 = ways.product


val input = io.Source.fromResource("2023/day-06-test.txt").getLines()

val timeLimit =
  val s"Time: $nums" = input.next()
  nums.split(" ").mkString.toLong

val recordDistance =
  val s"Distance: $nums" = input.next()
  nums.split(" ").mkString.toLong

// recordDistance = held * (timeLimit - held)
// recordDistance = held * timeLimit - held * held
// held * held - held * timeLimit + recordDistance = 0
// held = (timeLimit +- sqrt(timeLimit * timeLimit - 4 * recordDistance)) / 2
// x = -b +- sqrt(b * b - 4 * a * c) / 2 * a

val z1 = (timeLimit + math.sqrt(timeLimit * timeLimit - 4 * recordDistance)) / 2
val z2 = (timeLimit - math.sqrt(timeLimit * timeLimit - 4 * recordDistance)) / 2

// val zero1 = ((timeLimit + math.sqrt(timeLimit.toDouble * timeLimit - (4 * recordDistance))) / (2 * recordDistance))
// val zero2 = ((timeLimit - math.sqrt(timeLimit.toDouble * timeLimit - (4 * recordDistance))) / (2 * recordDistance))

val zero1 = z1.toLong
val zero2 = z2.toLong

(zero1 < timeLimit)
(zero2 < timeLimit)

zero1.toLong

val delta = zero2 - zero1

(zero1 to zero2 by delta.sign)

val ans2 = (zero1 to zero2 by delta.sign).size - 1


// //
