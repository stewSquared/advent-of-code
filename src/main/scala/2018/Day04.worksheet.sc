val input = io.Source.fromResource("2018/day-04.txt").getLines().toList.sorted

input foreach println

type Time = String
enum Event(time: Time):
  // case BeginShift(id: Int)
  case Asleep(time: Time) extends Event(time)
  case Awake(time: Time) extends Event(time)

import Event.{Asleep, Awake}

input.take(20) foreach println

case class Shift(id: Int, start: Time, events: List[Event])

def parseShift(logs: List[String]): (Shift, List[String]) =
  assert(logs.nonEmpty)
  val s"[$timestamp] Guard #$id begins shift" = logs.head
  val (activity, remainder) = logs.tail.span(!_.contains('#'))
  println(s"parsing events from $activity")
  val events = activity.map:
    case s"[$timestamp] falls asleep" => Asleep(timestamp)
    case s"[$timestamp] wakes up" => Awake(timestamp)
  (Shift(id.toInt, timestamp, events), remainder)

val allShifts: List[Shift] =
  Iterator.unfold(input): logs =>
    Option.when(logs.nonEmpty)(parseShift(logs))
  .toList

val shiftsByGuard = allShifts.groupBy(_.id)

def whichMinutesAsleep(shift: Shift): List[Int] =
  shift.events.grouped(2).flatMap:
    case List(Asleep(start), Awake(end)) =>
      val s"$day1 $h1:$m1" = start
      val startMin = h1.toInt * 60 + m1.toInt

      val s"$day2 $h2:$m2" = end
      val endMin = h2.toInt * 60 + m2.toInt

      (startMin until endMin)
  .toList

def totalMinutesAsleep(shift: Shift): Int =
  whichMinutesAsleep(shift).size

val (sleepiestGuard, sleepyShifts) =
  shiftsByGuard.maxBy:
    case (id, shifts) => shifts.map(totalMinutesAsleep).sum

val ans1 = sleepiestMinute(sleepyShifts)._1 * sleepiestGuard

def sleepiestMinute(shifts: List[Shift]): (Int, Int) =
  shifts
    .flatMap(whichMinutesAsleep)
    .groupMapReduce(identity)(_ => 1)(_ + _)
    .maxByOption(_._2).getOrElse((0, 0))

val ans2 = shiftsByGuard.map:
  case (id, shifts) => (id, sleepiestMinute(shifts))
.maxByOption:
  case (id, (minute, count)) => count
.map:
  case (id, (minute, count)) => id * minute
