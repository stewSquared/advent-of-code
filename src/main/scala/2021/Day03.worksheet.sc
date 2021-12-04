import io.Source
import util.Using

val input = Using(Source.fromResource("2021/day-03-1.txt")) {
  _.getLines.toList
}.get

val gString = input.transpose.map(_.groupBy(identity).maxBy(_._2.size)._1).mkString

val gammaRate = Integer.parseInt(gString, 2)
val epsilonRate = gammaRate ^ Integer.parseInt("1" * gString.length, 2)

val powerConsumption = gammaRate * epsilonRate

val oString = Iterator.unfold(input) { kept =>
  if !kept.exists(_.nonEmpty) then None
  else kept.groupMap(_.head)(_.tail)
    .maxByOption { (head, tails) =>
      tails.size -> head
    }
}.mkString

val cString = Iterator.unfold(input) { kept =>
  if !kept.exists(_.nonEmpty) then None
  else kept.groupMap(_.head)(_.tail)
    .minByOption { (head, tails) =>
      tails.size -> head
    }
}.mkString

val oxyRating = Integer.parseInt(oString, 2)
val co2Rating = Integer.parseInt(cString, 2)

val lifeSupportRating = oxyRating * co2Rating
