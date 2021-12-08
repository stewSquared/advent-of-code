val report = io.Source.fromResource("2021/day-03-1.txt").getLines.toList

val mostCommon = report.transpose.map(_.groupBy(identity).maxBy(_._2.size)._1)

val gammaRate = Integer.parseInt(mostCommon.mkString, 2)
val epsilonRate = gammaRate ^ Integer.parseInt("1" * mostCommon.length, 2)

val oString = Iterator.unfold(report) { kept =>
  if !kept.exists(_.nonEmpty) then None
  else kept.groupMap(_.head)(_.tail)
    .maxByOption((head, tails) => tails.size -> head)
}.mkString

val cString = Iterator.unfold(report) { kept =>
  if !kept.exists(_.nonEmpty) then None
  else kept.groupMap(_.head)(_.tail)
    .minByOption((head, tails) => tails.size -> head)
}.mkString

val oxyRating = Integer.parseInt(oString, 2)
val co2Rating = Integer.parseInt(cString, 2)

val powerConsumption = gammaRate * epsilonRate
val lifeSupportRating = oxyRating * co2Rating
