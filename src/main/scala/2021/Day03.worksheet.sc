val report = io.Source.fromResource("2021/day-03-1.txt").getLines.toList

val mostCommon = report.transpose.map(_.groupBy(identity).maxBy(_._2.size)._1)

val gammaRate = Integer.parseInt(mostCommon.mkString, 2)
val epsilonRate = gammaRate ^ Integer.parseInt("1" * mostCommon.length, 2)

val oxyBits = Iterator.unfold(report) { kept =>
  Option.when(kept.exists(_.nonEmpty)) {
    kept.groupMap(_.head)(_.tail).maxBy((h, t) => t.size -> h)
  }
}

val co2Bits = Iterator.unfold(report) { kept =>
  Option.when(kept.exists(_.nonEmpty)) {
    kept.groupMap(_.head)(_.tail).minBy((h, t) => t.size -> h)
  }
}

val oxyRating = Integer.parseInt(oxyBits.mkString, 2)
val co2Rating = Integer.parseInt(co2Bits.mkString, 2)

val powerConsumption = gammaRate * epsilonRate
val lifeSupportRating = oxyRating * co2Rating
