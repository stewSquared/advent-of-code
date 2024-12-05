val input = io.Source.fromResource("2018/day-03.txt").getLines().toList

import aoc.{Point, Area}

val claims: List[Area] = input.collect:
  case s"#${id} @ ${x},${y}: ${w}x${h}" =>
    val xRange = x.toInt until x.toInt + w.toInt
    val yRange = y.toInt until y.toInt + h.toInt
    Area(xRange, yRange)

val (_, overlapping) = claims.foldLeft((Set.empty[Point], Set.empty[Point])):
  case ((seen, overlap), claim) =>
    val (newOverlap, newSeen) = claim.pointsIterator.partition(seen)
    (seen ++ newSeen, overlap ++ newOverlap)

val ans1 = overlapping.size

val ans2 = 1 + claims.indexWhere: claim =>
  claim.pointsIterator.forall(!overlapping(_))
