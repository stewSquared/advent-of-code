val input = io.Source.fromResource("2015/day-12.txt").getLines().next()

import util.chaining.*

val numbers = "-?[0-9]+".r
val ans1 = numbers.findAllMatchIn(input).map(_.toString.toInt).sum
