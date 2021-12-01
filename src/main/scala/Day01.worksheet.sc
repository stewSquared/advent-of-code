import io.Source
import util.Using

val sweep = Using(Source.fromResource("day-01-1.txt")){
  _.getLines().map(_.toInt).toList
}.get

val ans1 = sweep.zip(sweep.tail).count(_ < _)

val ans2 = sweep.zip(sweep.drop(3)).count(_ < _)
