val input = io.Source.fromResource("2023/day-09.txt").getLines().toVector
val hists = input.map(_.split(" ").map(_.toLong).toVector)

def deltas(values: Vector[Long]) = values.zip(values.tail).map((a, b) => b - a)

def prediction(hist: Vector[Long]): Long =
  val finalDeltas = Iterator.unfold(hist): line =>
    Option.unless(line.forall(_ == 0)):
      line.last -> deltas(line)
  finalDeltas.sum

val ans1 = hists.map(prediction).sum
val ans2 = hists.map(_.reverse).map(prediction).sum
