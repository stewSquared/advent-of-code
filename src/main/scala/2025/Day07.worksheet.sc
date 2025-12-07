val input = io.Source.fromResource("2025/day-07.txt").getLines().toList

val start = input.head.indexOf("S")

input.size
input.map(_.count(_ == '^')).sum

val counts = collection.mutable.Map[Int, Long](start -> 1L).withDefaultValue(0L)

var splits = 0

for line <- input.tail do
  val splitting = counts.filter:
    case (i, c) if c != 0 => line(i) == '^'
    case _ => false
  .toList

  for
    (i, c) <- splitting
  do
    splits += 1
    counts(i) -= c
    counts(i + 1) += c
    counts(i - 1) += c

val ans1 = splits
val ans2 = counts.values.sum

// val finalBeams = input.tail.foldLeft(Vector(start)):
//   case (beams, line) =>
//     beams.flatMap: b =>
//       if line(b) == '^' then
//         Vector(b - 1, b + 1)
//       else Vector(b)

// val ans1 = finalBeams.size - 1

2 + 2
//
