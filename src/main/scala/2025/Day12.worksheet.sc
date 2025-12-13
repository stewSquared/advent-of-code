val input = io.Source.fromResource("2025/day-12.txt").getLines().toList

val shapeVolumes = List.from:
  Iterator.unfold(input): lines =>
    Option.when(lines.head.endsWith(":")):
      val grid = lines.slice(1,5)
      val volume = grid.map(_.count(_ == '#')).sum
      volume -> lines.drop(5)

val regions = input.collect:
  case s"${w}x${l}: $counts" =>
    val area = w.toInt * l.toInt
    area -> counts.split(' ').map(_.toInt).toList

val lowerBound = regions.count:
  case (area, counts) =>
    shapeVolumes.zip(counts).map(_ * _).sum <= area

val ans1 = lowerBound
