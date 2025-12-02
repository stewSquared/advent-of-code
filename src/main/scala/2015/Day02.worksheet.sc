val input = io.Source.fromResource("2015/day-02.txt").getLines().toList

val dimensions = input.map:
  case s"${l}x${w}x${h}" => (l.toInt, w.toInt, h.toInt)

val paperAreas = dimensions.map:
  case (l, w, h) =>
    val surfaceArea = (l*w + w*h + l*h)*2
    val slack = List(l,w,h).sorted.take(2).product
    surfaceArea + slack

val ans1 = paperAreas.sum

val ribbonLengths = dimensions.map:
  case (l, w, h) =>
    List(l+w, w+h, l+h).min * 2 + l*w*h

val ans2 = ribbonLengths.sum

2 + 2

//
