io.Source.fromResource("2025/day-12.txt").getLines.drop(30).count:
  case s"${w}x${l}: $ns" =>
    ns.split(' ').map(_.toInt).sum * 9 <= w.toInt * l.toInt
