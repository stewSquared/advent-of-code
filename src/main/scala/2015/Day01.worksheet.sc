val input = io.Source.fromResource("2015/day-01.txt").getLines().next()

val ans1 = input.count(_ == '(') - input.count(_ == ')')

val positions = input.scanLeft(0):
  case (l, '(') => l + 1
  case (l, ')') => l - 1

val ans2 = positions.indexWhere(_ < 0)
