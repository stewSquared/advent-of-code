val input = io.Source.fromResource("2023/day-08.txt").getLines().toList

val directions = input.head

val adj = input.drop(2).foldLeft(Map.empty[String, (String, String)]):
  case (m, s"$start = ($left, $right)") => m.updated(start, (left, right))

def move(pos: String, dir: Char): String =
  val (left, right) = adj(pos)
  dir match
    case 'L' => left
    case 'R' => right

def infDir: LazyList[Char] = LazyList(directions:_*) #::: infDir

def positions = infDir.scanLeft("AAA")(move)

val ans1 = positions.indexWhere(_ == "ZZZ")

val starts = adj.keys.filter(_.endsWith("A")).toList

val destinations = adj.keysIterator
  .map(start => start -> directions.foldLeft(start)(move))
  .toMap

val periods = starts.map: start =>
  Iterator.iterate(start)(destinations).indexWhere(_.endsWith("Z"))

val ans2 = periods.map(_.toLong).product * directions.length
