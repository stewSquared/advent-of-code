val input = io.Source.fromResource("2023/day-08.txt").getLines().toList

val directions = input.head

val adj = input.drop(2).foldLeft(Map.empty[String, (String, String)]):
  case (m, s"$start = ($left, $right)") => m.updated(start, (left, right))

adj foreach println

def move(pos: String, dir: Char): String =
  val (left, right) = adj(pos)
  dir match
    case 'L' => left
    case 'R' => right

def infDir: LazyList[Char] = LazyList(directions:_*) #::: infDir

def positions = Iterator.unfold("AAA", infDir):
  case (pos, dir #:: dirs) =>
    println(s"at $pos going $dir")
    val nextPos = move(pos, dir)
    Some((pos, (nextPos, dirs)))

val ans1 = positions.indexWhere(_ == "ZZZ")
