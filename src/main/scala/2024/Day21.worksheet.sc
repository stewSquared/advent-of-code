import aoc.{Area, Point, Dir}

val input = io.Source.fromResource("2024/day-21.txt").getLines.toList

val numKeyPad: Map[Char, Point] =
  val raw =
    """|789
       |456
       |123
       | 0A""".stripMargin
  val grid = raw.split("\n").toVector
  Area(grid).pointsIterator.collect:
    case p if grid(p) != ' ' =>
      grid(p) -> p
  .toMap

val dirKeyPad: Map[Char, Point] =
  val raw =
    """| ^A
       |<v>""".stripMargin
  val grid = raw.split("\n").toVector
  Area(grid).pointsIterator.collect:
    case p if grid(p) != ' ' =>
      grid(p) -> p
  .toMap

case class State(pos: Point, path: String):
  def next: List[State] = List(
    State(pos.n, path + '^'),
    State(pos.s, path + 'v'),
    State(pos.w, path + '<'),
    State(pos.e, path + '>')
  )

def paths(from: Char, to: Char, keyPad: Map[Char, Point]): List[String] =
  val startPos = keyPad(from)
  val endPos = keyPad(to)
  val steps = LazyList.iterate(List(State(startPos, ""))): paths =>
    paths.flatMap(_.next.filter(s => keyPad.values.toSet.contains(s.pos))).distinct
  val dist = startPos.dist(endPos)
  steps(dist).collect:
    case State(pos, path) if pos == endPos => path

val dirKeyPaths: Map[(Char, Char), List[String]] = Map.from:
  for
    from <- "A^v<>"
    to <- "A^v<>"
  yield (from, to) -> paths(from, to, dirKeyPad).map(_ + 'A')

val numKeyPaths: Map[(Char, Char), List[String]] = Map.from:
  for
    from <- ('0' to '9') :+ 'A'
    to <- ('0' to '9') :+ 'A'
  yield (from, to) -> paths(from, to, numKeyPad).map(_ + 'A')


def fullCodeMinPath(code: String): Int =
  ('A' +: code).zip(code).map:
    case (from, to) => minPath3Deep(from, to)
  .sum

def minPath(depth: Int, code: String): Int= ???

// val ans1 = input.map: code =>
//   fullCodeMinPath(code) * (code.init).toInt
// .sum

val memo = collection.mutable.Map.empty[(Char, Char, Int), Long]

def dirPathLength(from: Char, to: Char, depth: Int): Long =
  lazy val calc: Long =

    if depth == 0L then dirKeyPaths(from -> to).map(_.length).min
    else
      dirKeyPaths(from -> to).map: path =>
        ('A' +: path).zip(path).map:
          case (from, to) => dirPathLength(from, to, depth - 1)
        .sum
      .min
  memo.getOrElseUpdate((from, to, depth), calc)

def numPathLength(code: String, depth: Int): Long =
  ('A' +: code).zip(code).map:
    case(from, to) =>
      numKeyPaths(from -> to).map: path =>
        ('A' +: path).zip(path).map:
          case (from, to) => dirPathLength(from, to, depth)
        .sum
      .min
  .sum

val ans1 = input.map: code =>
  numPathLength(code, 24) * (code.init).toInt
.sum


// val memo = collection.mutable.Map.empty[(Char, Char, Int), Long]

// def dirPathLength(path: String, depth: Int): Long =
//   ('A' +: path).zip(path).map[Long]:
//     case (from, to) =>
//       if depth == 0L then path.length.toLong else
//         lazy val recurse =
//           dirKeyPaths(from -> to).map(dirPathLength(_, depth - 1)).min
//         memo.getOrElseUpdate((from, to, depth), recurse)
//   .sum

// def numPathLength(code: String, depth: Int): Long =
//   ('A' +: code).zip(code).map:
//     case(from, to) =>
//       numKeyPaths(from -> to).map(dirPathLength(_, depth)).min
//   .sum

// val ans1 = input.map: code =>
//   numPathLength(code, 0) * (code.init).toInt
// .sum









def minPath3Deep(from: Char, to: Char): Int =
  numKeyPaths(from -> to).map: path =>
    ('A' +: path).zip(path).map:
      case (from, to) => minPath1Deep(from, to)
    .sum
  .min

minPath3Deep('A','0')

instructionsInstructions("<")

// def minPath2Deep(from: Char, to: Char): Int =
//   dirKeyPaths(from -> to).map: path =>
//     ('A' +: path).zip(path).map:
//       case (from, to) => minPath1Deep(from, to)
//     .sum
//   .min

def minPath1Deep(from: Char, to: Char): Int =
  dirKeyPaths(from -> to).map: path =>
    ('A' +: path).zip(path).map:
      case (from, to) => dirMinPath(from, to)
    .sum
  .min

def dirMinPath(from: Char, to: Char): Int =
  dirKeyPaths(from -> to).map(_.length).min


def keyCodeInstructions(instructions: String): List[String] =
  val inputOptions: List[List[String]] =
    ('A' +: instructions).zip(instructions).toList.map:
      case (from, to) => numKeyPaths(from -> to)

  inputOptions.foldLeft(List("")): (acc, options) =>
    acc.flatMap(pre => options.map(pre + _))


def instructionsInstructions(instructions: String): List[String] =
  val inputOptions: List[List[String]] =
    ('A' +: instructions).zip(instructions).toList.map:
      case (from, to) => dirKeyPaths(from -> to)

  inputOptions.foldLeft(List("")): (acc, options) =>
    acc.flatMap(pre => options.map(pre + _))



keyCodeInstructions("029A")
instructionsInstructions("<A^A") foreach println

keyCodeInstructions("0")
threeDeepPaths("0")

keyCodeInstructions("029A")
  .flatMap(instructionsInstructions)
  .count(_.length == 28)

keyCodeInstructions("02")
  .flatMap(instructionsInstructions)
  .filter(_.length == 8) foreach println


def countMin(paths: List[String]): Int =
  paths.groupMapReduce(identity)(_.length)(_ + _)
    .minBy(_._1)._2

countMin(threeDeepPaths("0"))
countMin(threeDeepPaths("2"))


def threeDeepPaths(code: String) =
  Iterator.iterate(keyCodeInstructions(code)): paths =>
    paths.flatMap(instructionsInstructions)
  .drop(3).next

// threeDeepPaths("029A").map(_.length).min

// keyCodeInstructions("029A")
// keyCodeInstructions("029A").flatMap(instructionsInstructions)
