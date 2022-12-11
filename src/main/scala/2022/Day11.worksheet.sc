val lines = io.Source.fromResource("2022/day-11.txt").getLines()

def splitNewlines(lines: Iterator[String]): List[List[String]] =
  val lb = collection.mutable.ListBuffer.empty[List[String]]
  while lines.hasNext do lb += lines.takeWhile(_.nonEmpty).toList
  lb.result()

case class Monkey(
  items: List[BigInt],
  operation: BigInt => BigInt,
  test: BigInt => Int
)

val bigDiv = io.Source.fromResource("2022/day-11.txt").getLines().collect {
  case s"  Test: divisible by $x" => BigInt(x, 10)
}.product

val startState: Vector[Monkey] = splitNewlines(lines).toVector.map { monkeyLines =>
  Monkey(
  items = monkeyLines(1)
    .stripPrefix("  Starting items: ")
    .split(", ").map(BigInt.apply(_, 10)).toList,
   operation = (monkeyLines(2).stripPrefix("  Operation: new = old ") match
    case s"* old" => n => n * n
    case s"* $x" => n => n * BigInt(x, 10)
    case s"+ $x" => n => n + BigInt(x, 10)),
    test = ( monkeyLines(3) match
      case s"  Test: divisible by $x" => n =>
        if n % BigInt(x, 10) == 0 then
          monkeyLines(4).stripPrefix("    If true: throw to monkey ").toInt
        else
          monkeyLines(5).stripPrefix("    If false: throw to monkey ").toInt
    )
  )
}

def turn(monkey: Monkey, worryMore: Boolean): (List[(BigInt, Int)], Monkey) =
  monkey.items.map { item =>
    val newWorry = if worryMore then (monkey.operation(item) % bigDiv)
    else (monkey.operation(item) / 3)
    newWorry -> monkey.test(newWorry)
  } -> monkey.copy(items = Nil)

def round(monkeys: Vector[Monkey], worryMore: Boolean): (Vector[Long], Vector[Monkey]) =
  var endState = monkeys
  monkeys.indices.map { i =>
    val (thrown, nm) = turn(endState(i), worryMore)
    endState = endState.updated(i, endState(i).copy(items = Nil))
    thrown.foreach {
      case (w, cm) =>
        endState = endState.updated(cm, endState(cm).copy(endState(cm).items :+ w))
    }
    thrown.size.toLong
  }.toVector -> endState

var state = startState
var counts = Vector.fill(state.size)(0L)
for i <- 1 to 10000 do
  val (roundCounts, nextState) = round(state, worryMore = true)
  state = nextState
  counts = counts.zip(roundCounts).map(_ + _).toVector

startState.flatMap(_.items).size
36 * 1000
counts.sum
counts.sum.toDouble / (36 * 1000)

counts
counts.sorted.takeRight(2)
val ans1 = counts.sorted.takeRight(2).product
ans1 / counts.max

// 25738411485
// 27641421222
// 24563830340
// 245793351241
// 246410890
// 252724842
