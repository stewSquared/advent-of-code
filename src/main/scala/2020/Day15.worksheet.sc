
val input = io.Source.fromResource("2020/day-15.txt").getLines.toList

case class State(last: Int, turns: Int, history: Map[Int, Int]):
  def next: State =
    val speaking = history.get(last).fold(0)(turns - _)
    copy(last = speaking, turns + 1, history = history.updated(last, turns))


var exampleState = State(6, 3, Map(0 -> 1, 3 -> 2))

exampleState = exampleState.next
print(exampleState)
exampleState = exampleState.next
print(exampleState)
exampleState = exampleState.next
print(exampleState)
exampleState = exampleState.next
print(exampleState)
exampleState = exampleState.next
print(exampleState)
exampleState = exampleState.next
print(exampleState)
exampleState = exampleState.next
print(exampleState)


// val start = State(6, 3, Map(0 -> 1, 3 -> 2))

val start = State(
  0, 6,
  Map(
    12 -> 1,
    1 -> 2,
    16 -> 3,
    3 -> 4,
    11 -> 5
    )
  )

val ans1 = Iterator.iterate(start)(_.next).collectFirst {
  case State(s, 2020, _) => s
}.get

// val ans2 = Iterator.iterate(start)(_.next).collectFirst {
//   case State(s, 30000000, _) => s
// }.get

// TODO 20 seconds can optimize
