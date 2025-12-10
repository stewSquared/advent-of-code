val input = io.Source.fromResource("2025/day-10.txt").getLines().toList.drop(2).take(1)

case class Machine(goal: Int, buttons: List[Int], joltages: List[Int])

val machines = input.collect:
  case s"[$goal] $buttons {$joltages}" =>
    val goalBin = goal.map(c => if c == '#' then '1' else '0').mkString
    val goalInt = Integer.parseInt(goalBin, 2)

    val buttonInts = buttons.split(' ').toList.map: s =>
      val ones = s.slice(1, s.length - 1).split(',').map(_.toInt).toSet
      val buttonBin = goalBin.indices.map: i =>
        if ones(i) then '1' else '0'
      .mkString
      Integer.parseInt(buttonBin, 2)

    Machine(goalInt, buttonInts, joltages.split(',').map(_.toInt).toList)

def presses(machine: Machine): Int =
  machine.buttons.indices.drop(1).find: numButtons =>
    machine.buttons.combinations(numButtons).exists: comb =>
      comb.reduce(_ ^ _) == machine.goal
  .get

val ans1 = machines.map(presses).sum

def presses2(machine: Machine): Int =
  // println(machine.joltages.mkString(" "))
  def press(button: Int, sum: List[Int]): List[Int] =
    val bin = button.toBinaryString.reverse.padTo(machine.joltages.length, '0').reverse
    sum.zip(bin).map:
      case (n, bit) => n + bit.asDigit

  def search(): Int =
    import collection.mutable.{PriorityQueue, Map}
    val start = machine.joltages.map(_ => 0)
    val cost = Map(start -> 0)
    val pq = PriorityQueue.empty[List[Int]](Ordering.by(s => cost(s) -> (machine.joltages.sum - s.sum))).reverse

    var visiting = start

    while visiting != machine.joltages do
      println(visiting)
      for
        s <- machine.buttons.map(press(_, visiting))
        if !cost.contains(s)
        if s.zip(machine.joltages).forall(_ <= _)
      do
        cost(s) = cost(visiting) + 1
        pq.enqueue(s)
      visiting = pq.dequeue

    cost(visiting)

  search()

// val ans2 = machines.map(presses2(_)).sum

//
// a(x + z) + b(x + y + w) == 28x + 20y + 8z + 20w
// ax + az + bx + by + bw
// x(a + b) + yb + az + bw == 28x + 20y + 8z + 20w
// x(a + b) + yb + az + bw == 28x + 20y + 8z + 20w
// a + b == 28
// b = 20
// a = 8
// b = 20


//[..#...#.]
// (0,1,4,5,6,7) (0,2,3) (0,1,2,3,5,6,7) (0,1,2,3,6) (1,4) (0,1,2,5,6,7) {215,215,28,19,198,204,204,204}

// (0,1,4,5,6,7) (0,2,3) (0,1,2,3,5,6,7) (0,1,2,3,6) (1,4) (0,1,2,5,6,7) {215s,215t,28u,19v,198w,204x,204y,204z}

// a(s + t + w + x + y + z) + b(s + u + v) ...

// a + b + c + d +     f = 215
// a +     c + d + e + f = 215
//     b + c + d +     f = 28  // f = 9
//     b + c + d         = 19
// a +             e     = 198
// a +     c +         f = 204
// a +     c + d +     f = 204 // d = 0
// a +     c +         f = 204

val a = 187
val b = 11
val c = 8
val d = 0
val e = 11
val f = 9

a + b + c + d + e + f   //= 215

a + b + c + d +     f   //= 215
a +     c + d + e + f   //= 215
    b + c + d +     f  // = 28
    b + c + d          // = 19
a +             e       //= 198
a +     c +         f   //= 204
a +     c + d +     f   //= 204
a +     c +         f   //= 204



// a = 187
// b = 11
// c = 8
// d = 0
// e = 11
// f = 9

// e = c + 3
// 187 + e = 198

// a = 215 - 28

// a + e = 198
// a + c = 195
// e - c = 3
// e = c + 3

// f = 28 - 19 = 9
// f = 9
// d = 0
// b + c = 19
// b - e = 0
// b = e
//
