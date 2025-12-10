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
