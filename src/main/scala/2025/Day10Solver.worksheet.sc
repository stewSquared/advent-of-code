import optimus.algebra.Constraint
import optimus.optimization.*
import optimus.optimization.enums.SolverLib
import optimus.optimization.subjectTo
import optimus.optimization.model.MPIntVar
import optimus.algebra.ConstraintRelation.EQ
import optimus.algebra.Const
import optimus.algebra.Expression

val input = io.Source.fromResource("2025/day-10.txt").getLines().toList

case class Machine(buttons: List[Set[Int]], joltages: Vector[Int]):
  def solve(using MPModel): Long =
    val vars = buttons.zip('a' to 'z').map:
      case (b, l) =>
        // val max = joltages.filter(b.contains).min
        MPIntVar(l.toString, 0 to joltages.max) -> b
    .toMap

    // vars.toList.sortBy(_._1.symbol) foreach println

    val constraints = joltages.zipWithIndex.map:
      case (j, i) =>
        val vs = vars.collect:
          case (v, b) if b.contains(i) => v
        Constraint(vs.reduce(_ + _), EQ, Const(j))

    // constraints foreach println

    val sum: Expression = vars.keys.reduce(_ + _)

    minimize(sum)
    subjectTo(constraints*)
    start()
    vars.keys.toList.map: v =>
      v.value.get.round
      //.tapEach(value => println(s"${v.symbol} = $value"))
    .sum

val machines = input.collect:
  case s"[$goal] $buttons {$joltages}" =>
    val buttonInts = buttons.split(' ').toList.map: s =>
      s.slice(1, s.length - 1).split(',').map(_.toInt).toSet

    Machine(buttonInts, joltages.split(',').map(_.toInt).toVector)

machines.map(_.solve(using MPModel())).sum

// upper bound 65021
machines.map(_.joltages.map(_.toLong).sum).sum

// 16459 too low
