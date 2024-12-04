val input = io.Source.fromResource("2024/day-03.txt").getLines().toList

enum Instruction:
  case Do
  case Dont
  case Mul(a: Int, b: Int)

import Instruction.{Do, Dont, Mul}

val mulRegex = "mul\\(\\d+,\\d+\\)".r
val doRegex = "do\\(\\)".r
val dontRegex = "don't\\(\\)".r

val instRegex = s"^(${mulRegex}|${doRegex}|${dontRegex}).*".r

def nextMul(s: String): Option[(Instruction, String)] =
  mulRegex.findFirstMatchIn(s).map: m =>
    val s"mul($x,$y)" = m.matched
    (Mul(x.toInt, y.toInt), s.drop(m.end))

val allMuls = Iterator.unfold(input.mkString)(nextMul).toList

val ans1 = allMuls.map:
  case Mul(a, b) => a * b
.sum

def findNext(s: String): Option[(Instruction, String)] =
  Option.when(s.nonEmpty):
    s match
      case instRegex(inst) => inst match
        case s"mul($x,$y)" => (Mul(x.toInt, y.toInt), s.drop(inst.size))
        case "do()" => (Do, s.drop(inst.size))
        case "don't()" => (Dont, s.drop(inst.size))
      case _ => findNext(s.drop(1)).get

val instructions = Iterator.unfold(input.mkString)(findNext).toList

val (ans2, _) = instructions.foldLeft(0, true):
  case ((sum, enabled), instruction) =>
    instruction match
      case Do => (sum, true)
      case Dont => (sum, false)
      case Mul(a, b) =>
        if enabled then (sum + a * b, enabled)
        else (sum, enabled)
