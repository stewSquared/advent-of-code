val input = io.Source.fromResource("2024/day-03.txt").getLines().toList

input.size

enum Instruction:
  case Do
  case Dont
  case Mul(a: Int, b: Int)

import Instruction.{Do, Dont, Mul}

def parseMul(s: String): (Option[Instruction], String) =
  val mulStart = s.indexOf("mul")

  val mul =
    val maybeLR = util.Try:
      val s"${pre}mul($a,$b)$remainder" = s.drop(mulStart)
      (a, b)
    .toOption
    for
      (l, r) <- maybeLR
      a <- l.toIntOption
      b <- r.toIntOption
    yield Mul(a, b)

  mul -> s.drop(mulStart+3)

def parseDo(s: String): (Option[Instruction], String) =
  val doStart = s.indexOf("do()")
  assert(doStart >= 0)
  Some(Do) -> s.drop(doStart+4)

def parseDont(s: String): (Option[Instruction], String) =
  val dontStart = s.indexOf("don't()")
  assert(dontStart >= 0)
  Some(Dont) -> s.drop(dontStart+7)

val allMuls =
  Iterator.unfold(input.mkString): s =>
    Option.when(s.contains("mul")):
      parseMul(s)

val ans1 = allMuls.flatten.toList.map:
  case Mul(a, b) => a * b
.sum

val allInstructions =
  Iterator.unfold(input.mkString): s =>
    Option.when(s.contains("mul") || s.contains("do()") || s.contains("don't()")):
      val mulStart = s.indexOf("mul")
      val doStart = s.indexOf("do()")
      val dontStart = s.indexOf("don't()")

      if mulStart >= 0 && (doStart < 0 || mulStart < doStart) && (dontStart < 0 || mulStart < dontStart) then
        parseMul(s)
      else if doStart >= 0 && (dontStart < 0 || doStart < dontStart) then
        parseDo(s)
      else
        parseDont(s)

// import util.chaining.*

// val (ans2, _) = allInstructions.flatten.toList.foldLeft(0, true):
//   case ((sum, enabled), instruction) =>
//     instruction match
//       case Do => (sum, true)
//       case Dont => (sum, false)
//       case Mul(a, b) =>
//         if enabled then (sum + a * b, enabled)
//         else (sum, enabled)

//
