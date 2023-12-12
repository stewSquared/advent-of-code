import aoc.*

val input = io.Source.fromResource("2023/day-12.txt").getLines().toVector

input.size

def countSolutions(conditions: String, contiguousDamaged: List[Int]): Int =
  println(s"countSolutions($conditions, $contiguousDamaged)")
  if conditions.isEmpty then
    if contiguousDamaged.isEmpty then 1 else 0
  else
    contiguousDamaged match
      case Nil =>
        if conditions.forall("?.".contains) then 1 else 0
      case d :: ds =>
        val (matchable, unmatched) = conditions.span("#?".contains) // only need to take d + 1 elements
        if matchable.size < d then
          if conditions.headOption.forall("?.".contains) then
            countSolutions(conditions.drop(1), contiguousDamaged)
          else 0
        else
          // println("should be here")
          val (matched, unmatched) = conditions.splitAt(d)
          if unmatched.headOption.forall("?.".contains) then
            val matchedSolutions = countSolutions(unmatched.drop(1), ds)
            // println(s"solutions where matched from start $matchedSolutions")
            val unmatchedSolutions =
              if conditions.headOption.forall("?.".contains) then
                countSolutions(conditions.drop(1), contiguousDamaged)
              else 0
            // println(s"solutions where dropped 1 from start $unmatchedSolutions")
            matchedSolutions + unmatchedSolutions
          else
            if matched.startsWith("?") then
              countSolutions(matched.drop(1) + unmatched, contiguousDamaged)
            else 0

def parse(line: String): (String, List[Int]) = line match
  case s"$conditions $nums" =>
    val contiguousDamaged = nums.split(",").toList.map(_.toInt)
    conditions -> contiguousDamaged

// countSolutions.tupled(parse(input(0))) - 1
// countSolutions.tupled(parse(input(1))) - 4
// countSolutions.tupled(parse(input(2))) - 1
// countSolutions.tupled(parse(input(3))) - 1
// countSolutions.tupled(parse(input(4))) - 4
// countSolutions.tupled(parse(input(5))) - 10

val ans1 = input.map(parse).map(countSolutions.tupled).sum

//
