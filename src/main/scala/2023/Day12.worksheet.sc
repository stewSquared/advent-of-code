import aoc.*

val input = io.Source.fromResource("2023/day-12-ex.txt").getLines().toVector

input.size

object Foo:
  @annotation.tailrec
  def countSolutions(conditions: String, contiguousDamaged: List[Int], continue: List[(String, List[Int])], counted: Option[Int]): Int =
    println(s"countSolutions($conditions, $contiguousDamaged, $continue, $counted)")
    if conditions.startsWith(".") then
      countSolutions(conditions.dropWhile(_ == '.'), contiguousDamaged, continue, counted)
    else if conditions.isEmpty then
      val c = if contiguousDamaged.isEmpty then 1 else 0
      val c2 = counted.fold(c)(_ + c)
      if continue.isEmpty then c2
      else
        val (condCont, damCont) = continue.head
        countSolutions(condCont, damCont, continue.tail, Some(c2))
    else
      contiguousDamaged match
        case Nil =>
          val c = if conditions.forall("?.".contains) then 1 else 0
          val c2 = counted.fold(c)(_ + c)
          if continue.isEmpty then c2
          else
            val (condCont, damCont) = continue.head
            countSolutions(condCont, damCont, continue.tail, Some(c2))
        case d :: ds =>
          val (matchable, unmatched) = conditions.span("#?".contains) // only need to take d + 1 elements
          if matchable.size < d then
            if conditions.startsWith("?") then
              countSolutions(conditions.drop(1), contiguousDamaged, continue, counted)
            else
              if continue.isEmpty then counted.getOrElse(0)
              else
                val (condCont, damCont) = continue.head
                countSolutions(condCont, damCont, continue.tail, counted)
          else
            // println("should be here")
            val (matched, unmatched) = conditions.splitAt(d)
            if unmatched.headOption.forall("?.".contains) then
              val cont =
                if conditions.startsWith("?") then
                  (conditions.drop(1) -> contiguousDamaged) :: continue
                else continue
              countSolutions(unmatched.drop(1), ds, cont, counted)
            else
              if matched.startsWith("?") then
                countSolutions(matched.drop(1) + unmatched, contiguousDamaged, continue, counted)
              else
                if continue.isEmpty then counted.getOrElse(0)
                else
                  val (condCont, damCont) = continue.head
                  countSolutions(condCont, damCont, continue.tail, counted)

def parse(line: String): (String, List[Int]) = line match
  case s"$conditions $nums" =>
    val contiguousDamaged = nums.split(",").toList.map(_.toInt)
    conditions -> contiguousDamaged

def countSolutions(args: (String, List[Int])): Int =
  val (conditions, contiguousDamaged) = args
  Foo.countSolutions(conditions, contiguousDamaged, Nil, None)

countSolutions(parse(input(0))) - 1
countSolutions(parse(input(1))) - 4
countSolutions(parse(input(2))) - 1
countSolutions(parse(input(3))) - 1
countSolutions(parse(input(4))) - 4
countSolutions(parse(input(5))) - 10

def parse2(line: String): (String, List[Int]) =
  val (conditions, contiguousDamaged) = parse(line)
  List.fill(5)(conditions).mkString("?") -> List.fill(5)(contiguousDamaged).flatten

val ans1 = input.map(parse).map {
  case (conditions, contiguousDamaged) =>
    Foo.countSolutions(conditions, contiguousDamaged, Nil, None)
} foreach println

countSolutions(parse2(input(0))) - 1
// countSolutions(parse2(input(1))) - 16384
countSolutions(parse2(input(2))) - 1
countSolutions(parse2(input(3))) - 16
countSolutions(parse2(input(4))) - 2500
// countSolutions(parse2(input(5))) - 1506250

// val ans2 = input.map(parse2).map(countSolutions.tupled).sum

//
