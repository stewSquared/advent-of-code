val input = io.Source.fromResource("2015/day-19.txt").getLines().toList

val replacements: Map[String, List[String]] = input.collect:
  case s"$a => $b" => a -> b
.groupMap(_._1)(_._2)

val medicine = input.last

3 + 2

// val newMolecules = replacements.flatMap:
//   case (left, rights) => rights.map: right =>
//     def search(start: Int, acc: List[String]): List[String] =
//       val i = medicine.indexOfSlice(left)
//       if i == -1 then Nil else
//         val newString = medicine.patch(i, right, left.length)
//         search(i + right.length, newString :: acc)
//     search(0, Nil)

// val ans1 = newMolecules.toList.distinct.size
