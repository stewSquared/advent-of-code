// val input = io.Source.fromResource("2015/day-19.txt").getLines().toList

val input = List(
  "H => HO",
  "H => OH",
  "O => HH",
  "e => H",
  "e => O",
  "HOH"
)

val replacements: List[(String, String)] = input.collect:
  case s"$a => $b" => a -> b
// .groupMap(_._1)(_._2)

val medicine = input.last

def newMolecules(left: String, right: String, molecule: String): List[String] =
  def sliceIndices(from: Int): List[Int] =
    molecule.indexOfSlice(left, from) match
      case -1 => Nil
      case i => i :: sliceIndices(i + 1)

  sliceIndices(0).map(molecule.patch(_, right, left.length))

val ms = replacements.flatMap(newMolecules(_, _, medicine))

ms foreach println

ms.size
val ans1 = ms.distinct.size

// def search(): Int =
//   import collection.mutable.{PriorityQueue, Map}
//   val cost = Map[String, Int]
//   ???





// 480 too low
// 448 too low
