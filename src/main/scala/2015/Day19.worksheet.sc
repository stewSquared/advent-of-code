val input = io.Source.fromResource("2015/day-19.txt").getLines().toList

// val input = List(
// "H => HO",
// "H => OH",
// "O => HH",
// )

val replacements: List[(String, String)] = input.collect:
  case s"$a => $b" => a -> b
// .groupMap(_._1)(_._2)

val medicine = input.last
// val medicine = "HOHOHO"

def newMolecules(left: String, right: String, molecule: String): List[String] =
  def search(start: Int, acc: List[String]): List[String] =
    val i = medicine.indexOfSlice(left, start)
    if i == -1 then acc else
      val newMolecule = medicine.patch(i, right, left.length)
      search(i + right.length, newMolecule :: acc)
  search(0, Nil)

replacements.head

val ms = replacements.flatMap(newMolecules(_, _, medicine))

ms.size
val ans1 = ms.distinct.size
// 448 too low
