val input = io.Source.fromResource("2023/day-12.txt").getLines().toVector

def parse(line: String): (String, List[Int]) = line match
  case s"$record $groups" =>
    val contiguousDamaged = groups.split(",").toList.map(_.toInt)
    (record + ".") -> contiguousDamaged

def parse2(line: String): (String, List[Int]) = line match
  case s"$record $groups" =>
    val contiguousDamaged = groups.split(",").toList.map(_.toInt)
    (List.fill(5)(record).mkString("?") + ".") -> List.fill(5)(contiguousDamaged).flatten

def matchRanges(record: String, from: Int, groupSize: Int): List[Int] =
  val nextDamaged = record.indexOf("#", from)
  val operational = (from until record.length).filter(record(_) == '.').toSet

  val lastFrom = List(nextDamaged, record.size - groupSize - 1).filterNot(_ == -1).min
  val froms = (from to lastFrom).filter: i =>
    val noneOpreational = !(i until (i + groupSize)).exists(operational)
    val notDamagedAfter = record(i + groupSize) != '#'
    noneOpreational && notDamagedAfter

  froms.toList

def count(record: String, groups: List[Int]): Long =
  val memo = collection.mutable.Map.empty[(String, Int, List[Int]), Long]

  def search(record: String, from: Int, groups: List[Int]): Long =
    lazy val calculate: Long =
      if groups.isEmpty then
        if record.indexOf("#", from) == -1 then 1 else 0
      else
        val recursiveCounts = matchRanges(record, from, groups.head).map: i =>
          search(record, i + groups.head + 1, groups.tail)
        recursiveCounts.sum
    memo.getOrElseUpdate((record, from, groups), calculate)

  search(record, 0, groups)

val ans1 = input.map(parse).map(count.tupled).sum
val ans2 = input.map(parse2).map(count.tupled).sum
