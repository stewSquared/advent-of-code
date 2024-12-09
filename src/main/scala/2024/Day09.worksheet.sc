val input = io.Source.fromResource("2024/day-09.txt").getLines.mkString
// val input = "2333133121414131403"

import aoc.Interval

val diskMap: Map[Int, Interval[Int]] = input
  .grouped(2)
  .zipWithIndex
  .foldLeft(List.empty[(Int, Interval[Int])], 0):
    case (state, (chars, id)) if chars.size == 2 =>
      val fileBlocksSize = chars(0).asDigit
      val freeBlocksSize = chars(1).asDigit
      val (blocks, pos) = state
      val newBlock = (id -> Interval(pos, pos + fileBlocksSize - 1))
      (newBlock :: blocks) -> (pos + fileBlocksSize + freeBlocksSize)
    case (state, (chars, id)) =>
      val fileBlocksSize = chars(0).asDigit
      val freeBlocksSize = 0
      val (blocks, pos) = state
      val newBlock = (id -> Interval(pos, pos + fileBlocksSize - 1))
      (newBlock :: blocks) -> (pos + fileBlocksSize + freeBlocksSize)
  ._1.reverse
  .toMap

val occupied = // pos -> id
  diskMap.toList.flatMap:
    case (id, interval) =>
      interval.toRange.map(_ -> id).toList
  .toMap[Int, Int]

val occupiedReverse = // ids from end
   occupied.toList.sortBy(_._1).map(_._2).reverse

val sizeOccupied = diskMap.values.map(_.size).sum

object TailRec:
  def checkSum(from: Int, fillFrom: List[Int], sum: Long): Long =
    if from == sizeOccupied then sum else
      if occupied.contains(from) then
        println(s"$from * ${occupied(from)}")
        checkSum(from + 1, fillFrom, sum + from * occupied(from))
      else
        println(s"$from * ${fillFrom.head}")
        checkSum(from + 1, fillFrom.tail, sum + from * fillFrom.head)

val ans1 = TailRec.checkSum(0, occupiedReverse, 0)

val freeIntervals: List[Interval[Int]] = input
  .grouped(2)
  .filter(_.size == 2)
  .foldLeft(0, List.empty[Interval[Int]]):
    case ((index, intervals), chars) =>
      val fileBlocksSize = chars(0).asDigit
      val freeBlocksSize = chars(1).asDigit
      val newIndex = index + fileBlocksSize + freeBlocksSize

      if freeBlocksSize == 0 then
        newIndex -> intervals
      else
        val newInterval = Interval((index + fileBlocksSize) until newIndex)
        newIndex -> (newInterval :: intervals)
  ._2.reverse

val occupiedIntervals: List[Interval[Int]] = (input + "0")
  .grouped(2)
  .foldLeft(0, List.empty[Interval[Int]]):
    case ((index, intervals), chars) =>
      val fileBlocksSize = chars(0).asDigit
      val freeBlocksSize = chars(1).asDigit
      val newIndex = index + fileBlocksSize + freeBlocksSize

      val newInterval = Interval(index until (index + fileBlocksSize))
      newIndex -> (newInterval :: intervals)
  ._2.reverse


object Compact:
  def compact(
    compacted: Map[Int, Interval[Int]],
    freeSpaces: List[Interval[Int]],
    toMove: List[(Int, Interval[Int])]): Map[Int, Interval[Int]] =
    if toMove.isEmpty then compacted
    else
      val (id, interval) = toMove.head
      val spaceIndex = freeSpaces
        .takeWhile(_.min < interval.min)
        .indexWhere(_.size >= interval.size)

      if spaceIndex == -1 then
        compact(compacted.updated(id, interval), freeSpaces, toMove.tail)
      else
        val space = freeSpaces(spaceIndex)
        val newFreeSpace = space.copy(min = space.min + interval.size)

        // TODO: implement interval.shift
        val shift = interval.min - space.min
        val newPosition = interval.copy(interval.min - shift, interval.max - shift)

        compact(
          compacted = compacted.updated(id, newPosition),
          freeSpaces = freeSpaces.updated(spaceIndex, newFreeSpace),
          toMove = toMove.tail
        )

val toMove: List[(Int, Interval[Int])] =
  occupiedIntervals.zipWithIndex.map(_.swap).reverse

val compacted = Compact.compact(Map.empty, freeIntervals, toMove)

val ans2 = compacted.toList.sortBy(_._2.min).map[Long]:
  case (id, interval) =>
    interval.toRange.foreach: index =>
      println(s"$index * $id")
    interval.toRange.map(_.toLong * id.toLong).sum.toLong
.sum
