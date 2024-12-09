val input = io.Source.fromResource("2024/day-09.txt").getLines.mkString
// val input = "2333133121414131403"

import aoc.Interval

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

val idsFromEnd = occupiedIntervals
  .zipWithIndex
  .reverse
  .flatMap((interval, id) => interval.toRange.map(_ => id))

val idChunkStarts = occupiedIntervals
  .zipWithIndex
  .map((interval, id) => interval.min -> id)
  .toMap

val sizeOccupied = occupiedIntervals.map(_.size).sum

object TailRec:
  def checkSum(from: Int, fillFrom: List[Int], sum: Long): Long =
    if from == sizeOccupied then sum else
      if idChunkStarts.contains(from) then
        checkSum(from + 1, fillFrom, sum + from * idChunkStarts(from))
      else
        checkSum(from + 1, fillFrom.tail, sum + from * fillFrom.head)

val ans1 = TailRec.checkSum(0, idsFromEnd, 0)

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
