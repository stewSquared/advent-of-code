import aoc.*

val input = io.Source.fromResource("2023/day-15.txt").getLines().toList

def hash(string: String): Int =
  string.foldLeft(0):
    case (acc, char) =>
      ((acc + char.toInt) * 17) % 256

val strings = input(0).split(",").toList
val ans1 = strings.map(hash).sum

type Box = Vector[(String, Int)]
def insertLens(box: Box, label: String, focalLength: Int): Box =
  val existing = box.indexWhere(_._1 == label)
  if existing == -1 then
    box :+ (label, focalLength)
  else
    box.updated(existing, (label, focalLength))

def step(boxes: Map[Int, Box], string: String): Map[Int, Box] =
  string match
    case s"$label-$_" =>
      val i = hash(label)
      boxes.updated(i, boxes(i).filterNot(_._1 == label))
    case s"$label=$focalLength" =>
      val i = hash(label)
      boxes.updated(i, insertLens(boxes(i), label, focalLength.toInt))

val emptyBox = Map.empty[Int, Box].withDefaultValue(Vector.empty)
val finalBoxState = strings.foldLeft(emptyBox)(step)

val focusingPowers = finalBoxState.filter(_._2.nonEmpty).flatMap:
  case (boxNumber, box) =>
    box.zipWithIndex.map:
      case ((label, focalLength), slotNumber) =>
        (boxNumber + 1) * (slotNumber + 1) * focalLength

val ans2 = focusingPowers.sum
