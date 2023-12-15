val input = io.Source.fromResource("2023/day-15.txt").mkString.trim.split(",").toList

def hash(string: String): Int =
  string.foldLeft(0): (acc, char) =>
    ((acc + char.toInt) * 17) % 256

val ans1 = input.map(hash).sum

type Box = Vector[(String, Int)]
def insertLens(box: Box, label: String, focalLength: Int): Box =
  val slotNumber = box.indexWhere(_._1 == label)
  if slotNumber == -1 then box :+ (label, focalLength)
  else box.updated(slotNumber, (label, focalLength))

def step(boxes: Map[Int, Box], string: String) = string match
  case s"$label-" =>
    val i = hash(label)
    boxes + (i -> boxes(i).filterNot(_._1 == label))
  case s"$label=$focalLength" =>
    val i = hash(label)
    boxes + (i -> insertLens(boxes(i), label, focalLength.toInt))

val startBoxes = Map.empty[Int, Box].withDefaultValue(Vector.empty)
val finalBoxes = input.foldLeft(startBoxes)(step)

val focusingPowers = finalBoxes.flatMap:
  case (boxNumber, box) =>
    box.zipWithIndex.map:
      case ((label, focalLength), slotNumber) =>
        (boxNumber + 1) * (slotNumber + 1) * focalLength

val ans2 = focusingPowers.sum
