val input = io.Source.fromResource("2025/day-06.txt").getLines().toList

val numbers: Vector[Vector[Long]] = input.toVector.init.collect:
  line => line.split(' ').toVector.flatMap(_.toLongOption)

val ops = input.last.split(' ').toVector.filter(_.nonEmpty).map(_.head)

val ans1 = ops.zip(numbers.transpose).map:
  case ('*', nums) => nums.product
  case ('+', nums) => nums.sum
.sum

val maxLength = input.map(_.length).max
input.map(_.length).distinct

val nums2 = input.init.transpose.map: col =>
  col.filterNot(_.isWhitespace).mkString.toLongOption

val opLine = input.last

val initOp = opLine.head

val ans2 = opLine.zip(nums2).foldLeft(opLine.head -> List.empty[Long]):
  case ((op, acc), (' ', None)) => op -> acc
  case ((op, acc), ('*', Some(n))) => '*' -> (n::acc)
  case ((op, acc), ('+', Some(n))) => '+' -> (n::acc)
  case ((op, acc::rest), (' ', Some(n))) => op match
    case '*' => op -> ((acc*n) :: rest)
    case '+' => op -> ((acc+n) :: rest)
._2.sum

// 13837321028548
// 3393053451590101

//
