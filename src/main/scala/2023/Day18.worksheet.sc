import aoc.*

val input = io.Source.fromResource("2023/day-18-ex.txt").getLines.toList

def toCardinal(dir: String): Dir = dir match
  case "U" => Dir.N
  case "D" => Dir.S
  case "R" => Dir.E
  case "L" => Dir.W


def step(pos: Point, plan: List[Line], instruction: String): (Point, List[Line]) =
  instruction match
    case s"$rawDir $n (#$rawHex)" =>
      // val distance = Integer.parseInt(rawHex.take(5), 16)
      // val dir = rawHex(5) match
      //   case '0' => Dir.E
      //   case '1' => Dir.S
      //   case '2' => Dir.W
      //   case '3' => Dir.N
      val distance = n.toInt
      val dir = toCardinal(rawDir)
      val newPos = pos.move(dir, distance)
      newPos -> (Line(pos, newPos) :: plan)

val (_, plan) = input.foldLeft(Point.origin -> List.empty[Line]):
  case ((pos, plan), instruction) => step(pos, plan, instruction)

val boundaryPoints = plan.flatMap(_.points).toSet
boundaryPoints(Point.origin)

val vert = plan.toList.filter(_.vertical).sortBy(_.p.x).map:
  case Line(p, q) if p.y > q.y => Line(q, p)
  case line => line

val horiz = plan.toSet.filter(_.horizontal).map:
  case Line(p, q) if p.x > q.x => Line(q, p)
  case line => line

val cornerPoints = vert.toSet.flatMap(l => Set(l.p, l.q))
val boundingArea = Area.bounding(cornerPoints)

boundingArea

def parse(instruction: String): (Dir, Int) =
  instruction match
    case s"$rawDir $n (#$rawHex)" =>
      // val distance = Integer.parseInt(rawHex.take(5), 16)
      // val dir = rawHex(5) match
      //   case '0' => Dir.E
      //   case '1' => Dir.S
      //   case '2' => Dir.W
      //   case '3' => Dir.N
      val distance = n.toInt
      val dir = toCardinal(rawDir)
      dir -> distance


input.map(parse).foldLeft((Point.origin, 0)):
  case ((pos, area), (newDir, distance)) =>
    newDir match
      case Dir.E | Dir.W => pos.move(newDir, distance) -> area
      case Dir.N =>
        val newPos = pos.move(newDir, distance)
        val dy = line.q.y - line.p.y
        val area = Area(line.p.x to boundingArea.right, line.p.y to line.q.y by dy.sign)

        val dug2 = dug + area.size
        println(s"digging $area, dug = $dug2")
        line.q -> dug2




// val line = Line(Point(0, 0), Point(0, 10))
// val line = Line(Point(0, 10), Point(0, 0))
// val dy = line.q.y - line.p.y
// Area(line.p.x to boundingArea.right, line.p.y to line.q.y by dy.sign)
// Area(line.p.x to boundingArea.right, line.p.y to line.q.y by dy.sign).size

import util.chaining.*

vert.size

val (added, subtracted) = vert.foldLeft(Set.empty[Area] -> Set.empty[Area]):
  case ((added, subtracted), line) =>
    println(s"> line $line")

    added.foreach: a =>
      val t = util.Try(a.topLeft)
      assert(t.isSuccess, t)

    subtracted.foreach: a =>
      val t = util.Try(a.topLeft)
      assert(t.isSuccess, t)

    // added.foreach: a =>
    //   assert(a.yRange.size > 0, a)
    // subtracted.foreach: a =>
    //   assert(a.yRange.size > 0, a)

    val dy = line.q.y - line.p.y

    val ac = added.count: a =>
      // if (a.contains(line.p) || a.contains(line.q)) {
      //   println(s"added $a potentially overlaps with $line")
      // }
      /*(a.contains(line.p) || a.contains(line.q)) &&*/ !(horiz(Line(a.botLeft, line.p)) || horiz(Line(a.topLeft, line.q)))

    val sc = subtracted.count: s =>
      // if (s.contains(line.p) || s.contains(line.q)) {
      //   println(s"subtracted $s potentially overlaps with $line")
      // }
      /*(s.contains(line.p) || s.contains(line.q)) &**/ !(horiz(Line(s.botLeft.w, line.p)) || horiz(Line(s.topLeft.w, line.q)))

    // val adding =
    //   val areas = added union subtracted

      // val addTopConnected = added.find(a => horiz(a.topLeft, ) )

    import util.Try
    println(s"ac = $ac, sc = $sc")
    if (ac + sc) % 2 == 0 then //adding
      val newArea = Area(line.p.x to boundingArea.right + 10, line.p.y to line.q.y by dy.sign).tap(a => assert(Try(a.topLeft).isSuccess, a))
      println(s"adding $newArea")

      val addedTwice = added.collect:
        case o if o.top == newArea.bot && horiz(Line(o.topLeft, newArea.botLeft)) =>
          Area(newArea.xRange, o.top to o.top)
        case o if o.bot == newArea.top && horiz(Line(o.botLeft, newArea.topLeft)) =>
          Area(newArea.xRange, o.bot to o.bot).tap(a => assert(Try(a.topLeft).isSuccess, a)).tap(a => assert(a.yRange.size > 0, a))

      val addBack = subtracted.collect:
        case o if o.top == newArea.top && horiz(Line(o.topLeft, newArea.topLeft)) =>
          Area(o.xRange, o.top to o.top).tap(a => assert(Try(a.topLeft).isSuccess, a)).tap(a => assert(a.yRange.size > 0, a))
        case o if o.bot == newArea.bot && horiz(Line(o.botLeft, newArea.botLeft)) =>
          Area(o.xRange, o.bot to o.bot).tap(a => assert(Try(a.topLeft).isSuccess, a)).tap(a => assert(a.yRange.size > 0, a))

      ((added union addBack.toSet) + newArea) -> (subtracted union addedTwice.toSet)
    else // subtracting
      val newArea = Area(line.p.e.x to boundingArea.right + 10, line.p.y to line.q.y by dy.sign).tap(a => assert(Try(a.topLeft).isSuccess, a))
      println(s"subtracting $newArea")
      assert(newArea.yRange.size > 0, newArea)

      val subtractedTwice = subtracted.collect:
        case o if o.top == newArea.bot && horiz(Line(o.topLeft, newArea.botLeft)) =>
          Area(newArea.xRange, o.top to o.top)
        case o if o.bot == newArea.top && horiz(Line(o.botLeft, newArea.topLeft)) =>
          Area(newArea.xRange, o.bot to o.bot)

      // val subtractBack = added.collect:
      //   case o if o.top == newArea.bot && horiz(Line(o.topLeft, newArea.botLeft)) =>
      //     Area(o.xRange, o.top to o.top)
      //   case o if o.bot == newArea.top && horiz(Line(o.botLeft, newArea.topLeft)) =>
      //     Area(o.xRange, o.bot to o.bot)

      (added union subtractedTwice.toSet) -> ((subtracted/* union subtractBack.toSet*/) + newArea)

added.toList.sortBy(_.left) foreach println

subtracted.toList.sortBy(_.left) foreach println

// added.map(_.size[Long]).sum
// added.map(_.size[Long]).forall(_ > 0)
// subtracted.map(_.size[Long]).sum
// subtracted.map(_.size[Long]).forall(_ > 0)

val ans1 = added.iterator.map(_.size[Long]).sum - subtracted.iterator.map(_.size[Long]).sum


plan.size



// vert.foldLeft(Set.)



// def sliceDugRanges(nums: Set[Int]): List[Range] =
//   val sorted = nums.toList.sorted
//   val pairs = sorted.zip(sorted.tail)
//   val edges = LazyList.unfold(pairs): pairs =>
//     Option.when(pairs.nonEmpty):
//       val (range, rest) = pairs.span((a, b) => b == a + 1)
//       if range.nonEmpty then
//         (range.head._1 to range.last._2) -> (if rest.nonEmpty then rest.tail else Nil)
//       else
//         (rest.head._1 to rest.head._1) -> rest.tail

//   edges.toList.grouped(2).toList.map:
//     case List(a, b) => (a.min to b.max)
//     case List(a) => (a.min to sorted.last)
//     case Nil => ???

// def sliceArea(nums: Set[Int]): Long = sliceDugRanges(nums).map(_.size.toLong).sum

// sliceArea(Set(2,3,4,8,9,10))
// sliceArea(Set(2,10))
// sliceArea(Set(2,8,9,10))
// sliceArea(Set(2,3,4,10))
// sliceArea(Set(2,4,8,10))
// sliceArea(Set(2,4,8,10,12,14,18,20))
// sliceArea(Set(2,4,8,10,12,14,19,20))

// boundaryPoints.groupMap(_.y)(_.x).values.find(_.size % 2 == 1) foreach println

// boundaryPoints.groupMap(_.y)(_.x).collectFirst:
//   case (y, xs) if xs.size % 2 == 0 => y


// .view.map((y, xs) => sliceDugRanges(xs).flatMap(_.flatMap(x => Point(x, y))))
// val dugPoints = Set.from[Point]:
//   boundaryPoints.groupMap(_.y)(_.x).toList.flatMap:
//     case (y, xs) =>
//       sliceDugRanges(xs).flatMap(_.map(x => Point(x, y)))

// println:
//   area.draw: p =>
//     if plan.contains(p) then '#'
//     else if dugPoints.contains(p) then 'X'
//     else '.'

// val ans1 = boundaryPoints.groupMap(_.y)(_.x).view.mapValues(sliceArea).values.sum

// for area.yRange



// val insidePoint = Point(1, 1)
// val flood = Iterator.unfold(Set.empty[Point], Set(insidePoint)):
//   case (visited, current) =>
//     Option.when(current.nonEmpty):
//       val next = current.flatMap(area.adjacent).diff(boundaryPoints).diff(visited)
//       (current, current -> next)

// val area = Area.bounding(plan)
// val inner = flood.reduce(_ union _)
// val ans1 = inner.size + boundaryPoints.size

// inner.size
// boundaryPoints.size

// area.size
// area.width
// area.height

// println:
//   area.draw: p =>
//     if inner.contains(p) then 'X'
//     else if plan.contains(p) then '#'
//     else '.'



//
