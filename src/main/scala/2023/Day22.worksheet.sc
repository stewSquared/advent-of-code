import aoc.*

val input = io.Source.fromResource("2023/day-22.txt").getLines.toVector

val coordinates = input.map:
  case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
    val xRange = x1.toInt.min(x2.toInt) to x1.toInt.max(x2.toInt)
    val yRange = y1.toInt.min(y2.toInt) to y1.toInt.max(y2.toInt)
    val zRange = z1.toInt.min(z2.toInt) to z1.toInt.max(z2.toInt)
    assert(xRange.nonEmpty && yRange.nonEmpty && zRange.nonEmpty)
    val area = Area(x1.toInt to x2.toInt, y1.toInt to y2.toInt)
    val depth = Interval(zRange)
    area -> depth
// .sortBy:
//   case (area, depth) => depth.min
.zipWithIndex.map(_.swap).toMap

// coordinates.toList.sortBy(_._1) foreach println

type Brick = (Area, Interval[Int])

// def fall(settled: List[Brick], brick: Brick): List[Brick] =
//   val (a1, d1) = brick
//   val newBot = settled.find:
//     case (a2, d2) => a1.intersect(a2).nonEmpty
//   .map(_._2.max + 1).getOrElse(0)
//   val newDepth = Interval(newBot until newBot + d1.size)
//   (a1, newDepth) :: settled

def fallMap(settled: List[(Brick, List[Brick])], brick: Brick): List[(Brick, List[Brick])] =
  val (a1, d1) = brick
  val newBot = settled.map(_._1).filter:
    case (a2, d2) => a1.intersect(a2).nonEmpty
  .map(_._2.max + 1).maxOption.getOrElse(0)

  val below = settled.collect:
    case (b@(a2, d2), _) if d2.max + 1 == newBot && a1.intersect(a2).nonEmpty => b

  val newDepth = Interval(newBot until newBot + d1.size)

  ((a1, newDepth) -> below.toList) :: settled

val (_, bricks) = coordinates.unzip

2 + 2
// bricks foreach println

val below: Map[Brick, List[Brick]] =
  bricks.toList.sortBy(_._2.min).foldLeft(List.empty[(Brick, List[Brick])])(fallMap).toMap

// val name = below.keys.toList.sortBy(_._2.min).zip("ABCDEFG").toMap
val name = below.keys.toList.sortBy(_._2.min).zipWithIndex.toMap

val above: Map[Brick, List[Brick]] =
  below.toList.flatMap:
    case (above, bricks) =>
      bricks.map(_ -> above)
  .groupMap(_._1)(_._2)
  .withDefaultValue(Nil)

// above.view.mapValues(_.size).toList foreach println

val leafs = below.keySet.diff(above.keySet)

val redundant = above.toList.collect:
  case (brick, above) if above.forall(below(_).size > 1) => brick
.toSet

redundant.size

// val redundant2 = below.keys.toList.sortBy(_._2.min).filter:
//   case (brick, bricksBelow) => bricksBelow.size > 1
// .flatMap(_._2).distinct.filter: brick =>

// below.keys.toList.sortBy(_._2.min).foreach(b => println(s"${name(b)} $b"))

// below.keys.toList.sortBy(_._2.min).foreach: b =>
//   println(s"${name(b)} ${b._2} is above ${below(b).map(name)}")

// below.keys.toList.sortBy(_._2.min).foreach: b =>
//   println(s"${name(b)} ${b._2} is below ${above.get(b).getOrElse(Nil).map(name)}")

// below.keySet.intersect(leafs).toList.sortBy(_._2.min).foreach: b =>
//   println(s"${name(b)} ${b._2} is above ${below(b).map(name)}")

// below.keySet.intersect(leafs).toList.sortBy(_._2.min).foreach: b =>
//   println(s"${name(b)} ${b._2} is below ${above.get(b).getOrElse(Nil).map(name)}")


below.size
input.size

above.size

// leafs.map(name) foreach println
// redundant.map(name) foreach println

leafs.size
redundant.size

val ans1 = leafs.union(redundant).size

val candidates = below.keySet.diff(leafs).diff(redundant)

candidates.size



def drops(b: Brick): Int =
  def search(dropped: Set[Brick], dropping: Set[Brick]): Set[Brick] =
    if dropping.isEmpty then dropped else
      // val (rs, bs) = dropping.partition(redundant)
      // val newDropping =
      //   bs.flatMap(above).union:
      //     val ras: Set[Brick] = rs.flatMap(above(_).toSet)
      //     ras.filter: (ra: Brick) =>
      //       below(ra).forall(bra => dropped(bra) || dropping(bra))
      val ras: Set[Brick] = dropping.flatMap(above(_).toSet)
      val newDropping = ras.filter: (ra: Brick) =>
        below(ra).forall(bra => dropped(bra) || dropping(bra))

      search(dropped.union(dropping), newDropping)
  search(Set.empty, Set(b)).size - 1

// drops(candidates.toList.head)
val ans2 = candidates.iterator.map(drops).sum

candidates.iterator.map(drops) foreach println

// 78164
// 73059 is too high
// 1095 is too low
// 1232 is too low

val bs = below.keys.toList.sortBy(_._2.min)
bs.map(drops).sum

bs.size

drops(bs(0))
assert(bs.indices.forall(i => i == name(bs(i))))

candidates.filter: c =>
  below(c).forall(!redundant(_))
.filter: c =>
  below(c).size > 1
.map(name)

bs(411)
below(bs(411))
below(bs(411)).map(name)

below(bs(411)).map(redundant)


candidates.foreach: c =>
  println(s"${name(c)} drops ${drops(c)}")

// bs.indices.map(i => drops(bs(i))).foreach(b => println(s"$i ${name(bs(i))}"))



// 448 too high, right for someone else

// sortedBricks.combinations(2).foreach:
//   case List((i1, (a1, d1)), (i2, (a2, d2))) =>


//     if a1.intersect(a2).nonEmpty then
//       println(s"$i1 $i2")





// bricks.count:
//   case (a1, d1) =>
//     bricks.exists:
//       case (a2, d2) =>

// val byMin: Map[Int, List[Int]] =
//   coordinates.toList.map:
//     case (i, (area, depth)) => depth.min -> i
//   .groupMap(_._1)(_._2)

// // .zipWithIndex.map(_.swap).toMap

// val adj = coordinates.map:
//   case (index, (area, depth)) =>
//     val overlapping = byMin.getOrElse(depth.max + 1, Nil).filter: i =>
//       coordinates(i)._1.intersect(area).nonEmpty
//     index -> overlapping

// adj foreach println


// val ans1 = adj.values.flatten.toSet.diff(adj.keySet).size
