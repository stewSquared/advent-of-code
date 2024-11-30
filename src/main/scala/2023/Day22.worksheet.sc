import aoc.*

val input = io.Source.fromResource("2023/day-22.txt").getLines.toVector

case class Brick(area: Area, depth: Interval[Int])

val falling = List.from[Brick]:
  input.map:
    case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
      val xRange = Interval(x1.toInt, x2.toInt)
      val yRange = Interval(y1.toInt, y2.toInt)
      val zRange = Interval(z1.toInt, z2.toInt)
      val area = Area(xRange, yRange)
      Brick(area, zRange)

val settled = falling.sortBy(_.depth.min).foldLeft(List.empty[Brick]):
  case (settled, brick) =>
    val newBot = settled.filter(_.area.intersect(brick.area).nonEmpty)
      .map(_.depth.max + 1).maxOption.getOrElse(0)

    val newDepth = Interval(newBot until newBot + brick.depth.size)
    Brick(brick.area, newDepth) :: settled

val paired: List[(Brick, Brick)] =
  settled.combinations(2).toList.filter:
    case List(a, b) => a.area.intersect(b.area).nonEmpty
    case _ => ???
  .collect:
    case List(a, b) if a.depth.min - 1 == b.depth.max => a -> b
    case List(b, a) if a.depth.min - 1 == b.depth.max => a -> b

val bricksBelow: Map[Brick, List[Brick]] =
  paired.groupMap(_._1)(_._2).withDefaultValue(Nil)

val bricksAbove: Map[Brick, List[Brick]] =
  paired.map(_.swap).groupMap(_._1)(_._2).withDefaultValue(Nil)

val ans1 = settled.count: b =>
  bricksAbove(b).forall: a =>
    bricksBelow(a).sizeIs > 1

def chain(dropped: Set[Brick], dropping: Brick): Set[Brick] =
  val next = bricksAbove(dropping).filter: a =>
    bricksBelow(a).forall(b => dropped(b) || b == dropping)
  next.foldLeft(dropped + dropping)(chain)

def disintegrate(brick: Brick): Int =
  chain(Set.empty, brick).size - 1

val ans2 = settled.map(disintegrate).sum
