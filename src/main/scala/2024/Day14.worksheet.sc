val input = io.Source.fromResource("2024/day-14.txt").getLines.toList

// 101x103

import aoc.{Point, Area}

case class Robot(p: Point, v: Point):
  def move(a: Area) = Robot(wrap(p + v, a), v)

val robots = input.collect:
  case s"p=$px,$py v=$vx,$vy" =>
    Robot(Point(px.toInt, py.toInt), Point(vx.toInt, vy.toInt))

val area = Area(0 until 101, 0 until 103)

def wrap(p: Point, a: Area) =
  val x = (p.x + a.width) % a.width
  val y = (p.y + a.height) % a.height
  Point(x, y)

val finalPositions = robots.map: r =>
  // tood mult v by 100
  Iterator.iterate(r)(_.move(area)).drop(100).next
.map(_.p)

finalPositions.size

val quadrants =
  val midX = area.width / 2
  val midY = area.height / 2
  List(
    Area(0 until midX, 0 until midY),
    Area((midX + 1) until 101, 0 until midY),
    Area(0 until midX, (midY + 1) until 103),
    Area((midX + 1) until 101, (midY + 1) until 103))

// val ans1 = quadrants
//   .map(q => finalPositions.count(q.contains))
//   .map(_.toLong)
//   .product

extension (p: Point)
  infix def *(n: Int) = Point(p.x * n, p.y * n)

def states: Iterator[List[Robot]] =
  Iterator.iterate(robots)(_.map(_.move(area)))

def next(state: List[Robot]): List[Robot] =
  state.map(r => r.move(area))

val topHalf = quadrants.take(2)
val botHalf = quadrants.drop(2)

import aoc.Interval

// states.zipWithIndex.filter: (s, _) =>
//   val ps = s.map(_.p)
//   val top = topHalf.map(q => ps.count(q.contains)).sum
//   val bot = botHalf.map(q => ps.count(q.contains)).sum
//   val off = 25
//   val ratioFits = Interval(bot - off to bot + off).contains(top * 2)
//   if (ratioFits) {
//     println(s"top: $top, bot: $bot")
//   }
//   ratioFits
// .take(2)
// .foreach: (s, i) =>
//   val ps = s.map(_.p).toSet
//   println(s"State $i")
//   println:
//     area.draw: p =>
//       if ps(p) then '#' else '.'
//   println()

// val ys = s.map(_.p.y)
// val topQuarter = Interval(0 to (25 - 5))
// val botQuarter = Interval((78 + 5) to 103)
// val top = ys.count(topQuarter.contains)
// val bot = ys.count(botQuarter.contains)
// top + bot < 20

// s.map(_.p).count(_.y == 39) >= (area.width - 50)

val lineState = states
  .drop(65)
  .next

val start = lineState.map(r => r.copy(v = r.v * 103))

val step103 = Iterator
  .iterate(start)(next)
  .zipWithIndex

  // .take(101)
  // .foreach: (s, i) =>
  //   val ps = s.map(_.p).toSet
  //   println(s"State $i")
  //   println:
  //     area.draw: p =>
  //       if ps(p) then '#' else '.'
  //   println()

// state 7687
val firstNicePic = step103.drop(74).next._1

  // println:
  //   val ps = firstNicePic.map(_.p).toSet
  //   area.draw: p =>
  //     if ps(p) then '#' else '.'


val start2 = firstNicePic.map(r => r.copy(v = r.v * 101))

def moreNiceStates = Iterator
  .iterate(start2)(next)
  .zipWithIndex

val targetArea =
  val left = 32
  val top = 38
  val w = 31
  val h = 33
  val a = Area(
    top = top,
    bot = top + h + - 1,
    left = left,
    right = left + w + - 1
  )
  assert(a.width == w)
  assert(a.height == h)
  a

// starting from 7687, iterating by 10403
// val endState = moreNiceStates.find: (s, i) =>
  // val ps = s.map(_.p).toSet
  // targetArea.topBorder.points
  // val c = s.iterator
  //   .map(_.p)
  //   .count(targetArea.contains)
  // c == robots.size

// val ans2 = endState.get._2 * 10403 + 7687
// showImage(endState.get._1)

def showImage(state: List[Robot]) =
  val ps = state.map(_.p).toSet
  println:
    area.draw: p =>
      if ps(p) then '#' else '.'


  // .foreach: (s, i) =>
  //   val ps = s.map(_.p).toSet
  //   println(s"State $i")
  //   println:
  //     area.draw: p =>
  //       if ps(p) then '#' else '.'
  //   println()

2 + 2

// moreNiceStates.find: (s, i) =>
//   val top = s.map(_.p).filter(_.y == 38).sizeIs >= 15



val vs = lineState
  .filter(_.p.y == 38)
  .map(_.v.y)

import aoc.math.lcmN
import aoc.math.lcm

val ps = vs.map(v => lcm(area.height, v.abs))
val period = lcmN(ps.head, ps.tail*)

// val ans2 = period + 65
// 866554801 too high

// val period = lcmN(vs.head, vs.tail*)


states.take(0).zipWithIndex.filter: (s, _) =>
  s.map(_.p).filter(_.y == 38).sizeIs >= 15
.take(2)
.foreach: (s, i) =>
  val ps = s.map(_.p).toSet
  println(s"State $i")
  println:
    area.draw: p =>
      if ps(p) then '#' else '.'
  println()

robots.size
area.size


// states.take()

// val bps = Set(Point(0, 0))

// targetArea.topBorder.points.forall(bps)

val Some(endState, endIndex) = states.zipWithIndex
  .take(area.height * area.width)
  .find: (s, i) =>
    val ps = s.map(_.p).toSet
    targetArea.topBorder.points.forall(ps) &&
    targetArea.botBorder.points.forall(ps) &&
    targetArea.leftBorder.points.forall(ps) &&
    targetArea.rightBorder.points.forall(ps)

println(targetArea.topBorder)

showImage(endState)


// val ans2 = endState.get._2 * 10403 + 7687
// showImage(endState.get._1)

// states.take(100).zipWithIndex.foreach: (s, i) =>
//   val ps = s.map(_.p).toSet
//   println(s"State $i")
//   println:
//     area.draw: p =>
//       if ps(p) then '#' else '.'
//   println()




//
