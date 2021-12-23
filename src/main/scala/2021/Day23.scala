package `2021`.`23`

import io.Source
import util.{Using, Try}
import util.chaining.*

enum Amph(val energy: Int, val home: Pos.Entry):
  case A extends Amph(1, Pos.A)
  case B extends Amph(10, Pos.B)
  case C extends Amph(100, Pos.C)
  case D extends Amph(1000, Pos.D)

case class Room(amphType: Amph, occupants: List[Amph], capacity: Int):
  assert(occupants.size <= capacity)
  def full = occupants.size == capacity
  def distanceToBack = capacity - occupants.size
  def hasStrangers = occupants.exists(_ != amphType)
  def organized = full && !hasStrangers

  def nextAmph =
    Option.when(hasStrangers)(occupants.headOption).flatten

  def admits(a: Amph) = !(full || hasStrangers) && a == amphType

  def admit(a: Amph): Option[Room] =
    Option.when(admits(a))(copy(occupants = a :: occupants))

  def release: Option[(Amph, Room)] =
    nextAmph.map(_ -> copy(occupants = occupants.tail))

sealed trait Pos:
  import Pos.{A,B,C,D}
  def isEntry = this match
    case A | B | C | D => true
    case _ => false
  def isSpace = !isEntry

object Pos:
  export Hallway.*

  given Conversion[Entry, Hallway] = _.pos
  given Conversion[Space, Hallway] = _.pos

  sealed trait Space extends Pos { self: Hallway =>
    def ord: Int = self.ordinal
    def pos: Hallway = self
  }
  sealed trait Entry extends Pos { self: Hallway =>
    def ord: Int = self.ordinal
    def pos: Hallway = self
  }

  object Space:
    val values = Array[Space](L2, L1, AB, BC, CD, R1, R2)

  object Entry:
    val values = Array[Entry](A,B,C,D)

  enum Hallway extends Pos:
    case L2 extends Hallway with Space
    case L1 extends Hallway with Space
    case A extends Hallway with Entry
    case AB extends Hallway with Space
    case B extends Hallway with Entry
    case BC extends Hallway with Space
    case C extends Hallway with Entry
    case CD extends Hallway with Space
    case D extends Hallway with Entry
    case R1 extends Hallway with Space
    case R2 extends Hallway with Space

    def distanceTo(pos: Hallway): Int =
      (this.ordinal - pos.ordinal).abs

  enum Move:
    case Out(roomType: Entry, to: Space)
    case In(from: Space)

  object Move:
    final val values = for
      p <- Space.values
      a <- Entry.values
    yield Out(a, p)

  // def entrance(room: Room): Entry =
import Pos.*

case class Burrow(hallway: Map[Space, Amph], rooms: Map[Entry, Room], cost: Int):
  def show: String =
    val hallString = Pos.Hallway.values.map(p => hallway.map(identity).toMap.get(p).fold(".")(_.toString)).mkString
    val occupants = rooms.toList.sortBy(_._1.ord).map(_._2)
      .map(r => r.occupants.map(_.toString).reverse.padTo(r.capacity, ".").reverse)
    val roomString = occupants.transpose.collect {
      case List(a, b, c, d) => s"###$a#$b#$c#$d###"
    }.mkString("\n")
    s"""|#############
        |#$hallString#
        |${roomString}
        |#############
        |""".stripMargin// + roomStrings.mkString("\n")

  def extraSteps(a: Amph, m: Pos.Move.Out): Int =
    val distOut = m.roomType.distanceTo(m.to)
    val distIn = m.to.distanceTo(a.home)
    val minDist = 2 max m.roomType.distanceTo(a.home)
    distOut + distIn - minDist

  def apply(move: Pos.Move): Option[Burrow] = this.move(move)

  def organized = rooms.values.forall(_.organized)

  def blocked(p1: Pos.Hallway, p2: Pos.Hallway): Boolean =
    val List(left, right) = List(p1, p2).sortBy(_.ordinal)
    val spacesBetween = (left.ordinal + 1 until right.ordinal)
    // println(s"checking range $spacesBetween")
    hallway.keys.exists(p => spacesBetween.contains(p.ord))

  def occupied(pos: Space): Boolean = hallway.contains(pos)

  def move(m: Pos.Move): Option[Burrow] = m match
    case out@Pos.Move.Out(roomStart, hallDest) if !occupied(hallDest) =>
      for
        (a, r) <- rooms(roomStart).release
        if !blocked(roomStart, hallDest)
      yield
        copy(
          hallway.updated(hallDest, a),
          rooms.updated(roomStart, r),
          cost + extraSteps(a, out) * a.energy)
    case Pos.Move.In(hallStart) =>
      for
        a <- hallway.get(hallStart)
        if !blocked(hallStart, a.home)
        r <- rooms(a.home).admit(a)
      yield
        copy(hallway.removed(hallStart), rooms.updated(a.home, r))
    case _ => None

object Burrow:
  given Ordering[Burrow] = Ordering[Int].reverse.on(_.cost)
  def fromSource(source: Source) =
    val lines = source.getLines.drop(2).map(_.slice(2,11)).toList
    val occupants = lines.flatMap {
      case s"#$a#$b#$c#$d#" =>
        Try(List(a, b, c, d).map(Amph.valueOf)).toOption
    }.transpose
    val capacity = occupants.head.size
    val rooms = Map[Entry, Room](
      Pos.A -> Room(amphType = Amph.A, occupants(0), capacity),
      Pos.B -> Room(amphType = Amph.B, occupants(1), capacity),
      Pos.C -> Room(amphType = Amph.C, occupants(2), capacity),
      Pos.D -> Room(amphType = Amph.D, occupants(3), capacity))
    val minimumCost =
      val upAndOver = rooms.flatMap {
        case (entry, room) =>
          val alphs = Iterator.unfold(room)(_.release)
          alphs.zip(Iterator.from(1)).map { case (a, up) =>
            val over = entry.distanceTo(a.home).max(2)
            a.energy * (up + over)
          }
      }
      val travelling = rooms.values.flatMap(Iterator.unfold(_)(_.release))
        .groupMapReduce(identity)(_ => 1)(_ + _)
      val down = travelling.map { case (a, c) => a.energy * (1 to c).sum }
      upAndOver.sum + down.sum
    Burrow(hallway = Map.empty, rooms, minimumCost)

@main def day23(): Unit =
  import collection.mutable.PriorityQueue
  import util.chaining.*
  import Pos.*

  val initial = Using(Source.fromResource("2021/day-23-2.txt"))(Burrow.fromSource).get

  def searchFrom(burrow: Burrow): List[Burrow] =
    val ins = burrow.hallway.keys.map(Move.In.apply)
    val outs = for
      entry <- Entry.values
      space <- Space.values.filterNot(burrow.occupied)
    yield Move.Out(entry, space)
    (ins ++ outs).flatMap(burrow.move).toList

  var visiting = initial
  val toVisit = PriorityQueue(initial)
  while !visiting.organized do
    searchFrom(visiting).foreach(toVisit.enqueue(_))
    visiting = toVisit.dequeue()

  println(visiting.cost)
  println(visiting.show)
