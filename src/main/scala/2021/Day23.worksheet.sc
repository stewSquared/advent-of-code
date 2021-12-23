/* enum Amph(energy: Int):
  case A extends Amph(1)
  case B extends Amph(10)
  case C extends Amph(100)
  case D extends Amph(1000)

case class Room(amphType: Amph, occupants: List[Amph], capacity: 2 = 2):
  assert(occupants.size <= capacity)
  def full = occupants.size == capacity
  def distanceToBack = capacity - occupants.size
  def hasStrangers = occupants.exists(_ != amphType)
  def organized = full && !hasStrangers

  def nextAmph =
    if hasStrangers then occupants.headOption
    else None

  def admits(a: Amph) = !(full || hasStrangers)

  def admit(a: Amph): Option[Room] =
    if admits(a) then Some(copy(occupants = a :: occupants)) else None

  def release: Option[(Amph, Room)] =
    nextAmph.map(_ -> copy(occupants = occupants.tail))

  // def entrance: Hallway

sealed trait Pos:
  def isEntry = this match
    case A | B | C | D => true
    case _ => false
  def isSpace = !isEntry

object Pos:
  export Hallway.*

  sealed trait Space extends Pos { self: Hallway =>
    def ord: Int = self.ordinal
  }
  sealed trait Entry extends Pos { self: Hallway =>
    def ord: Int = self.ordinal
  }

  object Space:
    val values = Array[Hallway & Space](L2, L1, AB, BC, CD, R1, R2)

  object Entry:
    val values = Array[Hallway & Entry](A,B,C,D)

  def homeEntry(amphType: Amph): Entry & Hallway = amphType match
    case Amph.A => A
    case Amph.B => B
    case Amph.C => C
    case Amph.D => D

  enum Hallway extends Pos:
    case L2 extends Hallway with Pos.Space
    case L1 extends Hallway with Pos.Space
    case A extends Hallway with Pos.Entry
    case AB extends Hallway with Pos.Space
    case B extends Hallway with Pos.Entry
    case BC extends Hallway with Pos.Space
    case C extends Hallway with Pos.Entry
    case CD extends Hallway with Pos.Space
    case D extends Hallway with Pos.Entry
    case R1 extends Hallway with Pos.Space
    case R2 extends Hallway with Pos.Space

    def distanceTo(pos: Hallway): Int =
      (this.ordinal - pos.ordinal).abs

  enum Move:
    case Out(roomType: Pos.Entry & Pos.Hallway, to: Pos.Space & Pos.Hallway)
    case In(from: Pos.Space & Pos.Hallway, roomType: Pos.Entry & Pos.Hallway)

  object Move:
    final val values = for
      p <- Space.values
      a <- Entry.values
    yield Out(a, p)

  // def entrance(room: Room): Entry =

case class Burrow(hallway: Map[Pos.Space, Amph], rooms: Map[Pos.Entry, Room]):
  def show: String =
    val hallString = Pos.Space.values.map(p => hallway.get(p).fold(".")(_.toString)).mkString
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

  def apply(move: Pos.Move): Option[Burrow] = this.move(move)

  def organized = rooms.values.forall(_.organized)

  def blocked(p1: Pos.Hallway, p2: Pos.Hallway): Boolean =
    val List(left, right) = List(p1, p2).sortBy(_.ordinal)
    val spacesBetween = (left.ordinal + 1 until right.ordinal)
    // println(s"checking range $spacesBetween")
    hallway.keys.exists(p => spacesBetween.contains(p.ord))

  def occupied(pos: Pos.Space): Boolean = hallway.contains(pos)

  def move(m: Pos.Move): Option[Burrow] = m match
    case Pos.Move.Out(roomType, to) if !occupied(to) =>
      for
        (a, r) <- rooms(roomType).release
        if !blocked(Pos.homeEntry(r.amphType), to)
      yield
        Burrow(hallway.updated(to, a), rooms.updated(roomType, r))
    case Pos.Move.In(from, roomType) =>
      for
        a <- hallway.get(from)
        r <- rooms(roomType).admit(a)
        if !blocked(from, Pos.homeEntry(r.amphType))
      yield
        Burrow(hallway.removed(from), rooms.updated(roomType, r))
    case _ => None


  // def preferredSpaces: List[Space] = this match
  //   case A => List(L1, L2, AB, BC, CD, R1, R2)
  //   case B => List(L1, L2, AB, BC, CD, R1, R2)
  //   case C => List(L1, L2, AB, BC, CD, R1, R2)
  //   case D => List(L1, L2, AB, BC, CD, R1, R2)




((Hallway.A: Entry).ord)


def show(state: Burrow): Unit = println(state.show)

case class SearchState(burrow: Burrow, cost: Int, toIn: List[Move.In])


// @annotation.tailrec
// final def solutions(burrow: Burrow, back: List[Burrow], cost: Int): LazyList[Burrow] =
//   val outs = burrow.rooms.collect {
//     case (e, r) if r.hasStrangers =>
//   }

//   ???

val initial = Using(Source.fromResource("2021/day-23-1.txt")){ source =>
  val lines = source.getLines
  lines.next() // wall
  lines.next() // hallway
  val outer = lines.next().flatMap(c => Try(Amph.valueOf(s"$c")).toOption)
  val inner = lines.next().flatMap(c => Try(Amph.valueOf(s"$c")).toOption)
  val occupants = (outer zip inner).map(_.toList)

  Burrow(hallway = Map.empty,
    rooms = Map(
      Hallway.A -> Room(amphType = Amph.A, occupants(0)),
      Hallway.B -> Room(amphType = Amph.B, occupants(1)),
      Hallway.C -> Room(amphType = Amph.C, occupants(2)),
      Hallway.D -> Room(amphType = Amph.D, occupants(3)),
    )
  )
}.get



var s = initial

def next(move: Move): Unit =
  s(move).foreach { b =>
    println(b.show)
    s = b
  }

next(Out(D,R1))
next(Out(D,R1))



// show(initial)

// // Move.every.flatMap(initial.move) foreach show
// // Move.every.flatMap(initial.move).size


// println(initial.show)


// // import Amph.{A,B,C,D}
// initial(Move.Out(A, L1)).map(_.show)
// initial(Move.Out(A, R1)).map(_.show)
// initial(Move.Out(D, R1)).map(_.show)

// initial.rooms(D)
// initial.rooms(D).nextAmph
// initial.rooms(D).hasStrangers

// initial.rooms(A).release foreach println
// initial.occupied(Hallway.L1)
// Hallway.entrance(initial.rooms(A))
// initial.blocked(Hallway.entrance(initial.rooms(A)), Hallway.L1)

// initial.blocked(Hallway.A, Hallway.L1)

// Hallway.L1.ordinal
// Hallway.A.ordinal

// initial.hallway.keys.exists(_ => false)
// // initial.hallway.values.exists()

// (2 until 2).contains(1)

// println(initial(Move.Out(B, Hallway.L1)))

// // initial.move(Move.ToHall())

// Move.every foreach println

// Move.every.flatMap(initial.move).foreach(s => println(s.show))
 */
