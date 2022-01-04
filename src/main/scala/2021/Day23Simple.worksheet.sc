import io.Source
import util.Using
import collection.mutable.PriorityQueue

enum Amph(val energy: Int, val homeEntry: Tile):
  case A extends Amph(1, Tile.A)
  case B extends Amph(10, Tile.B)
  case C extends Amph(100, Tile.C)
  case D extends Amph(1000, Tile.D)

enum Tile:
  case LO, LI, A, AB, B, BC, C, CD, D, RI, RO

  def distanceTo(that: Tile) =
    (that.ordinal - this.ordinal).abs

object Tile:
  final val entries = Array(A, B, C, D)
  final val spaces = values.diff(entries)

type Room = (Tile, List[Amph])

case class Burrow(
    hallway: Map[Tile, Amph],
    rooms: Map[Tile, List[Amph]],
    cost: Int
):
  def organized = hallway.isEmpty && !Tile.entries.exists(hasStrangers)

  def hasStrangers(entry: Tile): Boolean =
    rooms(entry).exists(_.homeEntry != entry)

  def openPath(from: Tile, to: Tile): Boolean =
    val range =
      to.ordinal until from.ordinal by (from.ordinal - to.ordinal).sign
    !hallway.keys.map(_.ordinal).exists(range.contains)

  def moveAllIn: Burrow =
    val oneIn = hallway.collectFirst {
      case (startTile, a)
          if openPath(startTile, a.homeEntry) && !hasStrangers(a.homeEntry) =>
        copy(
          hallway.removed(startTile),
          rooms.updated(a.homeEntry, a :: rooms(a.homeEntry))
        )
    }
    oneIn.fold(this)(_.moveAllIn)

  def nextStates =
    for
      dest <- Tile.spaces
      (startEntry, a :: remaining) <- rooms.filter(r => hasStrangers(r._1))
      if openPath(startEntry, dest)
    // TODO if path home from dest doesn't intersect another amph's path home
    yield
      val extraCost =
        val distOut = startEntry.distanceTo(dest)
        val distIn = dest.distanceTo(a.homeEntry)
        val minCost = startEntry.distanceTo(a.homeEntry).max(2)
        (distOut + distIn - minCost) * a.energy

      val movedOut = copy(
        hallway.updated(dest, a),
        rooms.updated(startEntry, remaining),
        cost = cost + extraCost
      )
      if remaining.isEmpty then movedOut.moveAllIn else movedOut

object Burrow:
  given Ordering[Burrow] = Ordering[Int].reverse.on(_.cost)
  def fromSource(source: Source) =
    val lines = source.getLines.toList.drop(2).map(_.slice(2, 11)).toList
    val occupants = lines.flatMap { case s"#$a#$b#$c#$d#" =>
      util.Try(List(a, b, c, d).map(Amph.valueOf)).toOption
    }.transpose
    val rooms = Tile.entries.zip(occupants).toMap
    val minimumCost =
      val upAndOver = rooms.flatMap { case (entry, occupants) =>
        val alphs = Iterator.unfold(occupants) { os =>
          Option.when(os.exists(_.homeEntry != entry))(os.head -> os.tail)
        }
        alphs.zip(Iterator.from(1).take(4)).map { case (a, up) =>
          val over = entry.distanceTo(a.homeEntry).max(2)
          a.energy * (up + over)
        }
      }
      val travelling = rooms
        .flatMap { case (entry, occupants) =>
          Iterator.unfold(occupants) { os =>
            Option.when(os.exists(_.homeEntry != entry))(os.head -> os.tail)
          }
        }
        .groupMapReduce(identity)(_ => 1)(_ + _)
      val down = travelling.map { case (a, c) => a.energy * (1 to c).sum }
      upAndOver.sum + down.sum
    Burrow(hallway = Map.empty, rooms, minimumCost)

val initial =
  Using(Source.fromResource("2021/day-23-1.txt"))(Burrow.fromSource).get

def show(burrow: Burrow): String =
  val hallString =
    Tile.values.map(t => burrow.hallway.get(t).fold(".")(_.toString)).mkString
  val roomStrings = Tile.entries.map(burrow.rooms(_).mkString).mkString("|")
  s"#$hallString# $roomStrings ${burrow.cost}"
// s"$hallString || $roomStrings"

val toVisit = PriorityQueue(initial)
var visiting = toVisit.dequeue()
while !visiting.organized do
  // println(s"${show(visiting)} -> ")
  // visiting.nextStates.sortBy(_.cost).take(4).foreach(b => println(show(b)))
  // println()
  // visiting.nextStates.tapEach(b => println(s"${show(visiting)} -> ${show(b)}")).foreach(toVisit.enqueue(_))
  visiting.nextStates.foreach(toVisit.enqueue(_))
  visiting = toVisit.dequeue()

// 1406402 search steps in 53 seconds
show(visiting)
val ans1 = visiting.cost
