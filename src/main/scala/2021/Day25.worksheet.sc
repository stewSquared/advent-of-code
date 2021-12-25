import util.Using
import io.Source
import util.chaining.*

val input = Using(Source.fromResource("2021/day-25-1.txt"))(_.getLines.toList).get

case class Point(x: Int, y: Int)

case class Floor(width: Int, height: Int, south: Set[Point], east: Set[Point]):
  extension(p: Point)
    def s = p.copy(y = (p.y + 1) % height)
    def e = p.copy(x = (p.x + 1) % width)
    def clear: Boolean = !(south(p) || east(p))

  def moveSouth: (Set[Point], Floor) =
    val (moving, stayed) = south.partition(_.s.clear)
    val moved = moving.map(_.s)
    moved -> copy(south = stayed union moved)

  def moveEast: (Set[Point], Floor) =
    val (moving, stayed) = east.partition(_.e.clear)
    val moved = moving.map(_.e)
    moved -> copy(east = stayed union moved)

  def step: (Set[Point], Floor) =
    this.moveEast.pipe {
      case (movedEast, state) =>
        state.moveSouth.pipe {
          case (movedSouth, state) =>
            (movedEast union movedSouth) -> state
        }
    }

  def draw: String =
    val sb = collection.mutable.StringBuilder()
    for y <- 0 until width do
      for x <- 0 until width do
        val p = Point(x,y)
        sb += (if south(p) then 'v' else if east(p) then '>' else '.')
      sb += '\n'
    sb.result()

val floor =
  val eb = collection.mutable.ListBuffer.empty[Point]
  val sb = collection.mutable.ListBuffer.empty[Point]
  for (row, y) <- input.zipWithIndex do
    for (o, x) <- row.zipWithIndex do
      o match
        case '>' => eb += Point(x,y)
        case 'v' => sb += Point(x,y)
        case _ => ()
  Floor(width = input(0).size, height = input.size, south = sb.result().toSet, east = eb.result().toSet)

def steps: Iterator[Set[Point]] = Iterator.unfold(floor)(f => Some(f.step))

val ans1 = steps.indexWhere(_.isEmpty) + 1
