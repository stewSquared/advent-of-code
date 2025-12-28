import synacor.numbers.*
import aoc.*

val grid = Vector(
  "x,8,-,1",
  "4,x,11,x",
  "+,4,-,18",
  "22,-,9,x"
).map(_.split(',').toVector)

val area = Area(grid)

enum Op:
  case Add, Sub, Mul
  case Const(n: Int)

case class State(weight: Int, pos: Point, op: Op):
  def n = go(pos.n)
  def s = go(pos.s)
  def e = go(pos.e)
  def w = go(pos.w)

  private def go(p: Point): State =
    op match
      case Op.Const(_) => grid(p) match
        case "x" => this.copy(pos = p, op = Op.Mul)
        case "+" => this.copy(pos = p, op = Op.Add)
        case "-" => this.copy(pos = p, op = Op.Sub)
      case _ =>
        val n = grid(p).toInt
        val newWeight = op match
          case Op.Mul => weight * n
          case Op.Add => weight + n
          case Op.Sub => weight - n
          case _ => ???
        this.copy(pos = p, weight = newWeight, op = Op.Const(n))

  def next: Set[State] = aoc.Dir.values.map(d => go(pos.move(d))).toSet

val start = State(weight = 22, pos = Point(0,3), op = Op.Const(22))
val end = State(weight = 30, pos = Point(3,0), Op.Const(30))

def search: List[State] =
  import collection.mutable.{PriorityQueue, Map}
  var current = start
  val cost = Map[State, Int](current -> 0)

  // def h(s: State): Int = s.weight / 7 + 100 + s.pos.dist(end.pos)
  def h(s: State): Int = s.pos.dist(end.pos)
  val pq = PriorityQueue(current)(using Ordering.by[State, Int](s => cost(s) + h(s)).reverse)
  // val pq = PriorityQueue(current)(using Ordering.by(cost).reverse)
  val parent = Map.empty[State, State]

  // while current.pos != end.pos || (current.weight & 0x7FFF) != end.weight do
  while current != end do
    println(s"${cost(current)}: $current")
    for
      s <- current.next
      // if s.weight > 0.toLit // TODO: Verify
      // check highest bit is not 1?
      if s.weight > 0 // prevents evap
      if s.weight <= 0x7FFF // prevents shatter
      if !cost.contains(s) // dedup, prevents suboptimal
      if !((s.pos == end.pos) && (s == end))
      if !((s.pos == start.pos) && (s == start))
    do
      cost(s) = cost(current) + 1
      parent(s) = current
      pq.enqueue(s)

    current = pq.dequeue()

  println(s"ended at $current")
  import collection.immutable.Queue
  def path(s: State): Queue[State] =
    parent.get(s) match
      case Some(p) => path(p).enqueue(s)
      case None => Queue(s)

  import aoc.Dir
  val poss = path(current).map(_.pos)
  val dirs = poss.zip(poss.tail).map:
    case (p, q) =>
      aoc.Dir.values.collectFirst:
        case d if p.move(d) == q => d
      .get


  dirs.foreach:
    case Dir.N => println("north")
    case Dir.E => println("east")
    case Dir.S => println("south")
    case Dir.W => println("west")

  path(current).toList



start.n.e.e.s.w.n.n.s.e.n.s.n.e.n

// val ans = search
// ans foreach println
