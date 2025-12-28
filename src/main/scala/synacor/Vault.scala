package synacor
import synacor.numbers.*
import aoc.*

@main def vault(): Unit =
  val grid = Vector(
    "x,8,-,1",
    "4,x,11,x",
    "+,4,-,18",
    "22,-,9,x"
  ).map(_.split(',').toVector)

  val area = Area(grid)

  enum Op:
    case Add, Sub, Mul, Const

  case class State(weight: Int, pos: Point, op: Op):
    def next: Set[State] =
      // TODO: avoid topright
      pos.adjacent.filter(_.inBounds(area)).map: p =>
        op match
          case Op.Const => grid(p) match
            case "x" => this.copy(pos = p, op = Op.Mul)
            case "+" => this.copy(pos = p, op = Op.Add)
            case "-" => this.copy(pos = p, op = Op.Sub)
          case _ =>
            val n = grid(p).toInt
            val newWeight = op match
              // case Op.Mul => (weight * n) & 0x7FFF
              // case Op.Add => (weight + n) & 0x7FFF
              case Op.Mul => weight * n
              case Op.Add => weight + n
              case Op.Sub => weight - n
              // weight + (~n + 1.toLit)
              case _ => ???
            this.copy(pos = p, weight = newWeight, op = Op.Const)

  val start = State(weight = 22, pos = Point(0,3), op = Op.Const)
  val end = State(weight = 30, pos = Point(3,0), Op.Const)

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
        if s == end || s.pos != end.pos
        if s == start || s.pos != start.pos
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

  val ans = search
  ans foreach println

// State(22,Point(0,3),Const)
// north (22,Point(0,2),Add)
// east (26,Point(1,2),Const)
// east (26,Point(2,2),Sub)
// north (15,Point(2,1),Const)
// east (15,Point(3,1),Mul)
// south (270,Point(3,2),Const)
// west (270,Point(2,2),Sub)
// north (259,Point(2,1),Const)
// west (259,Point(1,1),Mul)
// south (1036,Point(1,2),Const)
// east (1036,Point(2,2),Sub)
// north (1025,Point(2,1),Const)
// west (1025,Point(1,1),Mul)
// west (4100,Point(0,1),Const)
// north (4100,Point(0,0),Mul)
// east (32800,Point(1,0),Const)
// east (32800,Point(2,0),Sub)
// east (32799,Point(3,0),Const)
// west (32799,Point(2,0),Sub)
// east (32798,Point(3,0),Const)

  // start.next foreach println
  // println()
  // println()
  // start.next.flatMap(_.next) foreach println
