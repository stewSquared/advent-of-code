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

  case class State(weight: Lit, pos: Point, op: Op):
    def next: Set[State] =
      // TODO: avoid topright
      pos.adjacent.filter(_.inBounds(area)).map: p =>
        op match
          case Op.Const => grid(p) match
            case "x" => this.copy(pos = p, op = Op.Mul)
            case "+" => this.copy(pos = p, op = Op.Add)
            case "-" => this.copy(pos = p, op = Op.Sub)
          case _ =>
            val n = grid(p).toInt.toLit
            val newWeight = op match
              case Op.Mul => weight * n
              case Op.Add => weight + n
              case Op.Sub => weight + (~n + 1.toLit)
              case _ => ???
            this.copy(pos = p, weight = newWeight, op = Op.Const)

  def search: List[State] =
    import collection.mutable.{PriorityQueue, Map}
    var current = State(weight = 22.toLit, pos = Point(0,3), op = Op.Const)
    val cost = Map[State, Int](current -> 0)
    val pq = PriorityQueue(current)(using Ordering.by(cost).reverse)
    val parent = Map.empty[State, State]

    val end = State(weight = 30.toLit, pos = Point(3, 0), Op.Const)
    while current != end do
      println(s"${cost(current)}: $current")
      for
        s <- current.next
        // if s.weight > 0.toLit // TODO: Verify
        // check highest bit is not 1?
        if !cost.contains(s)
        if !((s.pos == end.pos) && (s == end))
      do
        cost(s) = cost(current) + 1
        parent(s) = current
        pq.enqueue(s)

      current = pq.dequeue()

    import collection.immutable.Queue
    def path(s: State): Queue[State] =
      parent.get(s) match
        case Some(p) => path(p).enqueue(s)
        case None => Queue(s)

    path(end).toList

  val ans = search
  println(ans)
