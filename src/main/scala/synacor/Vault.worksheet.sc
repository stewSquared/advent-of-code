import aoc.*

val grid = Vector(
  "x,8,-,1",
  "4,x,11,x",
  "+,4,-,18",
  "22,-,9,x"
).map(_.split(',').toVector)

grid(Point(0,3))

val goal = 30


val area = Area(grid)

grid(Point(0,2))

enum Op:
  case Add, Sub, Mul, Const

import synacor.numbers.*

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

val s = State(weight = 22.toLit, pos = Point(0,3), op = Op.Const)

s.next foreach println
s.next.flatMap(_.next).filter(_.pos.inBounds(area)) foreach println

// def search: List[State] =
//   import collection.mutable.{PriorityQueue, Map}
//   var current = State(weight = 22.toLit, pos = Point(0,3), op = Op.Const)
//   val cost = Map[State, Int](current -> 0)
//   val pq = PriorityQueue(current)(using Ordering.by(cost).reverse)
//   val parent = Map.empty[State, State]

//   val end = State(weight = 30, pos = Point(3, 0), Op.Const)
//   while current != end do
//     println(s"${cost(current)}: $current")
//     for
//       s <- current.next
//       if !cost.contains(s)
//       if !((s.pos == end.pos) && (s == end))
//     do
//       cost(s) = cost(current) + 1
//       parent(s) = current
//       pq.enqueue(s)

//     current = pq.dequeue()

//   import collection.immutable.Queue
//   def path(s: State): Queue[State] =
//     parent.get(s) match
//       case Some(p) => path(p).enqueue(s)
//       case None => Queue(s)

//   path(end).toList

// val ans = search

synacor.numbers.U15.MaxValue


//


//
