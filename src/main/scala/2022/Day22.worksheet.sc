val input = io.Source.fromResource("2022/day-22.txt").getLines().toVector

val mapInput = input.takeWhile(_.nonEmpty)

mapInput foreach println

val map: Map[Point, Char] =
  (for
    (row, y) <- mapInput.zipWithIndex
    (char, x) <- row.zipWithIndex
    if char != ' '
  yield Point(x + 1, y + 1) -> char).toMap

enum Turn:
  case R, L

object Turn:
  def fromChar(c: Char): Turn = Turn.fromOrdinal("RL".indexOf(c))

val path = input.drop(mapInput.size + 1).head

val pathSteps: List[Int | Turn] =
  Iterator.unfold(path) { remaining =>
    Option.when[(Int | Turn, String)](remaining.nonEmpty) {
      remaining.span(_.isDigit) match
        case (steps, next) if steps.nonEmpty => steps.toInt -> next
        case _ => Turn.fromChar(remaining.head) -> remaining.tail
    }
  }.toList

enum Dir:
  case E, S, W, N

  def turnRight = this match
    case Dir.E => S
    case Dir.S => W
    case Dir.W => N
    case Dir.N => E

  def turnLeft = this match
    case Dir.E => N
    case Dir.S => E
    case Dir.W => S
    case Dir.N => W

case class Point(x: Int, y: Int):
  def move(d: Dir) = d match
    case Dir.E => copy(x = x + 1)
    case Dir.S => copy(y = y + 1)
    case Dir.W => copy(x = x - 1)
    case Dir.N => copy(y = y - 1)

enum Face(xRange: Range, yRange: Range):
  case D extends Face(51 to 100, 1 to 50)
  case R extends Face(101 to 150, 1 to 50)
  case F extends Face(51 to 100, 51 to 100)
  case U extends Face(51 to 100, 101 to 150)
  case L extends Face(1 to 50, 101 to 150)
  case B extends Face(1 to 50, 151 to 200)

  def contains(p: Point) = xRange.contains(p.x) && yRange.contains(p.y)
  val edge: Map[Dir, Seq[Point]] = Map(
    Dir.E -> yRange.map(Point(xRange.max, _)),
    Dir.S -> xRange.map(Point(_, yRange.max)),
    Dir.W -> yRange.map(Point(xRange.min, _)),
    Dir.N -> xRange.map(Point(_, yRange.min))
  )

object Face:
  def containing(p: Point): Face =
    Face.values.find(_.contains(p)).get

def move(p: Point, dir: Dir, blocking: Boolean = true): (Point, Dir) =
  val face = Face.containing(p)
  val (nextDir, nextPos) = (face, dir) match
    case (Face.R, Dir.E) if face.edge(dir).contains(p) =>
      assert(!map.contains(p.move(dir)))
      val edge = face.edge(dir)
      val target = Face.U.edge(Dir.E).reverse // pass
      Dir.W -> edge.zip(target).toMap.apply(p)
    case (Face.R, Dir.S) if face.edge(dir).contains(p) =>
      assert(!map.contains(p.move(dir)))
      val edge = face.edge(dir)
      val target = Face.F.edge(Dir.E)
      Dir.W -> edge.zip(target).toMap.apply(p)
    case (Face.R, Dir.N) if face.edge(dir).contains(p) =>
      assert(!map.contains(p.move(dir)))
      val edge = face.edge(dir)
      val target = Face.B.edge(Dir.S)
      Dir.N -> edge.zip(target).toMap.apply(p)
    case (Face.D, Dir.N) if face.edge(dir).contains(p) =>
      assert(!map.contains(p.move(dir)))
      val edge = face.edge(dir)
      val target = Face.B.edge(Dir.W)
      Dir.E -> edge.zip(target).toMap.apply(p)
    case (Face.D, Dir.W) if face.edge(dir).contains(p) =>
      assert(!map.contains(p.move(dir)))
      val edge = face.edge(dir)
      val target = Face.L.edge(Dir.W).reverse // pass
      Dir.E -> edge.zip(target).toMap.apply(p)
    case (Face.F, Dir.W) if face.edge(dir).contains(p) =>
      assert(!map.contains(p.move(dir)))
      val edge = face.edge(dir)
      val target = Face.L.edge(Dir.N)
      Dir.S -> edge.zip(target).toMap.apply(p)
    case (Face.F, Dir.E) if face.edge(dir).contains(p) =>
      assert(!map.contains(p.move(dir)))
      val edge = face.edge(dir)
      val target = Face.R.edge(Dir.S)
      Dir.N -> edge.zip(target).toMap.apply(p)
    case (Face.U, Dir.E) if face.edge(dir).contains(p) =>
      assert(!map.contains(p.move(dir)))
      val edge = face.edge(dir)
      val target = Face.R.edge(Dir.E).reverse // pass
      Dir.W -> edge.zip(target).toMap.apply(p)
    case (Face.U, Dir.S) if face.edge(dir).contains(p) =>
      assert(!map.contains(p.move(dir)))
      val edge = face.edge(dir)
      val target = Face.B.edge(Dir.E)
      Dir.W -> edge.zip(target).toMap.apply(p)
    case (Face.L, Dir.N) if face.edge(dir).contains(p) =>
      assert(!map.contains(p.move(dir)))
      val edge = face.edge(dir)
      val target = Face.F.edge(Dir.W)
      Dir.E -> edge.zip(target).toMap.apply(p)
    case (Face.L, Dir.W) if face.edge(dir).contains(p) =>
      assert(!map.contains(p.move(dir)))
      val edge = face.edge(dir)
      val target = Face.D.edge(Dir.W).reverse // pass
      Dir.E -> edge.zip(target).toMap.apply(p)
    case (Face.B, Dir.E) if face.edge(dir).contains(p) =>
      assert(!map.contains(p.move(dir)))
      val edge = face.edge(dir)
      val target = Face.U.edge(Dir.S)
      Dir.N -> edge.zip(target).toMap.apply(p)
    case (Face.B, Dir.S) if face.edge(dir).contains(p) =>
      assert(!map.contains(p.move(dir)))
      val edge = face.edge(dir)
      val target = Face.R.edge(Dir.N)
      Dir.S -> edge.zip(target).toMap.apply(p)
    case (Face.B, Dir.W) if face.edge(dir).contains(p) =>
      assert(!map.contains(p.move(dir)))
      val edge = face.edge(dir)
      val target = Face.D.edge(Dir.N)
      Dir.S -> edge.zip(target).toMap.apply(p)
    case _ =>
      assert(map.contains(p.move(dir)))
      dir -> p.move(dir)

  map(nextPos) match
    case '#' if blocking =>
      println(s"blocked attempting to go to $nextPos on ${Face.containing(nextPos)}")
      p -> dir
    case _ if !blocking =>
      if (Face.containing(nextPos) != Face.containing(p)) {
        println(s"wrapping from ${Face.containing(p)} to $nextPos on ${Face.containing(nextPos)}")
      }
      nextPos -> nextDir

// Part 1
// def move(p: Point, d: Dir): (Point, Dir) =
//   val next = p.move(d)
//   val wrapped: Point = if map.contains(next) then next else
//     println("wrapping around")
//     d match
//       case Dir.E => map.keys.filter(_.y == next.y).minBy(_.x)
//       case Dir.S => map.keys.filter(_.x == next.x).minBy(_.y)
//       case Dir.W => map.keys.filter(_.y == next.y).maxBy(_.x)
//       case Dir.N => map.keys.filter(_.x == next.x).maxBy(_.y)

//   map(wrapped) match
//     case '#' => p -> d
//     case '.' => wrapped -> d

val startPoint = Point(51, 1)

val (finalPos, finalDir) = pathSteps.foldLeft[(Point, Dir)](startPoint -> Dir.E){
  case ((pos, dir), inst) =>
    inst match
      case steps: Int =>
        LazyList.iterate(pos -> dir)(move(_, _))(steps)
      case Turn.R =>
        pos -> dir.turnRight
      case Turn.L =>
        pos -> dir.turnLeft
}

val ans = 1000 * finalPos.y + 4 * finalPos.x + finalDir.ordinal

println(Face.D)

var p = Point(51, 1)
Face.containing(p)
var d = Dir.N
p.move(d)
var d2 = move(p, d)._2
var p2 = Face.containing(move(p, d)._1)
move(p, d)._1
println((p2, d2))

p = Point(51, 1)
d = Dir.W
d2 = move(p, d)._2
p2 = Face.containing(move(p, d)._1)
move(p, d)._1
println((p2, d2))

p = Point(51,51)
d = Dir.W
d2 = move(p, d)._2
p2 = Face.containing(move(p, d)._1)
move(p, d)._1
println((p2, d2))

p = Point(100,51)
d = Dir.E
d2 = move(p, d)._2
p2 = Face.containing(move(p, d)._1)
move(p, d)._1
println((p2, d2))

p = Point(101,1)
d = Dir.N
d2 = move(p, d)._2
p2 = Face.containing(move(p, d)._1)
move(p, d)._1
println((p2, d2))

p = Point(150,1)
d = Dir.E
d2 = move(p, d)._2
p2 = Face.containing(move(p, d)._1)
move(p, d)._1
println((p2, d2))

p = Point(150,50)
d = Dir.S
d2 = move(p, d)._2
p2 = Face.containing(move(p, d)._1)
move(p, d)._1
println((p2, d2))

p = Point(100,101)
d = Dir.E
d2 = move(p, d)._2
p2 = Face.containing(move(p, d)._1)
move(p, d)._1
println((p2, d2))

p = Point(100,150)
d = Dir.S
d2 = move(p, d)._2
p2 = Face.containing(move(p, d)._1)
move(p, d)._1
println((p2, d2))

p = Point(1,101)
d = Dir.N
Face.containing(p)
d2 = move(p, d)._2
p2 = Face.containing(move(p, d)._1)
move(p, d)._1
println((p2, d2))

p = Point(1,101)
d = Dir.W
d2 = move(p, d)._2
p2 = Face.containing(move(p, d)._1)
move(p, d)._1
println((p2, d2))

p = Point(1,151)
d = Dir.W
d2 = move(p, d)._2
p2 = Face.containing(move(p, d)._1)
move(p, d)._1
println((p2, d2))

p = Point(1,200)
d = Dir.S
d2 = move(p, d, false)._2
p2 = Face.containing(move(p, d, false)._1)
move(p, d)._1
println((p2, d2))

p = Point(50,200)
d = Dir.E
d2 = move(p, d)._2
p2 = Face.containing(move(p, d)._1)
move(p, d)._1
println((p2, d2))

def testLooping(p: Point): Unit =
  for d <- Dir.values do
    println(s"moving 200 in $d")
    val end = List.iterate((p, d), 200 + 1)(move(_,_,false)).last
    assert((p, d) == end, s"$end was not ${p -> d}")

testLooping(Point(51,1))
