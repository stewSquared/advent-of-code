case class Tile(id: Int, top: String, bot: String, left: String, right: String, inner: Vector[String] = Vector.empty):
  val edges = Vector(top, bot, left, right)
  val reversed = edges.map(_.reverse)
  def matches(that: Tile): Boolean = (edges ++ reversed).intersect(that.edges).nonEmpty

  def rot: Tile = copy(top = left.reverse, left = bot, bot = right.reverse, right = top)
  def flip: Tile = copy(left = right, right = left, top = top.reverse, bot = bot.reverse)

  private[Tile] def rotateToOrient(edge: String): Option[Tile] =
    List.iterate(this, 4)(_.rot).find(_.left == edge)

  def orientToLeft(edge: String): Option[Tile] =
    rotateToOrient(edge) orElse this.flip.rotateToOrient(edge)

  def orientToTop(edge: String): Option[Tile] = orientToLeft(edge).map(_.rot)

val tiles: List[Tile] = util.Using(io.Source.fromResource("2020/day-20.txt")) { s =>
  val lines = s.getLines()
  val lb = collection.mutable.ListBuffer.empty[Tile]
  while lines.hasNext do
    val s"Tile $id:" = lines.next()
    val grid = lines.takeWhile(_.nonEmpty).toList
    val top = grid.head
    val left = grid.map(_.head).mkString
    val right = grid.map(_.last).mkString
    val bot = grid.last
    lb += Tile(id.toInt, top, bot, left, right)
  lb.result()
}.get

tiles foreach println
tiles.size

val adjacent: Map[Int, Set[Int]] =
  tiles.map { t =>
    val matching = tiles.collect {
      case s if t.id != s.id && t.matches(s) => s.id
    }
    t.id -> matching.toSet
  }.toMap

adjacent.view.mapValues(_.size) foreach println

adjacent.count(_._2.size == 2)

val ans1 = adjacent.collect{ case (t, ss) if ss.size == 2 => t.toLong }.product

val tilesById: Map[Int, Tile] =
  tiles.groupBy(_.id).view.mapValues(_.head).toMap

tiles.map(_.id)

val topLeft =
  val (id, adj) = adjacent.find(_._2.size == 2).get
  val tile = tilesById(id)
  val List(bot, right) = adj.toList.map(tilesById)
  val rots =
    val cw = List.iterate(tile, 4)(_.rot)
    val ccw = List.iterate(tile.flip, 4)(_.rot)
    cw ++ ccw
  rots.find(t => right.orientToLeft(t.right).isDefined && bot.orientToTop(t.bot).isDefined).get

topLeft

tiles.size
adjacent.count(_._2.size == 3)

case class Point(x: Int, y: Int)

type Grid = Map[Point, Tile]

val initialGrid: Map[Point, Tile] = Map(Point(0, 0) -> topLeft)

// def fillTop(start: Grid): Grid =
//   def addTop(grid: Grid, point: Point): Grid =
//     // choose between two tiles adjacent to the tile to the left of point, and add to grid
//   (1 until 12).map(Point(_, 0)).foldLeft(start)(addTop(_, _))

// def fillLeft(start: Grid): Grid = ??? // similar

// def fillInner(start: Grid): Grid = ???


//// testing

// val tl = tilesById(1951)

// val top = tilesById(2311).orientToLeft(tl.right)

// tilesById(3079).orientToLeft(top.get.right)

// tl.bot

// val left = tilesById(2729)
// left.left
// left.right
// left.top
// left.bot

// tilesById(2729).orientToTop(tl.top)
