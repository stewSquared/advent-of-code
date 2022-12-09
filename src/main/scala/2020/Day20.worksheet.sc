case class Tile(id: Int, top: String, bot: String, left: String, right: String, inner: Set[Point]):
  val width = top.length - 2
  val edges = Vector(top, bot, left, right)
  val reversed = edges.map(_.reverse)
  def matches(that: Tile): Boolean = (edges ++ reversed).intersect(that.edges).nonEmpty

  def rot: Tile = copy(
    top = left.reverse,
    left = bot,
    bot = right.reverse,
    right = top,
    inner.map(p => Point(x = width - 1 - p.y, y = p.x)) // Note this off-by-one to prevent in the future
  )

  def flip: Tile = copy(
    left = right,
    right = left,
    top = top.reverse,
    bot = bot.reverse,
    inner.map(p => p.copy(x = width - 1 - p.x))
  )

  private[Tile] def rotateToOrient(edge: String): Option[Tile] =
    List.iterate(this, 4)(_.rot).find(_.left == edge)

  def orientToLeft(edge: String): Option[Tile] =
    rotateToOrient(edge) orElse this.flip.rotateToOrient(edge)

  def orientToTop(edge: String): Option[Tile] = orientToLeft(edge).map(_.rot.flip)

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
    val innerGrid = grid.tail.init.map(_.tail.init).toVector
    val inner = for
      y <- grid.indices.dropRight(2)
      x <- grid.head.indices.dropRight(2)
      if grid(y + 1)(x + 1) == '#'
    yield Point(x, y)

    lb += Tile(id.toInt, top, bot, left, right, inner.toSet)
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

adjacent.count(_._2.size == 3)

adjacent.count(_._2.size == 4)


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

case class Point(x: Int, y: Int):
  def left: Point = copy(x = x - 1)
  def up: Point = copy(y = y - 1)

type Grid = Map[Point, Tile]

val initialGrid: Map[Point, Tile] = Map(Point(0, 0) -> topLeft)


def addTop(grid: Grid, point: Point): Grid =
  val leftTile = grid(point.left)
  val candidates = adjacent(leftTile.id).toList.map(tilesById).flatMap {
    t => t.orientToLeft(leftTile.right)
  }
  assert(candidates.length == 1)
  grid.updated(point, candidates.head)

val tilesAcross = math.sqrt(tiles.size).toInt

def fillTop(start: Grid): Grid =
  (1 until tilesAcross).map(Point(_, 0)).foldLeft(start)(addTop(_, _))

def addLeft(grid: Grid, point: Point): Grid =
  val topTile = grid(point.up)
  val candidates = adjacent(topTile.id).map(tilesById).toList.flatMap {
    t => t.orientToTop(topTile.bot)
  }
  assert(candidates.length == 1)
  grid.updated(point, candidates.head)

def fillLeft(start: Grid): Grid =
  (1 until tilesAcross).map(Point(0, _)).foldLeft(start)(addLeft(_, _))

def fillCorner(grid: Grid, point: Point): Grid =
  val leftTile = grid(point.left)
  val topTile = grid(point.up)

  val nextTileId = adjacent(leftTile.id).intersect(adjacent(topTile.id)).filterNot(_ == grid(point.up.left).id)

  assert(nextTileId.size == 1, s"$point, ${leftTile.id}, ${topTile.id}")
  // TODO: orient to top and left tiles
  val oriented = tilesById(nextTileId.head).orientToLeft(leftTile.right)
  // assert orienting to top changes nothing
  assert(oriented.isDefined, s"next tile $nextTileId")
  assert(oriented.flatMap(_.orientToTop(topTile.bot)).contains(oriented.get))
  grid.updated(point, oriented.get)

def fillInner(start: Grid): Grid =
  val points = for
    x <- 1 until tilesAcross
    y <- 1 until tilesAcross
  yield Point(x, y)
  points.foldLeft(start)(fillCorner(_, _))

val filledGrid = fillInner(fillLeft(fillTop(initialGrid)))

val finalImage = filledGrid.map {
  case (tilePos, tile) => tile.inner.toList.map(p =>
    Point(tilePos.x * tile.width + p.x, tilePos.y * tile.width + p.y))
}.flatten.toSeq.distinct.toSet//.reduce(_ union _)

val rawMonster = Vector(
  "                  # ".toVector,
  "#    ##    ##    ###".toVector,
  " #  #  #  #  #  #   ".toVector
)

val horizontalMonster = for
  x <- rawMonster.head.indices
  y <- rawMonster.indices
  if rawMonster(y)(x) == '#'
yield Point(x, y)

val vertRawMonster = rawMonster.transpose

val monsterOrientations = List(rawMonster, vertRawMonster).flatMap(
  m => List(m, m.map(_.reverse), m.reverse, m.reverse.map(_.reverse))
).map[Set[Point]] { m =>
  val points = for
    x <- m.head.indices
    y <- m.indices
    if m(y)(x) == '#'
  yield Point(x, y)
  points.toSet
}

for mo <- monsterOrientations do
  val width = mo.map(_.x).max
  val height = mo.map(_.y).max
  for y <- 0 to height do
    for x <- 0 to width do
      val p = Point(x, y)
      if mo(p) then print("O") else print(".")
    println()
  println()

val fullWidth = finalImage.maxBy(_.y).y

def search(monster: Set[Point]) = {
  for
    y <- 0 to fullWidth
    x <- 0 to fullWidth
    translate = monster.map(p => Point(x + p.x, y + p.y))
    if translate.subsetOf(finalImage)
  yield
    println(s"monster found at $x $y")
    translate
}

val monsterPoints = monsterOrientations.flatMap(search(_)).reduce(_ union _)

for y <- 0 until fullWidth do
  for x <- 0 until fullWidth do
    val p = Point(x, y)
    if monsterPoints(p) then print("O")
    else if finalImage(p) then print("#")
    else print(".")
  println()

monsterPoints.size
finalImage.size
val ans2 = finalImage.diff(monsterPoints).size
