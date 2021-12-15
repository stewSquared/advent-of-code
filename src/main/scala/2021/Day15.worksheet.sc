import collection.mutable.{PriorityQueue, Map}

val input = io.Source.fromResource("2021/day-15-1.txt").getLines.toVector

type Grid = Vector[Vector[Int]]

val riskLevels: Grid = input.map(_.map(_.asDigit).toVector).toVector

val fullRiskLevels: Grid =
  def inc(by: Int)(risk: Int): Int = (risk + by - 1) % 9 + 1
  (0 until 5).foldLeft(Vector.empty) { (gridAcc, dy) =>
    gridAcc ++ riskLevels.map { row =>
      (0 until 5).foldLeft(Vector.empty) { (rowAcc, dx) =>
        rowAcc ++ row.map(inc(by = dy + dx))
      }
    }
  }

case class Point(x: Int, y: Int):
  def r = copy(x = x + 1)
  def d = copy(y = y + 1)
  def l = copy(x = x - 1)
  def u = copy(y = y - 1)

extension (rows: Grid)
  def riskAt(p: Point): Int = rows(p.y)(p.x)

  def adjacent(p: Point) = Set(p.r, p.d, p.l, p.u).filter { q =>
    rows.indices.contains(q.x) && rows(0).indices.contains(q.y)
  }

  def lowestTotalRiskPath: Int =
    val end = Point(rows(0).indices.last, rows.indices.last)
    var visiting = Point(0, 0)
    val totalRisk = Map(visiting -> 0)
    val toVisit = PriorityQueue.empty[Point](Ordering.by(totalRisk)).reverse

    while visiting != end do
      for q <- adjacent(visiting) if !totalRisk.contains(q) do
        totalRisk(q) = totalRisk(visiting) + riskAt(q)
        toVisit.enqueue(q)
      visiting = toVisit.dequeue()

    totalRisk(end)

val ans1 = riskLevels.lowestTotalRiskPath
val ans2 = fullRiskLevels.lowestTotalRiskPath
