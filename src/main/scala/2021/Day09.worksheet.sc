import io.Source
import util.Using
import util.chaining.*

type Grid = Vector[Vector[Int]]

case class Point(x: Int, y: Int)

val heightMap: Grid = Using(Source.fromResource("2021/day-09-1.txt")) {
  _.getLines.map(_.map(_.asDigit).toVector).toVector
}.get

heightMap foreach println


extension (grid: Grid)
  def heightAt(p: Point): Int = grid(p.y)(p.x)
  def adjacent(p: Point): List[Point] = List(
    p.copy(x = p.x + 1),
    p.copy(x = p.x - 1),
    p.copy(y = p.y + 1),
    p.copy(y = p.y - 1)
  ).filter {
    case Point(x, y) =>
      y >= 0
      && y < grid.length
      && x >= 0
      && x < grid(0).length
  }

  def lowPoints: Seq[Point] = for {
    y <- 0 until grid.length
    x <- 0 until grid(0).length
    p = Point(x, y)
    // _ = println(p)
    if adjacent(p).forall(q => heightAt(q) > heightAt(p))
  } yield p // heightAt(p)

  def basin(low: Point): Set[Point] = {
    def recur(from: Point, visited: Set[Point]): Set[Point] = {
      val next = adjacent(from)
        .filterNot(visited.contains)
        .filter(p => heightAt(p) > heightAt(from))
        .filterNot(heightAt(_) == 9)

      next.map(recur(_, visited + from)).fold(Set.empty)(_ union _) + from
    }
    recur(low, Set(low))
  }

  // def basin(low: Point): Set[Point] = {
  //   def recur(from: Point, visited: Set[Point]): Set[Point] = {
  //     val next = adjacent(from)
  //       .filterNot(visited.contains)
  //       // .forall(p => heightAt(p) > heightAt(from)).toSet

  //       // if !next.exists(p => heightAt(p) <= heightAt(from))
  //     if next.forall(p => heightAt(p) > heightAt(from))
  //     then {
  //       println(s"$from, height: ${heightAt(from)}, next: $next")
  //       next.map(recur(_, visited ++ next)).fold(Set.empty)(_ union _) + from
  //     } else {
  //       println(s"skipping $from, height ${heightAt(from)}")
  //       Set.empty
  //     }
  //     // next.map(recur(_, visited + from)).fold(Set.empty)(_ union _) + from
  //   }
  //   recur(low, Set(low))
  // }

  def foo(from: Point, visited: Set[Point]): Set[Point] = {
    adjacent(from)
      .filterNot(visited.contains)
      .filter(p => heightAt(p) > heightAt(from)).toSet
  }

val ans1 = heightMap.lowPoints.map(p => heightMap.heightAt(p) + 1).sum

heightMap.foo(Point(1,0), Set.empty)
heightMap.foo(Point(2,0), Set(Point(1,0)))

heightMap.foo(Point(9,0), Set.empty)

heightMap.foo(Point(9,1), Set(Point(9,0)))
heightMap.foo(Point(8,0), Set(Point(9,0)))

heightMap.foo(Point(9,2), Set(Point(9,0), Point(9,1)))


val largestBasins = heightMap.lowPoints.sortBy(heightMap.basin(_).size).reverse.take(3)

val ans2 = heightMap.lowPoints.map(heightMap.basin(_).size).sorted.reverse.take(3).product

heightMap.lowPoints.map(heightMap.basin)

heightMap.lowPoints foreach println

heightMap.basin(Point(9, 0)) foreach println

heightMap.basin(Point(9, 0)).size

heightMap.basin(Point(6, 4)).size

heightMap.basin(Point(2, 2)).size

heightMap.basin(Point(2, 2)).toList.sortBy(p => p._1 -> p._2) foreach println

heightMap.basin(Point(1,0))

heightMap
  .adjacent(Point(1,0))
  .filter(p => heightMap.heightAt(p) > heightMap.heightAt(Point(1,0))).size

// lowPoints.map(p => p -> heightMap.heightAt(p)) foreach println
