package synacor
import synacor.numbers.*
import aoc.*

@main def vault2(): Unit =
  val adj = Map[Point, List[Int => Int]](
    Point(0,3) -> List(_ - 4, _ + 4),
    Point(1,2) -> List(_ + 4, _ * 4, _ * 11, _ - 11, _ - 9),
    Point(0,1) -> List(_ * 8, _ * 4, _ + 4),
    Point(0,1) -> List(_ * 4, _ * 11, _ - 11, _ - 1)
  )
  println(adj)
