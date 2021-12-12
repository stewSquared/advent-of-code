import io.Source
import util.chaining.*
import util.Using

enum Node(val name: String):
  case Start extends Node("start")
  case End extends Node("end")
  case Big(override val name: String) extends Node(name)
  case Small(override val name: String) extends Node(name)

object Node:
  def unapply(name: String): Option[Node] = Some {
    name match
      case "end" => End
      case "start" => Start
      case big if big.head.isUpper => Big(name)
      case small => Small(small)
  }

import Node.*

val edges = Using(Source.fromResource("2021/day-12-1-test.txt")) {
  _.getLines.toList.flatMap {
    case s"${Node(a)}-${Node(b)}" => List(a -> b, b -> a)
  }
}.get

val adjacent = edges.groupMap(_._1)(_._2).toMap[Node, List[Node]]

def search1(path: List[Node]): List[List[Node]] =
  adjacent(path.head).flatMap {
    case End                           => (End :: path).reverse :: Nil
    case b: Big                        => search1(b :: path)
    case s: Small if !path.contains(s) => search1(s :: path)
    case _                             => Nil
  }

def search2(path: List[Node], twiceAllowed: Boolean): List[List[Node]] =
  adjacent(path.head).flatMap {
    case End                           => (End :: path).reverse :: Nil
    case b: Big                        => search2(b :: path, twiceAllowed)
    case s: Small if !path.contains(s) => search2(s :: path, twiceAllowed)
    case s: Small if twiceAllowed      => search2(s :: path, twiceAllowed = false)
    case _                             => Nil
  }

val ans1 = search1(List(Start)).size
val ans2 = search2(List(Start), twiceAllowed = true).size
