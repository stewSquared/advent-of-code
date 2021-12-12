val edges: List[(String, String)] = io.Source.fromResource("2021/day-12-1-test.txt")
  .getLines.toList.flatMap { case s"$a-$b" => List(a -> b, b -> a) }

val adjacent: Map[String, List[String]] = edges.groupMap(_._1)(_._2).toMap

def search(path: List[String], twiceAllowed: Boolean): List[List[String]] =
  adjacent(path.head).flatMap {
    case "start"                => Nil
    case "end"                  => ("end" :: path).reverse :: Nil
    case b if b.head.isUpper    => search(b :: path, twiceAllowed)
    case s if !path.contains(s) => search(s :: path, twiceAllowed)
    case s if twiceAllowed      => search(s :: path, twiceAllowed = false)
    case _                      => Nil
  }

search(List("start"), twiceAllowed = false).size
search(List("start"), twiceAllowed = true).size
