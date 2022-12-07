val input = io.Source.fromResource("2022/day-07.txt").getLines().toList
// val input = io.Source.fromResource("2022/day-07-ex.txt").getLines().toList

case class Dir(name: String)
case class File(name: String, size: Long)

// how could ans be too small?
// I could be missing subdirectories
// I could be missing files
// size of a directory too small

// how could ans be too large?
// counting directories multiple times
// counting files multiple times

// for each file, determine dir

// val adj = collection.mutable.Map[String, Set[Dir | File]]
// val adj = collection.mutable.Map.empty[String, Set[String]]
// val size = collection.mutable.Map[String, Set[Dir | File]]

type Path = List[String]

def run(
    output: List[String],
    path: Path
): Option[(List[(Path, Dir | File)], (List[String], Path))] =
  output match
    case Nil                   => None
    case "$ cd /" :: rest      => Some(Nil -> (rest, List("root")))
    case "$ cd .." :: rest     => Some(Nil -> (rest, path.tail))
    case s"$$ cd $dir" :: rest => Some(Nil -> (rest, (dir :: path)))
    case "$ ls" :: rest =>
      val contents = rest.takeWhile(o => !o.startsWith("$"))
      val values: List[(Path, Dir | File)] = contents.map {
        case s"dir $name"   => path -> Dir(name)
        case s"$size $name" => path -> File(name, size.toLong)
      }
      Some(values -> (rest.dropWhile(o => !o.startsWith("$")) -> path))
    case _ =>
      println(output.head)
      ???

val adj: Map[Path, List[Dir | File]] =
  LazyList.unfold[List[(Path, Dir | File)], (List[String], List[String])](
    input -> List.empty[String]
  )(run).toList.flatten
  .groupMap(_._1)(_._2)

val memo = collection.mutable.Map.empty[Path, Long]

// def size(dirPath: Path): Long =
//   memo.getOrElseUpdate(
//     dirPath,
//     adj(dirPath).map {
//       case File(name, size) => size
//       case Dir(name) => size(name :: dirPath)
//     }.sum
//   )

def size(path: Path): Long =
  memo.getOrElseUpdate(
    path,
    if adj.contains(path) then
      adj(path).map {
        case File(name, _) => size(name :: path)
        case Dir(name) => size(name :: path)
      }.sum
    else adj(path.tail).collectFirst {
      case File(name, size) if path.head == name => size
    }.get
  )

adj.keys.forall(k => adj(k).size > 0)
adj.contains("root" :: Nil)
size("root" :: Nil)

memo.keys.toList.sortBy(_.reverse.mkString)
  .map(p =>
    p.reverse.mkString("/") + " " + size(p) + " " +
    (if size(p) <= 100000 then "*" else "LARGE")
  )
  .foreach(println)

2 + 2

adj.keys.toList.sortBy(_.reverse.mkString)
  .filter(size(_) <= 100000)
  .map(d => d.reverse.mkString("/") + " " + size(d))
  .foreach(println)

adj.keys.toList.sortBy(_.reverse.mkString)
  .map(d =>
    d.reverse.mkString("/") + " " + size(d) + " " +
    (if size(d) <= 100000 then "*" else "LARGE")
  )
  .foreach(println)

val p = "root/jssnn/zjbvwsnv".split('/').reverse.toList
size(p)
adj(p) foreach println
adj("cqhb" :: p) foreach println
adj("zjbvwsnv" :: p) foreach println
size("cqhb" :: p)
size("zjbvwsnv" :: p)

size("root" :: Nil)

memo.toList.sortBy(_._2) takeRight 10 foreach println

memo.size
adj.size

input.count {
  case s"$number $filename" => number.toIntOption.isDefined
}
471 - 183

input.count {
  _ == "$ ls"
}

adj.keys.map(size).count(_ <= 100000)

adj.keys.filter(size(_) <= 100000) foreach println

List(1,2,3).map(_ => 4)
Set(1,2,3).map(_ => 4)

val ans1 = adj.keys.toList.map(size).filter(_ <= 100000).sum

adj.keys.toList.map(p => p -> size(p)).sortBy(_._2) foreach println

val spaceAvailable = 70_000_000 - size("root" :: Nil)

val spaceNeeded = 30000000 - spaceAvailable

val ans2 = adj.keys.toList.map(p => p -> size(p)).sortBy(_._2)
  .find {
    case (path, size) => size >= spaceNeeded
  }.get._2


adj.keys.toList.map(p => p -> size(p)).sortBy(_._2)
  .filter(_._2 >= 8381165) foreach println

  // .find(_._2 >= 8381165).get._2

adj.keys.map(size).filter(_ <= 100000).sum

    // if files.contains(name) then files(name)
    // else contents(name).toList.map(size(_, contents)).sum

adj("root" :: Nil)

size("root" :: Nil)

size("pcccp" :: "root" :: Nil)

// memo.

// tests

adj foreach println
adj("jssnn" :: "root" :: Nil)
adj("bphfqs" :: "jssnn" :: "root" :: Nil)
// size("dhcrzvbr.wmn" :: "bphfqs" :: "jssnn" :: "root" :: Nil)
size("bphfqs" :: "jssnn" :: "root" :: Nil)
size("dbnsfp" :: "jssnn" :: "root" :: Nil)
adj("dbnsfp" :: "jssnn" :: "root" :: Nil)

adj.keys.filter(_.contains("pphv")) foreach println
val pphvPath = adj.keys.filter(_.contains("pphv")).head
adj(pphvPath)
size(pphvPath)
// adj(pphvPath).map(_ :: pphvPath).map(size)
// adj(pphvPath).map(_ :: pphvPath).map(size).sum

// adj.keys.toList.sortBy(_.size) foreach println

// // adj("dhcrzvbr.wmn" :: "bphfqs" :: "jssnn" :: "root" :: Nil)
// // size("jssnn" :: "root" :: Nil)
