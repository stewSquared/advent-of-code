val input = io.Source.fromResource("2022/day-07.txt").getLines().toList

case class Dir(name: String)
case class File(name: String, size: Long)

type Path = List[String]
type State = (List[String], Path)

def step(
    lines: List[String],
    path: Path
): Option[(List[(Path, Dir | File)], State)] =
  lines match
    case Nil                   => None
    case "$ cd /" :: rest      => Some(Nil -> (rest, List("root")))
    case "$ cd .." :: rest     => Some(Nil -> (rest, path.tail))
    case s"$$ cd $dir" :: rest => Some(Nil -> (rest, (dir :: path)))
    case "$ ls" :: rest =>
      val stdout = rest.takeWhile(o => !o.startsWith("$"))
      val contents: List[(Path, Dir | File)] = stdout.map {
        case s"dir $name"   => path -> Dir(name)
        case s"$size $name" => path -> File(name, size.toLong)
      }
      Some(contents -> (rest.dropWhile(o => !o.startsWith("$")) -> path))

val adj: Map[Path, List[Dir | File]] =
  LazyList
    .unfold[List[(Path, Dir | File)], State](input -> List.empty[String])(step)
    .toList
    .flatten
    .groupMap(_._1)(_._2)

val memo = collection.mutable.Map.empty[Path, Long]

def size(dirPath: Path): Long =
  memo.getOrElseUpdate(
    dirPath,
    adj(dirPath).map {
      case File(name, size) => size
      case Dir(name)        => size(name :: dirPath)
    }.sum
  )

val ans1 = adj.keysIterator.map(size).filter(_ <= 100000).sum

val ans2 =
  val spaceAvailable = 70_000_000 - size("root" :: Nil)
  val spaceNeeded = 30000000 - spaceAvailable
  adj.keys.toList.map(size).sorted.find(_ >= spaceNeeded).get
