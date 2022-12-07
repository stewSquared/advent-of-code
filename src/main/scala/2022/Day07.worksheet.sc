val input = io.Source.fromResource("2022/day-07.txt").getLines().toList

case class Dir(path: List[String]):
  def up = copy(path = path.tail)
  def cd(sub: String) = copy(path = sub :: path)

object Dir:
  val root = Dir(List("root"))

case class File(name: String, size: Long)

type State = (List[String], Dir)

def step(
    lines: List[String],
    curDir: Dir
): Option[(List[(Dir, Dir | File)], State)] =
  lines.headOption.map {
    case "$ cd /"      => Nil -> (lines.tail, Dir.root)
    case "$ cd .."     => Nil -> (lines.tail, curDir.up)
    case s"$$ cd $dir" => Nil -> (lines.tail, curDir.cd(dir))
    case "$ ls" =>
      val stdout = lines.tail.takeWhile(o => !o.startsWith("$"))
      val contents: List[(Dir, Dir | File)] = stdout.map {
        case s"dir $sub"   => curDir -> curDir.cd(sub)
        case s"$size $name" => curDir -> File(name, size.toLong)
      }
      contents -> (lines.tail.dropWhile(o => !o.startsWith("$")) -> curDir)
  }

val contents: Map[Dir, List[Dir | File]] =
  LazyList
    .unfold[List[(Dir, Dir | File)], State](input -> Dir.root)(step)
    .toList
    .flatten
    .groupMap(_._1)(_._2)

val memo = collection.mutable.Map.empty[Dir, Long]

def size(dir: Dir): Long =
  memo.getOrElseUpdate(
    dir,
    contents(dir).map {
      case File(name, size) => size
      case sub: Dir         => size(sub)
    }.sum
  )

val ans1 = contents.keysIterator.map(size(_)).filter(_ <= 100000).sum

val ans2 =
  val spaceAvailable = 70_000_000 - size(Dir.root)
  val spaceNeeded = 30000000 - spaceAvailable
  memo.values.toList.sorted.find(_ >= spaceNeeded).get
