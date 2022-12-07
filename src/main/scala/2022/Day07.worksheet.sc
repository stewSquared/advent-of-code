val input = io.Source.fromResource("2022/day-07.txt").getLines().toList

case class Dir(path: List[String]):
  def up = copy(path = path.tail)
  def cd(sub: String) = copy(path = sub :: path)
  def pathString = "/" + path.reverse.mkString("/")

object Dir:
  val root = Dir(Nil)

case class File(name: String, size: Long)

def buildContents(
    lines: List[String],
    curDir: Dir = Dir.root,
    contents: Map[Dir, List[File | Dir]] = Map.empty
): Map[Dir, List[File | Dir]] =
  lines.headOption.fold(contents) {
    case "$ cd /"      => buildContents(lines.tail, Dir.root, contents)
    case "$ cd .."     => buildContents(lines.tail, curDir.up, contents)
    case s"$$ cd $dir" => buildContents(lines.tail, curDir.cd(dir), contents)
    case "$ ls" =>
      val (stdout, nextCommands) = lines.tail.span(o => !o.startsWith("$"))
      val listedFiles: List[Dir | File] = stdout.map {
        case s"dir $sub"    => curDir.cd(sub)
        case s"$size $name" => File(name, size.toLong)
      }
      buildContents(nextCommands, curDir, contents.updated(curDir, listedFiles))
  }

val contents = buildContents(input, Dir.root, Map.empty)

val memo = collection.mutable.Map.empty[Dir, Long]

def size(dir: Dir): Long =
  memo.getOrElseUpdate(
    dir,
    contents(dir).map {
      case File(_, size) => size
      case sub: Dir      => size(sub)
    }.sum
  )

val ans1 = contents.keysIterator.map(size(_)).filter(_ <= 100000).sum

val ans2 =
  val spaceAvailable = 70_000_000 - size(Dir.root)
  val spaceNeeded = 30000000 - spaceAvailable
  memo.values.toList.sorted.find(_ >= spaceNeeded).get

contents.keysIterator
  .map(d => size(d) -> d.pathString)
  .filter(_._1 <= 100000)
  .foreach { case (size, path) =>
    println(s"$path $size")
  }
