val input = io.Source.fromResource("2022/day-20.txt").getLines().toVector
val numbers = input.map(_.toLong)

type Perm = Array[Int]
val id: Perm = numbers.indices.toArray

def shuffle(numbers: Vector[Long])(p: Perm, i: Int): Perm =
  val s = p(i)
  val n = (numbers(i) + s) % (numbers.size - 1) - s match
    case x if s + x < 0 => (x + numbers.size - 1).toInt
    case x              => x.toInt
  if (n % numbers.size) == 0 then p
  else
    val e = s + n
    val r = (s to e by n.sign)
    p.map {
      case `s` => e
      case j if r.contains(j) => j - n.sign
      case j => j
    }

def permute[T](numbers: Vector[T], p: Perm): Vector[T] =
  numbers.zipWithIndex
    .sortBy((n, i) => p(i))
    .map((n, i) => n)
    .toVector

def groveCoordinateSum(mixed: Vector[Long]) =
  val circ = Iterator.continually(mixed).flatten.dropWhile(_ != 0).drop(1)
  val x = circ.drop(999).next()
  val y = circ.drop(999).next()
  val z = circ.drop(999).next()
  x + y + z

val perm1 = id.foldLeft(id)(shuffle(numbers))
val ans1 = groveCoordinateSum(permute(numbers, perm1))

val key = 811589153L
val decrypted = numbers.map(_.toLong * key)

val perm2 = LazyList.iterate(id)(id.foldLeft(_)(shuffle(decrypted)))(10)
val ans2 = groveCoordinateSum(permute(decrypted, perm2))
