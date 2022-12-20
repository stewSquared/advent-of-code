val numbers =
  io.Source.fromResource("2022/day-20.txt").getLines().map(_.toLong).toVector

// val numbers = Vector[Long](1, 2, -3, 3, -2, 0, 4)

type Perm = Array[Int]
val id: Perm = numbers.indices.toArray

def compose(p: Map[Int, Int], q: Perm): Perm =
  q.map(i => p.get(i).getOrElse(i))

def inverse(p: Perm): Perm =
  p.zipWithIndex.sortBy(_._1).map(_._2)

def shuffle(numbers: Vector[Long])(p: Perm, i: Int): Perm =
  val s = p(i)
  val n = (numbers(inverse(p)(s)) + s) % (numbers.size - 1) - s match
    case x if s + x < 0 => (x + numbers.size - 1).toInt
    case x => x.toInt
  if (n % numbers.size) == 0 then p
  else
    val e = s + n
    val r = (s to e by (e - s).sign)
    val q = r.map { j =>
      j -> ((j + n - s) % r.size + s)
    }
    compose(q.toMap.withDefault(n => n), p)

def permute[T](numbers: Vector[T], p: Perm): Vector[T] =
  numbers.zipWithIndex
    .sortBy((n, i) => p(i))
    .map((n, i) => n)
    .toVector

def mix(numbers: Vector[Long]): Vector[Long] =
  val p = numbers.indices.foldLeft(id)(shuffle(numbers))
  permute(numbers, p)

val key = 811589153L

val decrypted = numbers.map(_.toLong * key)

// val mixingPerm = LazyList.iterate(id -> id) {
//   case (p, q) =>
//     val r = q.foldLeft(p)(shuffle(decrypted))
//     r -> inverse(q)
// }(10)._1

// val mixed = permute(decrypted, mixingPerm)

// val it = Iterator.continually(mixed).flatten
//   .drop(mixed.indexOf(0) + 1)

// val a = it.drop(999).next()
// val b = it.drop(999).next()
// val c = it.drop(999).next()

// val ans = a + b + c
// 7973051839072
