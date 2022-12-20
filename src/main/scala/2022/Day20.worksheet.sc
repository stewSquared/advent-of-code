var numbers =
  io.Source.fromResource("2022/day-20.txt").getLines().map(_.toInt).toVector

// numbers = Vector(1, 2, -3, 3, -2, 0, 4)

type Perm = Array[Int]
val id: Perm = numbers.indices.toArray

def compose(p: Map[Int, Int], q: Perm): Perm =
  q.map(i => p.get(i).getOrElse(i))

def inverse(p: Perm): Perm =
  p.zipWithIndex.sortBy(_._1).map(_._2)

def shuffle(p: Perm, i: Int): Perm =
  val s = p(i)
  val n = (numbers(inverse(p)(s)) + s) % (numbers.size - 1) - s match
    case x if s + x < 0 => x + numbers.size - 1
    case x => x
  if (n % numbers.size) == 0 then p
  else
    val e = s + n
    val r = (s to e by (e - s).sign)
    val q = r.map { j =>
      j -> ((j + n - s) % r.size + s)
    }
    compose(q.toMap.withDefault(n => n), p)

def permute(numbers: Vector[Int], p: Perm): Vector[Int] =
  numbers.zipWithIndex
    .sortBy((n, i) => p(i))
    .map((n, i) => n)
    .toVector

def mix(numbers: Vector[Int]): Vector[Int] =
  val p = numbers.indices.foldLeft(id)(shuffle)
  permute(numbers, p)

val mixed = mix(numbers)

val it = Iterator.continually(mixed).flatten
  .drop(mixed.indexOf(0) + 1)

val a = it.drop(999).next()
val b = it.drop(999).next()
val c = it.drop(999).next()

val ans = a + b + c
