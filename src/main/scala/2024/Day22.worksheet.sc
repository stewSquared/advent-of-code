import util.chaining.*

val secrets = io.Source.fromResource("2024/day-22.txt").getLines
  .toList.map(_.toLong)

extension(secret: Long)
  inline def mix(f: Long => Long): Long = f(secret) ^ secret
  inline def prune: Long = secret % 16777216

def next(secret: Long): Long =
  secret.mix(_ * 64).prune
    .pipe(_.mix(_ / 32).prune)
    .pipe(_.mix(_ * 2048).prune)

def last(secret: Long) = Iterator.iterate(secret)(next).drop(2000).next

val ans1 = secrets.map(last).sum

def prices(secret: Long) = Iterator.iterate(secret)(next)
  .map[Int](p => (p % 10).toInt)

def deltas(secret: Long) = prices(secret).sliding(2).map:
  case Seq(a, b) => b - a

def index(secret: Long): Map[(Int, Int, Int, Int), Int] =
  val deltaTups = deltas(secret).take(2000).sliding(4).map:
    case Seq(a,b,c,d) => (a,b,c,d)

  deltaTups.zip(prices(secret).drop(4)).toList
    .groupMapReduce(_._1)(_._2)((a, b) => a)

val fullIndex =
  val start = Map.empty[(Int, Int, Int, Int), Int].withDefaultValue(0)
  secrets.map(index).foldLeft(start): (acc, i) =>
    i.foldLeft(acc):
      case (acc, (k, v)) => acc.updated(k, v + acc(k))

val ans2 = fullIndex.values.max
