import aoc.{Area, Point, Dir}

val secrets = io.Source.fromResource("2024/day-22.txt").getLines
  .toList.map(_.toLong)

import util.chaining.*

def next(secret: Long): Long =
  secret.mix(secret * 64).prune.pipe: secret =>
    secret.mix(secret / 32).prune.pipe: secret =>
      secret.mix(secret * 2048).prune

extension(secret: Long)
  def mix(value: Long): Long = value ^ secret
  def prune: Long = secret % 16777216

def last(secret: Long) = Iterator.iterate(secret)(next).drop(2000).next

def prices(secret: Long) = Iterator.iterate(secret)(next)
  .map[Int](p => (p % 10).toInt)

def deltas(secret: Long) = prices(secret).sliding(2).map:
  case Seq(a, b) => b - a


def index(secret: Long): Map[(Int, Int, Int, Int), Int] =
  val deltaTups = deltas(secret).take(2000).sliding(4).map:
    case Seq(a,b,c,d) => (a,b,c,d)

  deltaTups.zip(prices(secret).drop(4))
    .toList
    .groupMapReduce(_._1)(_._2)((a, b) => a)

val ans1 = secrets.map(last).sum

val fullIndex =
  val start = Map.empty[(Int, Int, Int, Int), Int].withDefaultValue(0)
  secrets.map(index).foldLeft(start):
    (acc, i) =>
      i.foldLeft(acc):
        case (acc, (k, v)) => acc.updated(k, v + acc(k))

val ans2 = fullIndex.values.max
