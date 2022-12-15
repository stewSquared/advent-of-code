val lines = io.Source.fromResource("2022/day-11.txt").getLines().toVector

val items: Vector[Vector[Long]] = lines.collect { case s"$_ items: $xs" =>
  xs.split(", ").map(_.toLong).toVector
}

val divTests = lines.collect { case s"$_ divisible by $p" => p.toInt }

val lcm = divTests.product

val worryOp: Vector[Long => Long] = lines.collect {
  case s"$_ new = old * old" => o => o * o % lcm
  case s"$_ new = old * $x"  => o => o * x.toInt % lcm
  case s"$_ new = old + $x"  => o => o + x.toInt % lcm
}

val throwTo: Vector[Long => Int] =
  val ifTrue = lines.collect { case s"$_ true: $_ monkey $n" => n.toInt }
  val ifFalse = lines.collect { case s"$_ false: $_ monkey $n" => n.toInt }

  divTests.lazyZip(ifTrue).lazyZip(ifFalse).map { case (d, t, f) =>
    (w: Long) => if w % d == 0 then t else f
  }

def turnCounts(worryMore: Boolean) =
  Iterator.iterate((0, items, Vector.fill(items.size)(0L))) {
    case (m, items, counts) =>
      val nextItems = items(m)
        .foldLeft(items.updated(m, Vector.empty)) { (items, w) =>
          val newWorry = worryOp(m)(w) / (if worryMore then 1 else 3)
          val m2 = throwTo(m)(newWorry)
          items.updated(m2, items(m2).appended(newWorry))
        }
      val nextCounts = counts.updated(m, counts(m) + items(m).size)
      ((m + 1) % items.size, nextItems, nextCounts)
  }

def roundCount(rounds: Int, worryMore: Boolean): Vector[Long] =
  turnCounts(worryMore).drop(items.size * rounds).next()._3

val ans1 = roundCount(20, false).sorted.takeRight(2).product

val ans2 = roundCount(10000, true).sorted.takeRight(2).product
