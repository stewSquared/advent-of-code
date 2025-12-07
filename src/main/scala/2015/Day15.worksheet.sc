val input = io.Source.fromResource("2015/day-15.txt").getLines().toList

val ingredients: Vector[Vector[Long]] = input.toVector.collect:
  case s"$name: capacity $c, durability $d, flavor $f, texture $t, calories $cal" =>
    Vector(c.toLong, d.toLong, f.toLong, t.toLong, cal.toLong)

def score(amounts: Long*): Long =
  assert(amounts.size == 4)
  assert(amounts.sum == 100)
  val scaled = amounts.zip(ingredients).map:
    case (n, props) => props.init.map(_ * n)
  val props = scaled.transpose.map(_.sum).map:
    case n if n < 0 => 0
    case n => n
  props.product

val proportions = for
  a <- 0L to 100L
  b <- 0L to (100L - a)
  c <- 0L to (100L - a - b)
  d = 100L - a - b - c
yield List(a,b,c,d)

def calories(amounts: Long*): Long =
  val cals = ingredients.transpose.last
  amounts.zip(cals).map(_ * _).sum

val props = proportions.maxBy(p => score(p*))

val ans1 = score(props*)
val ans2 = proportions.collect:
  case ps if calories(ps*) == 500 => score(ps*)
.max
// 13324953600 too high


// score(24, 24, 26, 26)
// score(60, 10, 15, 15)
// score(1,1,1,97)
// score(1,1,49,49)
// score(40, 40, 10, 10)


// val ingredients = input.collect:
//   case s"$name: capacity $c, durability $d, flavor $f, texture $t, calories $cal" =>
//     Ingredient(
//       capacity = c.toInt,
//       durability = d.toInt,
//       flavor = f.toInt,
//       texture = t.toInt,
//       calories = cal.toInt
//     )

// List(sprinkles, butterscotch, chocolate, candy) = ingredients

// def score(
//   sp: Int,
//   bu: Int,
//   ch: Int,
//   ca: Int
// ): Int =



//
