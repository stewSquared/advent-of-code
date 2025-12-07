val input = io.Source.fromResource("2015/day-16.txt").getLines().toList

val forensics = Map(
  "children" -> 3,
  "cats" -> 7,
  "samoyeds" -> 2,
  "pomeranians" -> 3,
  "akitas" -> 0,
  "vizslas" -> 0,
  "goldfish" -> 5,
  "trees" -> 3,
  "cars" -> 2,
  "perfumes" -> 1,
)

val aunts = input.collect:
  case s"Sue $id: $things" =>
    id -> things.split(',').map(_.strip()).toList.collect:
      case s"$thing: $n" => thing -> n.toInt

val matches = aunts.filter:
  case (id, things) => things.forall:
    case (thing, n) if forensics.contains(thing) => forensics(thing) == n
    case _ => true

val ans1 = matches.head._1

val matches2 = aunts.filter:
  case (id, things) => things.forall:
    case (thing, n) if !forensics.contains(thing) => true
    case (thing@("cats" | "trees"), n)           => forensics(thing) < n
    case (thing@("pomeranians" | "goldfish"), n) => forensics(thing) > n
    case (thing, n)                              => forensics(thing) == n

matches2.size
val ans2 = matches2.head._1
