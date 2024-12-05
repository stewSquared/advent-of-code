val polymer = io.Source.fromResource("2018/day-05.txt").getLines().mkString

polymer.size

def react(polymer: String): String =
  ('a' to 'z').foldLeft(polymer):
    case (acc, c) =>
      acc.replaceAll(s"$c${c.toUpper}|${c.toUpper}$c", "")

def fullyReacted(polymer: String) = Iterator
  .iterate(polymer)(react)
  .sliding(2)
  .collectFirst:
    case Seq(a, b) if a.size == b.size => a
  .get

val ans1 = fullyReacted(polymer).size

def remove(unit: Char, polymer: String): String =
  polymer.filterNot(_.toLower == unit)

// Warning: 10 second runtime
// val ans2 = ('a' to 'z')
//   .map(remove(_, polymer))
//   .map(fullyReacted)
//   .map(_.size)
//   .min
// 4956
