val input = io.Source.fromResource("2015/day-05.txt").getLines().toList

val vowels = "aeiou".toSet
val badStrings = List("ab", "cd", "pq", "xy")

def nice(s: String) =
  val threeVowels = s.count(vowels) >= 3
  val duplicate = s.sliding(2).exists(p => p.head == p.last)
  val noBadStrings = !badStrings.exists(s.contains)
  threeVowels && duplicate && noBadStrings

input.count(nice)

nice("aaa")
nice("ugknbfddgicrmopn")

def pairAppearsTwice(s: String): Boolean =
  val appearances: Map[String, List[Int]] =
    s.sliding(2).zipWithIndex.toList.groupMap((pair, index) => pair)(_._2)

  appearances.exists:
    case (pair, indices) =>
      indices.sizeIs > 2 || (
        indices.sizeIs == 2 && (indices.head - indices.last).abs != 1
      )

def repeatWithGap(s: String) =
  s.sliding(3).exists:
    s => s.head == s.last

pairAppearsTwice("aaa")
pairAppearsTwice("aaaa")
pairAppearsTwice("xyaaaaaxy")

val ans2 = input.count(s => pairAppearsTwice(s) && repeatWithGap(s))

//
