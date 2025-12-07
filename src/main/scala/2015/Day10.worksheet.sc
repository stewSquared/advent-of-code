val input = "1321131112"

def lookSay(look: String): String =
  val n = look.length
  val pairs = Iterator.unfold(0):
    case (start) => Option.when(start < n):
      val head = look.charAt(start)
      val end = look.indexWhere(_ != head, start + 1) match
        case -1 => n
        case i => i
      val count = end - start
      (count, head) -> end

  val say = StringBuilder(look.length * 2)
  for (count, head) <- pairs do
    say ++= count.toString
    say += head
  say.result()

lookSay(input)

val ans1 = Iterator.iterate(input)(lookSay).drop(40).next().length
val ans2 = Iterator.iterate(input)(lookSay).drop(50).next().length
