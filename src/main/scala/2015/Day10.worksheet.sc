val input = "1321131112"

def lookSay(look: String): String =
  var head = look.head
  val digits = look.iterator.buffered
  val say = collection.mutable.StringBuilder()
  var count = 0
  while digits.hasNext do
    val next = digits.next()
    if next == head then
      count += 1
    else
      say ++= count.toString
      say += head
      head = next
      count = 1
  say ++= count.toString
  say += head
  say.toString()

lookSay(input)

val ans1 = Iterator.iterate(input)(lookSay).drop(40).next().length
val ans2 = Iterator.iterate(input)(lookSay).drop(50).next().length
