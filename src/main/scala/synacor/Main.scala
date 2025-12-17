package synacor

import numbers.*

@main def main(): Unit =
  def resource = Thread.currentThread()
    .getContextClassLoader
    .getResourceAsStream("synacor/challenge.bin")

  val memory = util.Using(resource): is =>
    val bytes = is.readAllBytes()
    assert(bytes.size % 2 == 0)
    assert(bytes.sizeIs <= 0x10000)
    val a = Array.ofDim[Word](0x8000)

    for i <- bytes.indices by 2 do
      a(i / 2) = Word.fromBytes(bytes(i), bytes(i + 1))

    Memory(a.toVector)
  .get

  val start = VMState[Ready](
    pc = Adr.fromInt(0),
    registers = Registers.init,
    stack = Nil,
    memory = memory,
    input = """|go doorway
               |go north
               |go north
               |go bridge
               |continue
               |go down
               |go east
               |take empty lantern
               |go west
               |go west
               |go west
               |go passage
               |go ladder
               |go west
               |go south
               |go north
               |look can
               |take can
               |use can
               |go west
               |go ladder
               |use lantern
               |go darkness
               |continue
               |go west
               |go west
               |go west
               |go west
               |go north
               |look red coin
               |take red coin
               |go north
               |""".stripMargin.toList
  )


  var col = 0
  Iterator.unfold(start)(_.step).flatten.foreach: ch =>
    if ch == '\n' then col = 0
    if col == 80 then
      col = 0
      println

    col += 1
    print(ch)
    // Thread.sleep(1)

  println("Synacor VM Halted")
