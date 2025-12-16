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

  val start = VMState(
    pc = Adr.fromInt(0),
    registers = Registers.init,
    stack = Nil,
    memory
  )

  Iterator.unfold(start)(_.step).flatten.foreach: ch =>
    print(ch)
    Thread.sleep(100)

  println

  //
