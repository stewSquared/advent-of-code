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

  val start = Tick.Continue:
    VMState[Ready](
      pc = Adr.fromInt(0),
      registers = Registers.init,
      stack = Nil,
      memory = memory,
    )

  var emu: Emulator = Emulator(start, Nil)

  println("starting emulation...")
  // if !emu.isRunning then emu else
  while true do
    val (out, next) = emu.nextOut
    println(out) // TODO formatting
    val in = io.StdIn.readLine()
    in match
      case "!halt" => ???
      case "!rewind" => ???
      case s"!set R${n}" => ??? // loop(emu.setRegister(n.toInt))
      case command =>
        emu = next.feed(s"${command}\n")

  println("Synacor VM Halted")
