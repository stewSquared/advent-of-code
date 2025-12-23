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

  var emu: Emulator = Emulator.init:
    Tick.Continue:
      VMState[Ready](
        pc = Adr.fromInt(0),
        registers = Registers.init,
        stack = Nil,
        memory = memory,
      )

  def typeln(string: String): Unit =
    var col = 0
    string.foreach: ch =>
      print(ch)
      col += 1
      if col > 80 then
        print('\n'); col = 0
      if ch == '\n' then col = 0
      scala.Console.flush()
      Thread.sleep(5)
    println()



  println("starting emulation...")
  // if !emu.isRunning then emu else

  val shortcut = List("doorway", "north", "north", "bridge", "continue", "down", "east", "take empty lantern", "west", "west", "west", "passage", "ladder", "west", "south", "north", "take can", "use can", "use lantern", "west", "ladder", "darkness", "continue", "west", "west", "west", "west", "north", "take red coin", "north", "west", "take blue coin", "up", "take shiny coin", "down", "east", "east", "take concave coin", "down", "take corroded", "take corroded coin", "up", "west", "inv", "use blue coin", "use red coin", "use shiny coin", "use concave coin", "use corroded coin", "north", "teleporter", "take teleporter")
  emu = emu.feedMultiple(shortcut.map(_ + '\n'))
  emu = emu.progressUntilBlocked.copy(outputQueue = collection.immutable.Queue.empty)
  emu = emu.feed("look\n")

  while true do
    emu = emu.progressUntilBlocked
    emu = emu.useOutput(typeln) // TODO: Format
    io.StdIn.readLine() match
      case "/halt" => ???
      case "/undo" | "/oops" => emu = emu.undo
      case "/hist" => emu.inputHistory.map(_.strip).foreach(typeln)
      case "/oplog toggle" =>
        emu = emu.toggleOplog
        typeln(s"oplog enabled: ${emu.oplogEnabled}")
      case "/oplog show" =>
        emu.oplog.foreach(println)
      case "/oplog clear" => emu = emu.clearOplog
        typeln("cleared oplog")
      case "/oplog size" =>
        typeln(s"${emu.oplog.size} instructions ran")
      case "/oplog hash" =>
        typeln(s"${emu.oplog.hashCode()}")
      case s"/get R${i}" =>
        val r = Reg.fromIndex(i.toInt)
        val word = emu.getRegister(r)
        typeln(s"Register $r value is: ${word.hex}")
      case s"/set R${i} $n" =>
        val r = Reg.fromIndex(i.toInt)
        val v = n.toInt.toU15
        emu = emu.setRegister(r, v)
        typeln(s"Set Register $r to value: ${v.hex}")
      case command =>
        emu = emu.feed(s"${command}\n")

  println("Synacor VM Halted")
