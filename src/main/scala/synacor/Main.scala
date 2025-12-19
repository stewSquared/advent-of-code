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
  emu = emu.feedMultiple(shortcut.map(_ + '\n')).copy(outputQueue = collection.immutable.Queue.empty)
  emu = emu.progressUntilBlocked
  emu = emu.feed("look\n")

  while true do
    emu = emu.progressUntilBlocked
    emu = emu.useOutput(typeln) // TODO: Format
    io.StdIn.readLine() match
      case "/halt" => ???
      case "/undo" | "/oops" => emu = emu.undo
      case "/inputlog" => println(emu.inputHistory.map(_.strip).mkString("List(\"","\", \"", "\")"))
      case s"/get R8" =>
        val word = emu.getRegister(numbers.Reg.R8)
        typeln(s"Register 8 value is: ${word.hex}")
      case s"/set R${i} $n" => i.toInt match
        case 8 =>
          import numbers.*
          // val lit = n.toInt.toLit
          val word = Word.fromInt(n.toInt)
          emu = emu.setRegisters(_.updated(Reg.R8, word))
          // emu = emu.feed("look\n")
        case _ => ???
      case command =>
        emu = emu.feed(s"${command}\n")

  println("Synacor VM Halted")
