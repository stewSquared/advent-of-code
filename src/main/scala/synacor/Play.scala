package synacor
import synacor.*
import synacor.numbers.*
import synacor.VMState

@main def play(): Unit =
  println("")
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

  val startVM = VMState[Ready](
    pc = Adr.fromInt(0),
    registers = Registers.init,
    stack = Nil,
    memory = memory,
  )


  var emu: Emulator = Emulator.init(Tick.Continue(startVM))

  val shortcut = List("doorway", "north", "north", "bridge", "continue", "down", "east", "take empty lantern", "west", "west", "west", "passage", "ladder", "west", "south", "north", "take can", "use can", "use lantern", "west", "ladder", "darkness", "continue", "west", "west", "west", "west", "north", "take red coin", "north", "west", "take blue coin", "up", "take shiny coin", "down", "east", "east", "take concave coin", "down", "take corroded", "take corroded coin", "up", "west", "inv", "use blue coin", "use red coin", "use shiny coin", "use concave coin", "use corroded coin", "north", "teleporter", "take teleporter")
  emu = emu.feedMultiple(shortcut.map(_ + '\n'))
  emu = emu.progressUntilBlocked.copy(outputQueue = collection.immutable.Queue.empty)
  emu = emu.toggleOplog
  emu.oplogEnabled
  // emu = emu.setRegister(Reg.R8, U15.fromInt(42))
  emu = emu.feed("use teleporter\n")
  emu.oplog.size
  // emu.oplog foreach println
  emu = emu.clearOplog
  emu = emu.progressUntilBlocked.useOutput(println)
  emu.oplog.size
  // emu.oplog.map(_.swap) foreach println
  emu.oplog.map(_._2).count(_.contains("R8"))
  // emu.oplog.map(_._2).filter(_.contains("R8")) foreach println
  emu.oplog.map(_._1).count(_.op == Opcode.CALL)
  // emu.oplog.map(_._1).filter(_.op == Opcode.CALL) foreach println
  // val callCounts = emu.oplog.map(_._1).filter(_.op == Opcode.CALL).groupMapReduce(identity)(_ => 1)(_ + _)
  // callCounts.toList.sortBy(-_._2) foreach println
  // 2125.adr

  given Emulator.HackPerm = Emulator.HackPerm

  startVM.modifyPC(_ => Adr.fromInt(0x084D)).showInst
  startVM.memory.apply(Adr.fromInt(2125)).hex

  extension (a: Adr)
    def nextInstruction(op: Opcode) = op.numParams match
      case 0 => a.inc1
      case 1 => a.inc2
      case 2 => a.inc3
      case 3 => a.inc4

  extension (memory: Memory)
    def extractFunction(adr: Adr, ret: Int = 0): List[(Adr, Inst)] =
      def loop(adr: Adr, ret: Int): List[(Adr, Inst)] =
        val op = memory(adr).op
        def a = memory(adr.inc1)
        def b = memory(adr.inc2)
        def c = memory(adr.inc3)
        val inst = Inst.parse(op, a, b, c)

        inst match
          case Inst.RET if ret == 0 => Nil
          case Inst.RET => (adr -> Inst.RET) :: loop(adr.nextInstruction(op), ret - 1)
          case inst => (adr -> inst) :: loop(adr.nextInstruction(op), ret)
      loop(adr, ret)

  def show(pair: (Adr, Inst)): Unit = println:
    val (adr, inst) = pair
    s"@${adr.hex}: $inst"

  println("0x084D function:")
  startVM.memory.extractFunction(Adr.fromInt(0x084D)) foreach show // R1 = XOR(R1, R2)
  println("0x05B2 function:")
  startVM.memory.extractFunction(Adr.fromInt(0x05B2)) foreach show
  println("0x0683 function:")
  startVM.memory.extractFunction(Adr.fromInt(0x0683)) foreach show
  println("0x0623 function:")
  startVM.memory.extractFunction(Adr.fromInt(0x0623)) foreach show //
  println("0x154B+3 function: runs after failing JF")
  startVM.memory.extractFunction(Adr.fromInt(0x154B+3)) foreach show
  println("0x178B function: from infinite loop")
  startVM.memory.extractFunction(Adr.fromInt(0x178B), ret = 2) foreach show // ???
  println("0x0731 function:")
  startVM.memory.extractFunction(Adr.fromInt(0x0731)) foreach show // ???
