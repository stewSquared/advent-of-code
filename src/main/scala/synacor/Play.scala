package synacor
import synacor.*
import synacor.numbers.*
import synacor.VMState

@main def play(): Unit =
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
    pc = 0.toAdr,
    registers = Registers.init,
    stack = Nil,
    memory = memory,
  )


  var emu: Emulator = Emulator.init(Tick.Continue(startVM))

  val shortcut = List("doorway", "north", "north", "bridge", "continue", "down", "east", "take empty lantern", "west", "west", "west", "passage", "ladder", "west", "south", "north", "take can", "use can", "use lantern", "west", "ladder", "darkness", "continue", "west", "west", "west", "west", "north", "take red coin", "north", "west", "take blue coin", "up", "take shiny coin", "down", "east", "east", "take concave coin", "down", "take corroded", "take corroded coin", "up", "west", "inv", "use blue coin", "use red coin", "use shiny coin", "use concave coin", "use corroded coin", "north", "teleporter", "take teleporter")
  emu = emu.feedMultiple(shortcut.map(_ + '\n'))
  emu = emu.progressUntilBlocked.copy(outputQueue = collection.immutable.Queue.empty)
  emu = emu.toggleOplog
  emu = emu.feed("use teleporter\n")
  emu = emu.clearOplog
  emu = emu.progressUntilBlocked.useOutput(println)

  extension (a: Adr)
    def nextInstruction(op: Opcode) = op.numParams match
      case 0 => a.inc1
      case 1 => a.inc2
      case 2 => a.inc3
      case 3 => a.inc4

  extension (memory: Memory)
    def extractFunction(adr: Adr, ret: Int): List[(Adr, Inst)] =
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

  def show(pair: (Adr, Inst)): String =
    val (adr, inst) = pair
    s"@${adr.hex}: $inst"

  def printExtract(adr: Adr, ret: Int = 0): Unit =
    println(s"@${adr.hex}: CALL")
    startVM.memory.extractFunction(adr, ret = ret).map(show) foreach println
    println

  printExtract(0x084D.toAdr) // R1 = XOR(R1, R2)
  printExtract(0x05B2.toAdr)
  printExtract(0x0683.toAdr)
  printExtract(0x0623.toAdr) //
  printExtract((0x154B+3).toAdr)
  printExtract(0x178B.toAdr, ret = 2) // ???
  printExtract(0x0731.toAdr) // ???
  printExtract(0x1721.toAdr) // ???
  printExtract(0x0607.toAdr) // ???

  // CALL(R3)
  printExtract(0x1135.toAdr) // from - 9 in vault
  printExtract(0x08C8.toAdr) // from - 9 in vault
  printExtract(0x08E9.toAdr) // from - 9 in vault
  printExtract(0x11A3.toAdr) // from - 9 in vault

// @0x1135: CALL
// @0x1135: PUSH(R1)
// @0x1137: PUSH(R2)
// @0x1139: PUSH(R3)
// @0x113B: PUSH(R4)
// @0x113D: PUSH(R5)
// @0x113F: PUSH(R6)
// @0x1141: RMEM(R6, 0x0F71)
// @0x1144: GT(R4, R6, 0x752F)
// @0x1148: JT(R4, 0x1152)
// @0x114B: ADD(R6, R6, 0x0001)
// @0x114F: WMEM(0x0F71, R6)
// @0x1152: SET(R4, R1)
// @0x1155: SET(R5, R2)
// @0x1158: ADD(R1, R6, 0x0002)
// @0x115C: CALL(0x08E9)
// @0x115E: RMEM(R2, 0x0F72)
// @0x1161: OR(R1, R2, R1)
// @0x1165: SET(R2, R5)
// @0x1168: CALL(0x08C8)
// @0x116A: WMEM(0x0F72, R1)
// @0x116D: SET(R1, 0x0F73)
// @0x1170: ADD(R2, R6, 0x0002)
// @0x1174: SET(R3, R5)
// @0x1177: CALL(0x11A3)
// @0x1179: SET(R1, 0x0F74)
// @0x117C: MULT(R2, R6, R6)
// @0x1180: MULT(R3, R5, R5)
// @0x1184: CALL(0x11A3)
// @0x1186: SET(R1, 0x0F75)
// @0x1189: SET(R2, 0x000D)
// @0x118C: MULT(R3, R4, 0x0009)
// @0x1190: MULT(R3, R3, R3)
// @0x1194: CALL(0x11A3)
// @0x1196: POP(R6)
// @0x1198: POP(R5)
// @0x119A: POP(R4)
// @0x119C: POP(R3)
// @0x119E: POP(R2)
// @0x11A0: POP(R1)

// @0x08C8: CALL
// @0x08C8: PUSH(R2)
// @0x08CA: PUSH(R3)
// @0x08CC: JF(R2, 0x08E4)
// @0x08CF: ADD(R2, R2, 0x7FFF)
// @0x08D3: AND(R3, R1, 0x4000)
// @0x08D7: MULT(R1, R1, 0x0002)
// @0x08DB: JF(R3, 0x08CC)
// @0x08DE: OR(R1, R1, 0x0001)
// @0x08E2: JMP(0x08CC)
// @0x08E4: POP(R3)
// @0x08E6: POP(R2)

// @0x08E9: CALL
// @0x08E9: PUSH(R2)
// @0x08EB: GT(R2, R1, 0x000E)
// @0x08EF: JT(R2, 0x0905)
// @0x08F2: SET(R2, R1)
// @0x08F5: SET(R1, 0x0001)
// @0x08F8: JF(R2, 0x0908)
// @0x08FB: ADD(R2, R2, 0x7FFF)
// @0x08FF: MULT(R1, R1, 0x0002)
// @0x0903: JMP(0x08F8)
// @0x0905: SET(R1, 0x7FFF)
// @0x0908: POP(R2)

// @0x11A3: CALL
// @0x11A3: PUSH(R1)
// @0x11A5: RMEM(R1, R1)
// @0x11A8: CALL(0x08C8)
// @0x11AA: SET(R2, R3)
// @0x11AD: CALL(0x084D)
// @0x11AF: POP(R2)
// @0x11B1: WMEM(R2, R1)
