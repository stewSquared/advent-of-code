import synacor.*
import synacor.numbers.*

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
  stack = Stack.init,
  memory = memory,
)

startVM.showInst
// startVM.tick.state.show

var emu: Emulator = Emulator.init(Tick.Continue(startVM))

val shortcut = List("doorway", "north", "north", "bridge", "continue", "down", "east", "take empty lantern", "west", "west", "west", "passage", "ladder", "west", "south", "north", "take can", "use can", "use lantern", "west", "ladder", "darkness", "continue", "west", "west", "west", "west", "north", "take red coin", "north", "west", "take blue coin", "up", "take shiny coin", "down", "east", "east", "take concave coin", "down", "take corroded", "take corroded coin", "up", "west", "inv", "use blue coin", "use red coin", "use shiny coin", "use concave coin", "use corroded coin", "north", "teleporter", "take teleporter")
emu = emu.feedMultiple(shortcut.map(_ + '\n'))
emu = emu.progressUntilBlocked.copy(outputQueue = collection.immutable.Queue.empty)
emu = emu.setRegister(numbers.Reg.R8, 100.toLit)
val toVault = List("use teleporter", "north", "north", "north", "north", "north", "north", "north", "north", "north")
emu = emu.feedMultiple(toVault.map(_ + '\n'))
emu = emu.progressUntilBlocked.copy(outputQueue = collection.immutable.Queue.empty)
emu = emu.feed("look\n").progressUntilBlocked
println(emu.outputQueue.dequeue._1)
emu = emu.useOutput(println)

val log1 = emu
  .feed("take orb\n").progressUntilBlocked
  .feed("go east\n").progressUntilBlocked.copy(outputQueue = collection.immutable.Queue.empty)
  .toggleOplog
  .feed("go east\n").progressUntilBlocked
  .oplog

val log2 = emu
  .feed("go east\n").progressUntilBlocked.copy(outputQueue = collection.immutable.Queue.empty)
  .toggleOplog
  .feed("go east\n").progressUntilBlocked
  .oplog

emu
  .feed("take orb\n").progressUntilBlocked
  .feed("go east\n").progressUntilBlocked.copy(outputQueue = collection.immutable.Queue.empty)
  .toggleOplog
  .feed("go east\n").progressUntilBlocked
  .outputQueue.dequeue

emu
  .feed("go east\n").progressUntilBlocked.copy(outputQueue = collection.immutable.Queue.empty)
  .toggleOplog
  .feed("go east\n").progressUntilBlocked
  .outputQueue.dequeue

log1.size
log2.size

log1.diff(log2).size

val c1 = log1.filter(_._1.op == Opcode.CALL)
val c2 = log2.filter(_._1.op == Opcode.CALL)
c1.size
c2.size

val counts1 = c1.groupMapReduce(_._1)(_ => 1)(_ + _)
val counts2 = c2.groupMapReduce(_._1)(_ => 1)(_ + _)

counts1.toList.sortBy(_._2).reverse foreach println
counts2.toList.sortBy(_._2).reverse foreach println

counts1.keySet.diff(counts2.keySet) foreach println

extension (a: Adr)
  def nextInstruction(op: Opcode) = op.numParams match
    case 0 => a.inc1
    case 1 => a.inc2
    case 2 => a.inc3
    case 3 => a.inc4

extension (memory: Memory)
  def extractFunction(adr: Adr): List[Inst] =
    def loop(adr: Adr): List[Inst] =
      val op = Opcode.parse(memory(adr))
      def a = memory(adr.inc1)
      def b = memory(adr.inc2)
      def c = memory(adr.inc3)
      val inst = Inst.parse(op, a, b, c)

      inst match
        case Inst.RET => Nil
        case inst => inst :: loop(adr.nextInstruction(op))
    loop(adr)

startVM.memory.extractFunction(0x1721.toAdr) foreach println

// 0x05B2

// emu = emu.feed("west\n").progressUntilBlocked
// emu = emu.feed("west\n").progressUntilBlocked
// emu = emu.feed("north\n").progressUntilBlocked
// emu = emu.feed("east\n").progressUntilBlocked
// val s1 = emu.state match
//   case Tick.Halt(code) => ???
//   case Tick.Output(c, state) => ???
//   case Tick.Input(f) => f(' ')
//   case Tick.Continue(state) => ???

// val s2 = emu2
//   .feed("west\n").progressUntilBlocked
//   .feed("west\n").progressUntilBlocked
//   .state match
//     case Tick.Halt(code) => ???
//     case Tick.Output(c, state) => ???
//     case Tick.Input(f) => f(' ')
//     case Tick.Continue(state) => ???

// val mem1 = s1.memory
// val mem2 = s2.memory

// s1.registers == s2.registers // TODO fix register bank equality
// s1.registers.underlying.mkString(",") == s2.registers.underlying.mkString(",")


// mem1 == mem2
// mem1.underlying.zip(mem2.underlying).indexWhere(_ != _)
// 2732
// mem1.underlying.zip(mem2.underlying).indexWhere(_ != _, 2732 + 1)
2732.toHexString

//
