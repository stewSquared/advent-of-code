package synacor

import collection.immutable.Queue

// TODO alternate between in/out types
// TODO: Maybe progress from VMState to state, rather than tick to tick
// history should be VMState
case class Emulator(
  state: Tick,
  history: List[Tick],
  outputQueue: Queue[String],
  inputHistory: Queue[String],
  oplog: Queue[Inst],
  oplogEnabled: Boolean
):
  def feedMultiple(inputs: List[String]): Emulator =
    inputs.foldLeft(this):
      case (state, input) =>
        state.progressUntilBlocked.feed(input)

  def toggleOplog: Emulator = copy(oplogEnabled = !oplogEnabled)

  def clearOplog: Emulator = copy(oplog = Queue.empty)

  def feed(input: String): Emulator =
    assert(input.indexOf('\n') == input.length - 1)

    def loop(tick: Tick, chars: List[Char], ops: Queue[Inst] = Queue.empty): (Tick, Queue[Inst]) = tick match
      case Tick.Continue(state) =>
        if oplogEnabled then
          loop(state.tick, chars, ops.enqueue(state.inst))
        else
          loop(state.tick, chars)
      case Tick.Input(f) => chars match
        case c::cs => loop(f(c).tick, cs)
        case Nil => throw new Exception("Not enough input.") // dead code
      case tick => if chars.isEmpty then (tick, ops) else
        throw new Exception(s"Input too long.") // dead code

    val (next, ops) = loop(state, input.toList)
    copy(state = next, inputHistory = inputHistory.enqueue(input), oplog = oplog.enqueueAll(ops))

  def progressUntilBlocked: Emulator =
    def loop(tick: Tick, chars: Queue[Char], ops: Queue[Inst] = Queue.empty): (Tick, String, Queue[Inst]) = tick match
      case Tick.Continue(state) =>
        val log = if oplogEnabled then ops.enqueue(state.inst) else ops
        loop(state.tick, chars, log)
      case Tick.Output(c, state) =>
        val log = if oplogEnabled then ops.enqueue(state.inst) else ops
        loop(state.tick, chars.enqueue(c), log)
      case _ => (tick, chars.mkString, ops)

    val (next, out, ops) = loop(state, Queue.empty)
    assert(next.isBlocked)
    copy(next, history = state::history, outputQueue = outputQueue.enqueue(out), oplog = oplog.enqueueAll(ops))

  def useOutput(f: String => Unit): Emulator =
    outputQueue.dequeueOption.fold(this):
      case (out, outs) =>
        f(out)
        copy(outputQueue = outs)

  def undo: Emulator = history match
    case _ :: past :: olderHistory =>
      copy(state = past, history = olderHistory)
      // TODO: drop command history
    case _ => this

  def setRegisters(f: Registers => Registers): Emulator = state match
    case tick: Tick.Input =>
      val setRegisters: VMState[Ready] => VMState[Ready] = vm =>
        vm.copy(registers = f(vm.registers))
      val newTick = Tick.Input(tick.f andThen setRegisters)
      this.copy(state = newTick)
    case tick: Tick.Continue =>
      val newTick = tick.copy(state = tick.state.copy(registers = f(tick.state.registers)))
      this.copy(state = newTick)
    case tick: Tick.Output =>
      val newTick = tick.copy(state = tick.state.copy(registers = f(tick.state.registers)))
      this.copy(state = newTick)
    case _ => ???

  def getRegister(reg: numbers.Reg): numbers.U15 = state match
    case tick: Tick.Input => tick.f(' ').registers(reg) // TODO is hack
    case _ => ???

object Emulator:
  def init(start: Tick): Emulator =
    apply(start, Nil, Queue.empty, Queue.empty, Queue.empty, false)
