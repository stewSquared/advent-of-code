package synacor

import collection.immutable.Queue

// TODO alternate between in/out types
case class Emulator(state: Tick, history: List[Tick], outputQueue: Queue[String], inputHistory: Queue[String], oplog: Queue[(Inst, String)], oplogEnabled: Boolean):
  given perm: Emulator.HackPerm = Emulator.HackPerm

  def feedMultiple(inputs: List[String]): Emulator =
    inputs.foldLeft(this):
      case (state, input) =>
        state.progressUntilBlocked.feed(input)

  def toggleOplog: Emulator = copy(oplogEnabled = !oplogEnabled)

  def clearOplog: Emulator = copy(oplog = Queue.empty)

  def feed(input: String): Emulator =
    assert(input.indexOf('\n') == input.length - 1)

    def loop(tick: Tick, chars: List[Char], ops: Queue[(Inst, String)] = Queue.empty): (Tick, Queue[(Inst, String)]) = tick match
      case Tick.Continue(state) =>
        val log = if oplogEnabled then ops.enqueue(state.inst -> state.showInst) else ops
        loop(state.tick, chars, log)
      case Tick.Input(f) => chars match
        case c::cs => loop(f(c).tick, cs)
        case Nil => throw new Exception("Not enough input.") // dead code
      case tick => if chars.isEmpty then (tick, ops) else
        throw new Exception(s"Input too long.") // dead code

    val (next, ops) = loop(state, input.toList)
    copy(state = next, inputHistory = inputHistory.enqueue(input), oplog = oplog.enqueueAll(ops))

  def progressUntilBlocked: Emulator =
    // TODO make LazyList
    def loop(tick: Tick, chars: Queue[Char], ops: Queue[(Inst, String)] = Queue.empty): (Tick, String, Queue[(Inst, String)]) = tick match
      case Tick.Continue(state) =>
        if oplogEnabled then println(state.showInst)
        val log = if oplogEnabled then ops.enqueue(state.inst -> state.showInst) else ops
        loop(state.tick, chars, log)
      case Tick.Output(c, state) =>
        val log = if oplogEnabled then ops.enqueue(state.inst -> state.showInst) else ops
        loop(state.tick, chars.enqueue(c), log)
      case _ => (tick, chars.mkString, ops)

    val (next, out, ops) = loop(state, Queue.empty)
    assert(next.isBlocked)
    copy(next, history = state::history, outputQueue = outputQueue.enqueue(out), oplog = oplog.enqueueAll(ops))

  def useOutput(f: String => Unit): Emulator =
    val (out, outs) = outputQueue.dequeue
    f(out)
    copy(outputQueue = outs)

  def undo: Emulator = history match
    case _ :: past :: olderHistory =>
      copy(state = past, history = olderHistory)
      // TODO: drop command history
    case _ => this

  def modifyState(f: VMState[Ready] => VMState[Ready]): Emulator = state match
    case tick: Tick.Input =>
      this.copy(state = Tick.Input(tick.f.andThen(f)))
    case tick: Tick.Continue =>
      this.copy(state = Tick.Continue(f(tick.state)))
    case tick: Tick.Output =>
      this.copy(state = tick.copy(state = f(tick.state)))
    case _ => ???

  def setRegister(r: numbers.Reg, v: numbers.U15): Emulator =
    modifyState(_.modifyRegisters(_.updated(r, v)))

  def getRegister(reg: numbers.Reg): numbers.Word = state match
    case tick: Tick.Input => tick.f(' ').registers(reg) // TODO: is hack
    case _ => ???

object Emulator:
  def init(start: Tick): Emulator =
    apply(start, Nil, Queue.empty, Queue.empty, Queue.empty, false)
  trait HackPerm
  val HackPerm = new HackPerm {}
