package synacor

import collection.immutable.Queue

// TODO alternate between in/out types
case class Emulator(state: Tick, history: List[Tick], outputQueue: Queue[String], inputHistory: Queue[String]):
  def feedMultiple(inputs: List[String]): Emulator =
    inputs.foldLeft(this):
      case (state, input) =>
        state.progressUntilBlocked.feed(input)

  def feed(input: String): Emulator =
    assert(input.indexOf('\n') == input.length - 1)

    def loop(tick: Tick, chars: List[Char]): Tick = tick match
      case Tick.Continue(state) => loop(state.tick, chars)
      case Tick.Input(f, state) => chars match
        case c::cs => loop(f(c).tick, cs)
        case Nil => throw new Exception("Not enough input.") // dead code
      case tick => if chars.isEmpty then tick else
        throw new Exception(s"Input too long.") // dead code

    copy(state = loop(state, input.toList), inputHistory = inputHistory.enqueue(input))

  def progressUntilBlocked: Emulator =
    def loop(tick: Tick, chars: Queue[Char]): (String, Tick) = tick match
      case Tick.Continue(state) => loop(state.tick, chars)
      case Tick.Output(c, state) => loop(state.tick, chars.enqueue(c))
      case _ => chars.mkString -> tick

    val (out, next) = loop(state, Queue.empty)
    assert(next.isBlocked)
    copy(next, state::history, outputQueue.enqueue(out))

  def useOutput(f: String => Unit): Emulator =
    val (out, outs) = outputQueue.dequeue
    f(out)
    copy(outputQueue = outs)

  def undo: Emulator = history match
    case _ :: past :: olderHistory =>
      copy(state = past, history = olderHistory)
      // TODO: drop command history
    case _ => this

  def setRegisters(f: Registers => Registers): Emulator = state match
    case tick: Tick.Input =>
      val newTick = tick.copy(state = tick.state.copy(registers = f(tick.state.registers)))
      this.copy(state = newTick)
    case tick: Tick.Continue =>
      val newTick = tick.copy(state = tick.state.copy(registers = f(tick.state.registers)))
      this.copy(state = newTick)
    case tick: Tick.Output =>
      val newTick = tick.copy(state = tick.state.copy(registers = f(tick.state.registers)))
      this.copy(state = newTick)
    case _ => ???

  def getRegister(reg: numbers.Reg): numbers.Word = state match
    case tick: Tick.Input => tick.state.registers(reg)
    case _ => ???

object Emulator:
  def init(start: Tick): Emulator =
    apply(start, Nil, Queue.empty, Queue.empty)

