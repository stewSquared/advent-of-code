package synacor

import collection.immutable.Queue

case class Emulator(state: Tick, history: List[Tick]):
  def feed(input: String): Emulator =
    assert(input.indexOf('\n') == input.length - 1)

    def loop(tick: Tick, chars: List[Char]): Tick = tick match
      case Tick.Continue(state) => loop(state.tick, chars)
      case Tick.Input(f) => chars match
        case c::cs => loop(f(c).tick, cs)
        case Nil => throw new Exception("Not enough input.") // dead code
      case tick => if chars.isEmpty then tick else
        throw new Exception(s"Input too long.") // dead code

    val next = loop(state, input.toList)
    Emulator(next, state::history)

  def nextOut: (String, Emulator) =
    def loop(tick: Tick, chars: Queue[Char]): (String, Tick) = tick match
      case Tick.Continue(state) => loop(state.tick, chars)
      case Tick.Output(c, state) => loop(state.tick, chars.enqueue(c))
      case _ => chars.mkString -> tick

    val (out, next) = loop(state, Queue.empty)
    out -> Emulator(next, state::history)
