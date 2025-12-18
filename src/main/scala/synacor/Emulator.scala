package synacor

def feedInput(start: Tick, input: String): Tick =
  assert(input.last == '\n')
  // assert(start.isInstanceOf[Tick.Input])
  input.foldLeft(start):
    case (tick, c) =>
      Iterator.iterate(tick):
        case Tick.Continue(state) => state.tick
        case Tick.Input(f) => f(c).tick // dummy ticks
      .collectFirst:
        case Tick.Input(f) => f(c).tick
      .getOrElse:
        throw new Exception(s"input too long: $input")

def progressCollectingOutput(start: Tick): (String, Tick) =
  val sb = collection.mutable.StringBuilder()
  var tick = start

  while !tick.needsInput do
    tick match
      case Tick.Continue(state) => tick = state.tick
      case Tick.Output(c, state) =>
        sb.addOne(c)
        tick = state.tick
      case Tick.Input(f) => ()
      case Tick.Halt(code, last) => ()

  sb.result() -> tick

case class Emulator(state: Tick, history: List[Tick]):
  def feed(input: String): Emulator =
    val next = feedInput(state, input)
    Emulator(next, state::history)

  def nextOut: (String, Emulator) =
    val (out, next) = progressCollectingOutput(state)
    out -> Emulator(next, state::history)
