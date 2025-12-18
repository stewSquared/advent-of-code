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

memory.get

val start = VMState(
  pc = Adr.fromInt(0),
  registers = Registers.init,
  stack = Nil,
  memory.get
)

// start.step

// [info] The cover of this book subtly swirls with colors.  It is titled "A Brief Introd
// [info] uction to Interdimensional Physics".  It reads:
// [info] Recent advances in interdimensional physics have produced fascinating
// [info] predictions about the fundamentals of our universe!  For example,
// [info] interdimensional physics seems to predict that the universe is, at its root, a
// [info] purely mathematical construct, and that all events are caused by the
// [info] interactions between eight pockets of energy called "registers".
// [info] Furthermore, it seems that while the lower registers primarily control mundane
// [info] things like sound and light, the highest register (the so-called "eighth
// [info] register") is used to control interdimensional events such as teleportation.
// [info] A hypothetical such teleportation device would need to have have exactly two
// [info] destinations.  One destination would be used when the eighth register is at its
// [info] minimum energy level - this would be the default operation assuming the user
// [info] has no way to control the eighth register.  In this situation, the teleporter
// [info] should send the user to a preconfigured safe location as a default.
// [info] The second destination, however, is predicted to require a very specific
// [info] energy level in the eighth register.  The teleporter must take great care to
// [info] confirm that this energy level is exactly correct before teleporting its user!
// [info] If it is even slightly off, the user would (probably) arrive at the correct
// [info] location, but would briefly experience anomalies in the fabric of reality
// [info] itself - this is, of course, not recommended.  Any teleporter would need to tes
// [info] t
// [info] the energy level in the eighth register and abort teleportation if it is not
// [info] exactly correct.
// [info] This required precision implies that the confirmation mechanism would be very
// [info] computationally expensive.  While this would likely not be an issue for large-
// [info] scale teleporters, a hypothetical hand-held teleporter would take billions of
// [info] years to compute the result and confirm that the eighth register is correct.
// [info] If you find yourself trapped in an alternate dimension with nothing but a
// [info] hand-held teleporter, you will need to extract the confirmation algorithm,
// [info] reimplement it on more powerful hardware, and optimize it.  This should, at the
// [info] very least, allow you to determine the value of the eighth register which would
// [info] have been accepted by the teleporter's confirmation mechanism.
// [info] Then, set the eighth register to this value, activate the teleporter, and
// [info] bypass the confirmation mechanism.  If the eighth register is set correctly, no
// [info] anomalies should be experienced, but beware - if it is set incorrectly, the
// [info] now-bypassed confirmation mechanism will not protect you!
// [info] Of course, since teleportation is impossible, this is all totally ridiculous.


// ## Coin Puzzle

// red coin 2 dots
// blue coin 9 dots
// shiny coin pentagon
// concave coin 7 dots
// corroded coin triangle

// _ + _ * _^2 + _^3 - _ == 399
// _ + _ * math.pow(_, 2).toInt + math.pow(_, 3) - _ == 399

// 393 == 9 + 2 * 5^2 + 7^3 - 3

// 9 + 2 * math.pow(5, 2).toInt + math.pow(7, 3).toInt - 3 == 399
// blue, red, shiny, concave, corroded

// def states = Iterator.unfold(start)(_.step)

// val ans = states.foreach:
//   case None => ()
//   case Some(char) => print(char)

//
