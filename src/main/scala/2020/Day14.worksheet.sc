val input = io.Source.fromResource("2020/day-14.txt").getLines.toList

input.size
// val mem = collection.mutable.Map().withDefaultValue(0)

def masked(n: Long, mask: String): Long =
  val bin = n.toBinaryString.reverse.padTo(36, '0').reverse
  val newBin = bin.zip(mask).map {
    case (b, 'X') => b
    case (_, '1') => '1'
    case (_, '0') => '0'
  }
  BigInt.apply(newBin.mkString, 2).toLong

def maskedAddresses(a: Long, mask: String): List[Long] =
  val bin = a.toBinaryString.reverse.padTo(36, '0').reverse.toList
  def newBins(addresses: List[String], pairs: List[(Char, Char)]): List[String] = pairs match
    case (_, 'X') :: tail =>
      val both = addresses.flatMap {
        addr => List(addr :+ '0', addr :+ '1')
      }
      newBins(both, tail)
    case (_, '1') :: tail => newBins(addresses.map(_ :+ '1'), tail)
    case (b, '0') :: tail => newBins(addresses.map(_ :+ b), tail)
    case Nil => addresses
  newBins(List(""), bin.zip(mask))
    // .tapEach(println(_))
    .map(a => BigInt.apply(a.mkString, 2).toLong)


maskedAddresses(42L, "000000000000000000000000000000X1001X")
maskedAddresses(26L, "00000000000000000000000000000000X0XX")

case class State(mask: String, memory: Map[Long, Long]):
  def interperet(inst: String): State = inst match
    case s"mask = $m" => copy(mask = m)
    case s"mem[$a] = $v" => copy(memory = memory.updated(a.toLong, masked(v.toLong, mask)))

  def interperet2(inst: String): State = inst match
    case s"mask = $m" => copy(mask = m)
    case s"mem[$a] = $v" =>
      val addrs = maskedAddresses(a.toLong, mask)
      addrs.foldLeft(this)((s, addr) =>
        s.copy(memory = s.memory.updated(addr, v.toLong))
      )


val start = State(mask = "X" * 36, Map.empty.withDefaultValue(0L))

val finalState: State = input.foldLeft(start)((s, i) => s.interperet(i))

val ans = finalState.memory.values.sum

val ans2 =
  val s = input.foldLeft(start)(_ interperet2 _)
  s.memory.values.sum

input.foldLeft(start)(_ interperet2 _).memory
