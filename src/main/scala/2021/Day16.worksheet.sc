import collection.mutable.{ListBuffer, StringBuilder}

enum Packet:
  case Literal(version: Int, binaryString: String)
  case Operator(version: Int, typ: Int, subpackets: List[Packet])

  def versions: List[Int] = this match
    case lit: Packet.Literal => lit.version :: Nil
    case op: Packet.Operator => op.version :: op.subpackets.flatMap(_.versions)

  def value: BigInt = this match
    case lit: Literal => BigInt(lit.binaryString, 2)
    case op: Operator =>
      val values = op.subpackets.map(_.value)
      op.typ match
        case 0 => values.sum
        case 1 => values.product
        case 2 => values.min
        case 3 => values.max
        case 5 => if values(0) > values(1) then 1 else 0
        case 6 => if values(0) < values(1) then 1 else 0
        case 7 => if values(0) == values(1) then 1 else 0

object Packet:
  extension (bits: Iterator[Char])
    def nextString(n: Int): String =
      val sb = new StringBuilder
      for _ <- 0 until n do sb += bits.next()
      sb.result

    def nextInt(n: Int): Int = Integer.parseInt(nextString(n), 2)

  def apply(binaryString: String): Packet = parse(binaryString.iterator)

  def parse(bits: Iterator[Char]): Packet =
    val version = bits.nextInt(3)
    val typ = bits.nextInt(3)
    typ match
      case 4     => Literal(version, parseBinaryString(bits))
      case opTyp => Operator(version, opTyp, parseSubpackets(bits))

  def parseBinaryString(bits: Iterator[Char]): String =
    val nibbles = new ListBuffer[String]
    while bits.next() == '1' do nibbles += bits.nextString(4)
    nibbles += bits.nextString(4)
    nibbles.result().mkString

  def parseSubpackets(bits: Iterator[Char]): List[Packet] =
    val lengthTypeId = bits.next()
    lengthTypeId match
      case '0' =>
        val subpacketsLength = bits.nextInt(15)
        val subpacketBits = bits.take(subpacketsLength).buffered
        val subpackets = new ListBuffer[Packet]
        while subpacketBits.hasNext do subpackets += parse(subpacketBits)
        subpackets.result()
      case '1' =>
        val numSubpackets = bits.nextInt(11)
        List.fill(numSubpackets)(parse(bits))

def hexToBin(hex: String): String = hex.flatMap { digit =>
  val nibble = Integer.parseInt(digit.toString, 16).toBinaryString
  nibble.reverse.padTo(4, '0').reverse
}

val input = io.Source.fromResource("2021/day-16-1.txt").getLines.next()

val packet = Packet(hexToBin(input))

val ans1 = packet.versions.sum
val ans2 = packet.value

// tests

val example1A = Packet("110100101111111000101000")
  .asInstanceOf[Packet.Literal]
println(Integer.parseInt(example1A.binaryString, 2))
example1A.versions.sum

val example1B = Packet(
  "00111000000000000110111101000101001010010001001000000000"
)
example1B.versions.sum

val example1C = Packet(
  "11101110000000001101010000001100100000100011000001100000"
)
example1C.versions.sum

val example1D = Packet(hexToBin("8A004A801A8002F478"))
example1D.versions.sum

val example1E = Packet(hexToBin("620080001611562C8802118E34"))
example1E.versions.sum

val example1F = Packet(hexToBin("C0015000016115A2E0802F182340"))
example1F.versions.sum

val example1G = Packet(hexToBin("A0016C880162017C3686B18A3D4780"))
example1G.versions.sum
