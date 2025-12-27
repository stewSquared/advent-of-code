package synacor
package numbers

opaque type Word = Int
opaque type U15 = Int
opaque type Adr <: U15 = Int
opaque type Lit <: U15 = Int

extension (n: Int)
  def toU15: U15 = U15.parse(n)
  def toLit: Lit = Lit.parse(n)
  def toAdr: Adr = Adr.parse(n)
  def toReg: Reg = Reg.parse(n)

extension (s: String)
  def toWord: Word = Word.fromInt(Integer.parseUnsignedInt(s, 10))

object Word:
  def fromBytes(low: Byte, high: Byte): Word =
    low & 0xFF | (high << 8) & 0xFF00

  def fromInt(n: Int): Word =
    require(n <= 0xFFFF, s"${n.toHexString} is not a valid 16-bit word")
    n

  def fromU15(n: U15): Word = n

  extension (w: Word)
    def fitsU15: Boolean = (w & 0x8000) != 0x8000
    def hex: String = f"0x${w}%04X"
    def toInt: Int = w

object U15:
  val MaxValue: U15 = 0x7FFF
  def parse(w: Word): U15 =
    require(Word.fitsU15(w), s"${w.hex} is not a valid U15")
    w

  extension (v: U15)
    inline def hex: String = f"0x${v}%04X"
    inline def asAdr: Adr = v
    inline def asLit: Lit = v

object Adr:
  def parse(w: Word): Adr = U15.parse(w)

  extension (a: Adr)
    inline def inc1: Adr = a + 1
    inline def inc2: Adr = a + 2
    inline def inc3: Adr = a + 3
    inline def inc4: Adr = a + 4
    inline def toIndex: Int = a & 0x7FFF

object Lit:
  def parse(w: Word): Lit = U15.parse(w)

  extension (a: Lit)
    infix def +(b: Lit): Lit = (a + b) & U15.MaxValue
    infix def *(b: Lit): Lit = (a * b) & U15.MaxValue
    infix def %(b: Lit): Lit = (a % b) & U15.MaxValue
    infix def &(b: Lit): Lit = (a & b) & U15.MaxValue
    infix def |(b: Lit): Lit = (a | b) & U15.MaxValue
    infix def >(b: Lit): Boolean = a > b
    def unary_~ = ((~a) & U15.MaxValue): Lit
    inline def toChar: Char = a.toChar

enum Reg:
  case R1, R2, R3, R4, R5, R6, R7, R8

object Reg:
  def parse(w: Word): Reg =
    require((0x8000 to 0x8007).contains(w), s"invalid register value: ${w.hex}")
    Reg.fromOrdinal(w.toInt & 0x7)

  def fromIndex(i: Int): Reg = Reg.fromOrdinal(i)

  extension (r: Reg)
    inline def toIndex: Int = r.ordinal
    inline def name = r.toString
