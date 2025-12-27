package synacor
package numbers

opaque type Word = Int
opaque type U15 <: Word = Int
opaque type Adr <: U15 = Int
opaque type Lit <: U15 = Int

extension (n: Int)
  def toU15: Lit = U15.fromInt(n)
  def toLit: Lit = Word.fromInt(n)
  def toAdr: Adr = Adr.fromInt(n)
  def toReg: Reg = Reg.fromInt(n)

object Word:
  def fromBytes(low: Byte, high: Byte): Word =
    low & 0xFF | (high << 8) & 0xFF00

  def fromInt(n: Int): Word =
    require(n <= 0xFFFF, s"${n.toHexString} is not a valid 16-bit word")
    n

  extension (w: Word)
    def u15: U15 = U15.fromInt(w)
    def adr: Adr = Adr.fromInt(w)
    def lit: Lit = Lit.fromInt(w)
    def reg: Reg = Reg.fromInt(w)
    def asChar: Char = w.toChar
    def fitsU15: Boolean = (w & 0x8000) != 0x8000
    def op: Opcode = Opcode.fromOrdinal(w)
    def hex: String = f"0x${w}%04X"
    def toInt: Int = w

object U15:
  val MaxValue: U15 = 0x7FFF
  def fromInt(n: Int): U15 =
    require(n <= U15.MaxValue, s"${n.toHexString} is not a valid unsigned 15-bit integer")
    n
  def parse(w: Word): U15 =
    require(Word.fitsU15(w), s"${Word.hex(w)} is not a valid U15")
    w

object Adr:
  def fromInt(n: Int): Adr = U15.fromInt(n)
  def parse(w: Word): Adr = U15.parse(w)

extension (a: Adr)
  inline def inc1: Adr = a + 1
  inline def inc2: Adr = a + 2
  inline def inc3: Adr = a + 3
  inline def inc4: Adr = a + 4
  inline def toIndex: Int = a & 0x7FFF

object Lit:
  def fromInt(n: Int): Lit = U15.fromInt(n)
  def parse(w: Word): Lit = U15.parse(w)

extension (a: Lit)
  infix def +(b: Lit): Lit = (a + b) & U15.MaxValue
  infix def *(b: Lit): Lit = (a * b) & U15.MaxValue
  infix def %(b: Lit): Lit = (a % b) & U15.MaxValue
  infix def &(b: Lit): Lit = (a & b) & U15.MaxValue
  infix def |(b: Lit): Lit = (a | b) & U15.MaxValue
  infix def >(b: Lit): Boolean = a > b
  def unary_~ = ((~a) & U15.MaxValue): Lit

enum Reg:
  case R1, R2, R3, R4, R5, R6, R7, R8

object Reg:
  def fromInt(n: Int): Reg =
    require(0x8000 <= n && n <= 0x8007, s"invalid register value: ${n.toHexString}")
    Reg.fromOrdinal(n & 0x7FFF)

  def parse(w: Word): Reg =
    require((0x8000 to 0x8007).contains(w), s"invalid register value: ${Word.hex(w)}")
    Reg.fromOrdinal(w.toInt & 0x7)

  def fromIndex(i: Int): Reg = Reg.fromOrdinal(i)

extension (r: Reg)
  inline def toIndex: Int = r.ordinal
  inline def name = r.toString
