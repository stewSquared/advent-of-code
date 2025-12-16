package synacor
package numbers

opaque type Word = Int
opaque type U15 <: Word = Int
opaque type Adr <: U15 = Int
opaque type Lit <: U15 = Int
opaque type Reg <: Word = Int

extension (n: Int)
  def toLit: Lit = Word.fromInt(n)

object Word:
  def fromBytes(low: Byte, high: Byte): Word =
    low & 0xFF | (high << 8) & 0xFF00

  def fromInt(n: Int): Word =
    require(n <= 0xFFFF)
    n

  extension (w: Word)
    def adr: Adr = Adr.fromInt(w)
    def lit: Lit = Lit.fromInt(w)
    def reg: Reg = Reg.fromInt(w)
    // def value: Reg | Lit =
    //   if (w & 0x8000) == 0x8000 then Reg.fromInt(w)
    //   else Lit.fromInt(w)
    def asChar: Char = w.toChar
    def fitsU15: Boolean = (w & 0x8000) == 0x8000
    def op: Opcode = Opcode.fromOrdinal(w)

object U15:
  val MaxValue: U15 = 0x7FFF
  def fromInt(n: Int): U15 =
    require(n <= U15.MaxValue)
    n

object Adr:
  def fromInt(n: Int): Adr = U15.fromInt(n)

extension (a: Adr)
  inline def inc1: Adr = a + 1
  inline def inc2: Adr = a + 2
  inline def inc3: Adr = a + 3
  inline def inc4: Adr = a + 4
  inline def toIndex: Int = a & 0x7FFF

object Lit:
  def fromInt(n: Int): Lit = U15.fromInt(n)

extension (a: Lit)
  infix def +(b: Lit): Lit = (a + b) & U15.MaxValue
  infix def *(b: Lit): Lit = (a * b) & U15.MaxValue
  infix def %(b: Lit): Lit = (a % b) & U15.MaxValue
  infix def &(b: Lit): Lit = (a & b) & U15.MaxValue
  infix def |(b: Lit): Lit = (a | b) & U15.MaxValue
  infix def >(b: Lit): Boolean = a > b
  def unary_~ = ((~a) & U15.MaxValue): Lit

object Reg:
  def fromInt(n: Int): Reg =
    require(0x8000 <= n && n <= 0x8007)
    n

extension (r: Reg)
  def toIndex(using DummyImplicit): Int = r & 0x7FFF
