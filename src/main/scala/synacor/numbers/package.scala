package synacor
package numbers

opaque type Word = Int

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

  def fromU15(n: U15): Word = n.toInt

  extension (w: Word)
    def fitsU15: Boolean = (w & 0x8000) != 0x8000
    def hex: String = f"0x${w}%04X"
    def toInt: Int = w

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
