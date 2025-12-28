package synacor
package numbers

opaque type U15 = Int
opaque type Adr <: U15 = Int
opaque type Lit <: U15 = Int

object U15:
  val MaxValue: U15 = 0x7FFF
  def parse(w: Word): U15 =
    require(Word.fitsU15(w), s"${w.hex} is not a valid U15")
    w.toInt

  extension (v: U15)
    inline def hex: String = f"0x${v}%04X"
    inline def asAdr: Adr = v
    inline def asLit: Lit = v
    inline def toInt: Int = v

object Adr:
  def parse(w: Word): Adr = U15.parse(w)

  extension (a: Adr)
    inline def inc1: Adr = a + 1
    inline def inc2: Adr = a + 2
    inline def inc3: Adr = a + 3
    inline def inc4: Adr = a + 4
    inline def toIndex: Int = a & 0x7FFF

@annotation.nowarn("msg=Extension method .* will never be selected*")
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
