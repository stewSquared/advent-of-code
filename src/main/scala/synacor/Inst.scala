package synacor

import numbers.*

enum Arg[T <: U15]:
  case Const(value: T)
  case Ref(reg: Reg)

  override def toString: String = this match
    case Const(value) => value.hex
    case Ref(reg) => reg.name

  def value(using regs: Registers, cast: Cast[T]): T = this match
    case Const(value) => value
    case Ref(reg) => cast(regs(reg))

  def show(using regs: Registers): String = this match
    case Const(value) => value.hex
    case Ref(reg) => s"${reg.name}(${regs(reg).hex})"

object Arg:
  import Cast.cast

  def parse[T <: U15 : Cast](w: Word): Arg[T] =
    if w.fitsU15 then Arg.Const(w.cast) else Arg.Ref(w.reg)

trait Cast[T]:
  def apply(w: Word): T

object Cast:
  given Cast[Lit]:
    def apply(w: Word) = w.lit
  given Cast[Adr]:
    def apply(w: Word) = w.adr
  given Cast[U15]:
    def apply(w: Word) = w.u15

  extension(w: Word)
    def cast[T](using cast: Cast[T]): T = cast.apply(w)

enum Inst(val op: Opcode):
  /** stop execution and terminate the program */
  case HALT extends Inst(Opcode.HALT)
  /** set register <a> to the value of <b> */
  case SET(a: Reg, b: Arg[Lit]) extends Inst(Opcode.SET)
  /** push <a> onto the stack */
  case PUSH(a: Arg[Lit]) extends Inst(Opcode.PUSH)
  /** remove the top element from the stack and write it into <a>; empty stack = error */
  case POP(a: Reg) extends Inst(Opcode.POP)
  /** set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise */
  case EQ(a: Reg, b: Arg[Lit], c: Arg[Lit]) extends Inst(Opcode.EQ)
  /** set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise */
  case GT(a: Reg, b: Arg[Lit], c: Arg[Lit]) extends Inst(Opcode.GT)
  /** jump to <a> */
  case JMP(a: Arg[Adr]) extends Inst(Opcode.JMP)
  /** if <a> is nonzero, jump to <b> */
  case JT(a: Arg[Lit], b: Arg[Adr]) extends Inst(Opcode.JT)
  /** if <a> is zero, jump to <b> */
  case JF(a: Arg[Lit], b: Arg[Adr]) extends Inst(Opcode.JF)
  /** assign into <a> the sum of <b> and <c> (modulo 32768) */
  case ADD(a: Reg, b: Arg[Lit], c: Arg[Lit]) extends Inst(Opcode.ADD)
  /** store into <a> the product of <b> and <c> (modulo 32768) */
  case MULT(a: Reg, b: Arg[Lit], c: Arg[Lit]) extends Inst(Opcode.MULT)
  /** store into <a> the remainder of <b> divided by <c> */
  case MOD(a: Reg, b: Arg[Lit], c: Arg[Lit]) extends Inst(Opcode.MOD)
  /** stores into <a> the bitwise and of <b> and <c> */
  case AND(a: Reg, b: Arg[Lit], c: Arg[Lit]) extends Inst(Opcode.AND)
  /** stores into <a> the bitwise or of <b> and <c> */
  case OR(a: Reg, b: Arg[Lit], c: Arg[Lit]) extends Inst(Opcode.OR)
  /** stores 15-bit bitwise inverse of <b> in <a> */
  case NOT(a: Reg, b: Arg[Lit]) extends Inst(Opcode.NOT)
  /** read memory at address <b> and write it to <a> */
  case RMEM(a: Reg, b: Arg[Adr]) extends Inst(Opcode.RMEM)
  /** write the value from <b> into memory at address <a> */
  case WMEM(a: Arg[Adr], b: Arg[Lit]) extends Inst(Opcode.WMEM)
  /** write the address of the next instruction to the stack and jump to <a> */
  case CALL(a: Arg[Adr]) extends Inst(Opcode.CALL)
  /** remove the top element from the stack and jump to it; empty stack = halt */
  case RET extends Inst(Opcode.RET)
  /** write the character represented by ascii code <a> to the terminal */
  case OUT(a: Arg[Lit]) extends Inst(Opcode.OUT)
  /** read a character from the terminal and write its ascii code to <a>; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read */
  case IN(a: Reg) extends Inst(Opcode.IN)
  /** no operation */
  case NOOP extends Inst(Opcode.NOOP)

  def show(using Registers, Memory): String = this match
    case HALT => "HALT"
    case SET(a, b) => s"SET ${a.name} ${b.show}"
    case PUSH(a) => s"PUSH ${a.show}"
    case POP(a) => s"POP ${a.name}"
    case EQ(a, b, c) => s"EQ ${a.name} ${b.show} ${c.show}"
    case GT(a, b, c) => s"GT ${a.name} ${b.show} ${c.show}"
    case JMP(a) => s"JMP ${a.show}"
    case JT(a, b) => s"JT ${a.show} ${b.show}"
    case JF(a, b) => s"JF ${a.show} ${b.show}"
    case ADD(a, b, c) => s"ADD ${a.name} ${b.show} ${c.show}"
    case MULT(a, b, c) => s"MULT ${a.name} ${b.show} ${c.show}"
    case MOD(a, b, c) => s"MOD ${a.name} ${b.show} ${c.show}"
    case AND(a, b, c) => s"AND ${a.name} ${b.show} ${c.show}"
    case OR(a, b, c) => s"OR ${a.name} ${b.show} ${c.show}"
    case NOT(a, b) => s"NOT ${a.name} ${b.show}"
    case RMEM(a, b) => s"RMEM ${a.name} M[${b.show}](${summon[Memory].apply(b.value).hex})"
    case WMEM(a, b) => s"WMEM M[${a.show}] ${b.show}"
    case CALL(a) => s"CALL ${a.show}"
    case RET => "RET"
    case OUT(a) => s"OUT ${a.show}"
    case IN(a) => s"IN ${a.name}"
    case NOOP => "NOOP"

  override def toString: String =
    this.productIterator.map:
      case reg if reg.toString.toIntOption.isDefined => reg.asInstanceOf[Reg].name
      case n => n.toString
    .mkString(s"${this.productPrefix}(", ", ", ")")

object Inst:
  extension (w: Word)
    def lit: Arg[Lit] = Arg.parse[Lit](w)
    def adr: Arg[Adr] = Arg.parse[Adr](w)

  def parse(op: Opcode, a: => Word, b: => Word, c: => Word): Inst = op match
    case Opcode.HALT => Inst.HALT
    case Opcode.SET => Inst.SET(a.reg, b.lit)
    case Opcode.PUSH => Inst.PUSH(a.lit)
    case Opcode.POP => Inst.POP(a.reg)
    case Opcode.EQ => Inst.EQ(a.reg, b.lit, c.lit)
    case Opcode.GT => Inst.GT(a.reg, b.lit, c.lit)
    case Opcode.JMP => Inst.JMP(a.adr)
    case Opcode.JT => Inst.JT(a.lit, b.adr)
    case Opcode.JF => Inst.JF(a.lit, b.adr)
    case Opcode.ADD => Inst.ADD(a.reg, b.lit, c.lit)
    case Opcode.MULT => Inst.MULT(a.reg, b.lit, c.lit)
    case Opcode.MOD => Inst.MOD(a.reg, b.lit, c.lit)
    case Opcode.AND => Inst.AND(a.reg, b.lit, c.lit)
    case Opcode.OR => Inst.OR(a.reg, b.lit, c.lit)
    case Opcode.NOT => Inst.NOT(a.reg, b.lit)
    case Opcode.RMEM => Inst.RMEM(a.reg, b.adr)
    case Opcode.WMEM => Inst.WMEM(a.adr, b.lit)
    case Opcode.CALL => Inst.CALL(a.adr)
    case Opcode.RET => Inst.RET
    case Opcode.OUT => Inst.OUT(a.lit)
    case Opcode.IN => Inst.IN(a.reg)
    case Opcode.NOOP => Inst.NOOP
