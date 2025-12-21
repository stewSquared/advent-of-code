package synacor

import numbers.*

import synacor.{Registers, Opcode}
import synacor.numbers.{Reg, Lit, Adr, Word, U15}

enum Inst(val op: Opcode):
  /* stop execution and terminate the program */
  case HALT extends Inst(Opcode.HALT)

  /* set register <a> to the value of <b> */
  case SET(a: RegArg, b: LitArg) extends Inst(Opcode.SET)

  /* push <a> onto the stack */
  case PUSH(a: RegArg) extends Inst(Opcode.PUSH)

  /* remove the top element from the stack and write it into <a>; empty stack = error */
  case POP(a: RegArg) extends Inst(Opcode.POP)

  /* set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise */
  case EQ(a: RegArg, b: LitArg, c: LitArg) extends Inst(Opcode.EQ)

  /* set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise */
  case GT(a: RegArg, b: LitArg, c: LitArg) extends Inst(Opcode.GT)

  /* jump to <a> */
  case JMP(a: AdrArg) extends Inst(Opcode.JMP)

  /* if <a> is nonzero, jump to <b> */
  case JT(a: LitArg, b: AdrArg) extends Inst(Opcode.JT)

  /* if <a> is zero, jump to <b> */
  case JF(a: LitArg, b: AdrArg) extends Inst(Opcode.JF)

  /* assign into <a> the sum of <b> and <c> (modulo 32768) */
  case ADD(a: RegArg, b: LitArg, c: LitArg) extends Inst(Opcode.ADD)

  /* store into <a> the product of <b> and <c> (modulo 32768) */
  case MULT(a: RegArg, b: LitArg, c: LitArg) extends Inst(Opcode.MULT)

  /* store into <a> the remainder of <b> divided by <c> */
  case MOD(a: RegArg, b: LitArg, c: LitArg) extends Inst(Opcode.MOD)

  /* stores into <a> the bitwise and of <b> and <c> */
  case AND(a: RegArg, b: LitArg, c: LitArg) extends Inst(Opcode.AND)

  /* stores into <a> the bitwise or of <b> and <c> */
  case OR(a: RegArg, b: LitArg, c: LitArg) extends Inst(Opcode.OR)

  /* stores 15-bit bitwise inverse of <b> in <a> */
  case NOT(a: RegArg, b: LitArg) extends Inst(Opcode.NOT)

  /* read memory at address <b> and write it to <a> */
  case RMEM(a: RegArg, b: AdrArg) extends Inst(Opcode.RMEM)

  /* write the value from <b> into memory at address <a> */
  case WMEM(a: AdrArg, b: LitArg) extends Inst(Opcode.WMEM)

  /* write the address of the next instruction to the stack and jump to <a> */
  case CALL(a: AdrArg) extends Inst(Opcode.CALL)

  /* remove the top element from the stack and jump to it; empty stack = halt */
  case RET extends Inst(Opcode.RET)

  /* write the character represented by ascii code <a> to the terminal */
  case OUT(a: LitArg) extends Inst(Opcode.OUT)

  /* read a character from the terminal and write its ascii code to <a>; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read */
  case IN(a: RegArg) extends Inst(Opcode.IN)

  /* no operation */
  case NOOP extends Inst(Opcode.NOOP)

  def show(using Registers): String =
    import Inst.*
    val args = this match
      case SET(a, b) => s"${a.show} ${b.show}"
      case PUSH(a) => s"${a.show}"
      case POP(a) => s"${a.show}"
      case EQ(a, b, c) => s"${a.show} ${b.show} ${c.show}"
      case GT(a, b, c) => s"${a.show} ${b.show} ${c.show}"
      case JMP(a) => s"${a.show}"
      case JT(a, b) => s"${a.show} ${b.show}"
      case JF(a, b) => s"${a.show} ${b.show}"
      case ADD(a, b, c) => s"${a.show} ${b.show} ${c.show}"
      case MULT(a, b, c) => s"${a.show} ${b.show} ${c.show}"
      case MOD(a, b, c) => s"${a.show} ${b.show} ${c.show}"
      case AND(a, b, c) => s"${a.show} ${b.show} ${c.show}"
      case OR(a, b, c) => s"${a.show} ${b.show} ${c.show}"
      case NOT(a, b) => s"${a.show} ${b.show}"
      case RMEM(a, b) => s"${a.show} M[${b.show}]"
      case WMEM(a, b) => s"M[${a.show}] ${b.show}"
      case CALL(a) => s"${a.show}"
      case OUT(a) =>
        val ch = a.value.asChar
        s"${a.show} '${if ch == '\n' then "\\n" else ch}'"
      case IN(a) => s"${a.show}"
      case HALT | RET | NOOP => ""
    s"$op $args"

object Inst:
  def currentInstruction(vm: VMState[?]): Inst =
    import vm.{op, a, b, c}

    def aReg = RegArg(a.reg)
    def aLit = LitArg.fromWord(a)
    def bLit = LitArg.fromWord(b)
    def cLit = LitArg.fromWord(c)
    def aAdr = AdrArg.fromWord(a)
    def bAdr = AdrArg.fromWord(b)

    op match
      case Opcode.HALT => Inst.HALT
      case Opcode.SET => Inst.SET(aReg, bLit)
      case Opcode.PUSH => Inst.PUSH(aReg)
      case Opcode.POP => Inst.POP(aReg)
      case Opcode.EQ => Inst.EQ(aReg, bLit, cLit)
      case Opcode.GT => Inst.GT(aReg, bLit, cLit)
      case Opcode.JMP => Inst.JMP(aAdr)
      case Opcode.JT => Inst.JT(aLit, bAdr)
      case Opcode.JF => Inst.JF(aLit, bAdr)
      case Opcode.ADD => Inst.ADD(aReg, bLit, cLit)
      case Opcode.MULT => Inst.MULT(aReg, bLit, cLit)
      case Opcode.MOD => Inst.MOD(aReg, bLit, cLit)
      case Opcode.AND => Inst.AND(aReg, bLit, cLit)
      case Opcode.OR => Inst.OR(aReg, bLit, cLit)
      case Opcode.NOT => Inst.NOT(aReg, bLit)
      case Opcode.RMEM => Inst.RMEM(aReg, bAdr)
      case Opcode.WMEM => Inst.WMEM(aAdr, bLit)
      case Opcode.CALL => Inst.CALL(aAdr)
      case Opcode.RET => Inst.RET
      case Opcode.OUT => Inst.OUT(aLit)
      case Opcode.IN => Inst.IN(aReg)
      case Opcode.NOOP => Inst.NOOP
