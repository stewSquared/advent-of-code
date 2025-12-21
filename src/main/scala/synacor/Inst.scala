package synacor

import numbers.*

sealed trait Inst:
  def op: Opcode

case class Inst0(op: Opcode) extends Inst
case class Inst1[A <: U15](op: Opcode, a: Arg[A]) extends Inst
case class Inst2[A <: U15, B <: U15](op: Opcode, a: Arg[A], b: Arg[B]) extends Inst
case class Inst3[A <: U15, B <: U15, C <: U15](op: Opcode, a: Arg[A], b: Arg[B], c: Arg[C]) extends Inst

extension (vm: VMState[?])
  def inst: Inst =
    import Opcode.*
    import vm.{op, aArg, bArg, cArg}
    op match
      case HALT => Inst0(op)
      case SET => Inst2(op, aArg[U15], bArg[Lit])
      case PUSH => Inst1(op, aArg[U15])
      // case PUSH => Inst1(op, aRef])
      case POP => Inst1(op, aArg[U15])
      case EQ => Inst3(op, aArg[U15], bArg[Lit], cArg[Lit])
      case GT => Inst3(op, aArg[U15], bArg[Lit], cArg[Lit])
      case JMP => Inst1(op, aArg[Adr])
      case JT => Inst2(op, aArg[Lit], bArg[Adr])
      case JF => Inst2(op, aArg[Lit], bArg[Adr])
      case ADD => Inst3(op, aArg[U15], bArg[Lit], cArg[Lit])
      case MULT => Inst3(op, aArg[U15], bArg[Lit], cArg[Lit])
      case MOD => Inst3(op, aArg[U15], bArg[Lit], cArg[Lit])
      case AND => Inst3(op, aArg[U15], bArg[Lit], cArg[Lit])
      case OR => Inst3(op, aArg[U15], bArg[Lit], cArg[Lit])
      case NOT => Inst2(op, aArg[U15], bArg[Lit])
      case RMEM => Inst2(op, aArg[U15], bArg[Lit])
      case WMEM => Inst2(op, aArg[Adr], bArg[Lit])
      case CALL => Inst1(op, aArg[Adr])
      case RET => Inst0(op)
      case OUT => Inst1(op, aArg[Lit])
      case IN => Inst1(op, aArg[U15])
      case NOOP => Inst0(op)
