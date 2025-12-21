package synacor

import numbers.*

sealed trait Arg:
  override def toString: String = this match
    case RegArg(reg) => reg.name
    case LitVal(value) => value.show
    case LitRef(reg) => reg.name
    case AdrVal(value) => value.show
    case AdrRef(reg) => reg.name

object Arg:
  given (using Registers): Show[Arg] with
    def show(arg: Arg): String = arg match
      case RegArg(reg) => reg.name
      case litArg: LitArg => litArg match
        case LitVal(value) => value.show
        case ref@LitRef(reg) => s"${reg.name}(${ref.value.show})"
      case adrArg: AdrArg => adrArg match
        case AdrVal(value) => value.show
        case ref@AdrRef(reg) => s"${reg.name}(${ref.value.show})"

sealed trait LitArg extends Arg:
  def value(using registers: Registers): Lit = this match
    case LitVal(value) => value
    case LitRef(reg) => registers(reg).lit

object LitArg:
  def fromWord(w: Word): LitArg =
    if w.fitsU15 then LitVal(w.lit)
    else LitRef(w.reg)

sealed trait AdrArg extends Arg:
  def value(using registers: Registers): Adr = this match
    case AdrVal(value) => value
    case AdrRef(reg) => registers(reg).adr

object AdrArg:
  def fromWord(w: Word): AdrArg =
    if w.fitsU15 then AdrVal(w.adr)
    else AdrRef(w.reg)

import synacor.numbers.{Reg, Lit, Adr}

case class RegArg(reg: Reg) extends Arg:
  def value(using registers: Registers): U15 =
    registers(reg)

case class LitVal(value: Lit) extends LitArg
case class LitRef(reg: Reg) extends LitArg
case class AdrVal(value: Adr) extends AdrArg
case class AdrRef(reg: Reg) extends AdrArg
