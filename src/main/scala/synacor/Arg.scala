package synacor

import numbers.*

enum Arg[A <: U15]:
  case RegRef[A <: U15](reg: Reg) extends Arg[A]
  case Const[A <: U15](v: A) extends Arg[A]

  def toReg: Reg = this match
    case RegRef(reg) => reg
    case Const(v) => v.reg // should fail

  def value(using Registers, Cast[A]): A = this match
    case ref@RegRef(reg) => Cast[A].cast(ref.deref(summon[Registers]))
    case Const(v) => v

  def toRegOption: Option[Reg] = this match
    case RegRef(reg) => Some(reg)
    case Const(v) => None

object Arg:
  extension (regRef: RegRef[?])
    def deref(registers: Registers): U15 = // TODO: Use Cast
      registers(regRef.reg)

  given [A <: U15 : Cast : Show](using Registers): Show[Arg[A]] with
    override def show(arg: Arg[A]) = arg match
      case ref@Arg.RegRef(reg) => Show[RegRef[A]].show(ref)
      case const@Arg.Const(v) => Show[Const[A]].show(const)

  given [A <: U15](using Cast[A], Show[A]): Show[Const[A]] with
    override def show(const: Arg.Const[A]) = Show[A].show(cast(const.v))

  given [A <: U15](using registers: Registers)(using Cast[A], Show[A]): Show[RegRef[A]] with
    override def show(regRef: Arg.RegRef[A]): String =
      val a: A = cast[A](regRef.deref(registers))
      s"${regRef.reg.name}(${Show[A].show(a)})"

  def fromWord[A <: U15 : Cast](w: Word): Arg[A] =
    if w.fitsU15 then Const(Cast[A].cast(w))
    else RegRef(w.reg)

given Cast[Lit] with
  override def cast(w: Word): Lit = w.lit

given Cast[Adr] with
  override def cast(w: Word): Adr = w.adr

given Cast[U15] with
  override def cast(w: Word): U15 = w.u15

given Cast[Char] with
  override def cast(w: Word): Char = w.asChar

given Show[Lit] with
  def show(a: Lit): String = a.hex

given Show[Adr] with
  def show(a: Adr): String = s"&${a.hex}"
