package synacor

import numbers.*

enum Arg:
  case RegRef(reg: Reg) extends Arg
  case Const(v: U15) extends Arg // type parameter

  def toReg: Reg = this match
    case RegRef(reg) => reg
    case Const(v) => v.reg // should fail

  def toRegOption: Option[Reg] = this match
    case RegRef(reg) => Some(reg)
    case Const(v) => None

object Arg:
  extension (regRef: RegRef)
    def deref(registers: Registers): U15 =
      registers(regRef.reg)

  def fromWord(w: Word): Arg =
    if w.fitsU15 then Const(w.u15)
    else RegRef(w.reg)

  // def toString = this match
  //   case RegRef(reg) =>
  //   case Const(a) =>

// object Arg:
//   given showRegRef[A](using registers: Registers, showA: Show[A]): Show[RegRef[A]] =
//     new Show[RegRef[A]] {
//       def show(regRef: RegRef[A]): String =
//         val a: A = registers(regRef.reg)
//         s"${regRef.name}(${showA(a)})"
//     }

  // def showInstance(a: Arg)(using registers: Registers): Show[Arg] = a match
  //   case RegRef[A](reg) =>
  //     val
  //     s"${reg.name}(${register(reg).hex})"
  //   case Const(a) =>


case class Inst(op: Opcode, args: List[Arg])

case class Inst0(op: Opcode)
case class Inst1(op: Opcode, a: Arg)
case class Inst2(op: Opcode, a: Arg, b: Arg)
case class Inst3(op: Opcode, a: Arg, b: Arg, c: Arg)

trait Show[A]:
  def show(a: A): String

object Show:
  def apply[A](using Show[A]) = summon[Show[A]]

def show[A : Show](a: A): String = Show[A].show(a)
