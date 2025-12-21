package synacor

trait Show[-A]:
  def show(a: A): String

object Show:
  def apply[A](using Show[A]) = summon[Show[A]]

extension[A : Show](a: A) def show: String = Show[A].show(a)

import numbers.{Adr, Lit}

given Show[Lit] with
  def show(a: Lit): String = a.hex

given Show[Adr] with
  def show(a: Adr): String = s"${a.hex}"
