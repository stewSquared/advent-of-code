package synacor

trait Show[A]:
  def show(a: A): String

object Show:
  def apply[A](using Show[A]) = summon[Show[A]]

extension[A : Show](a: A) def show: String = Show[A].show(a)
