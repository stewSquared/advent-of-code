package synacor

import numbers.Word

trait Cast[A]:
  def cast(w: Word): A

object Cast:
  def apply[A](using Cast[A]) = summon[Cast[A]]

def cast[A : Cast](w: Word): A = Cast[A].cast(w)
