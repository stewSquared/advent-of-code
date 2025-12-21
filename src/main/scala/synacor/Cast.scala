package synacor

import numbers.Word

trait Cast[A]:
  def cast(w: Word): A

object Cast:
  def apply[A](using Cast[A]) = summon[Cast[A]]

def cast[A : Cast](w: Word): A = Cast[A].cast(w)

import numbers.*

given Cast[Lit] with
  override def cast(w: Word): Lit = w.lit

given Cast[Adr] with
  override def cast(w: Word): Adr = w.adr

given Cast[U15] with
  override def cast(w: Word): U15 = w.u15

given Cast[Char] with
  override def cast(w: Word): Char = w.asChar
