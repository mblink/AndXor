package andxor

import cats.Apply

trait AndXorDeriving[TC[_], Cop, Prod] { self =>
  def mkChoose[A](f: A => Cop)(using d: Decidable[TC]): TC[A]
  def mkAlt[A](f: Cop => A)(using a: Alt[TC]): TC[A]
  def mkDivide[A](f: A => Prod)(using a: Divide[TC]): TC[A]
  def mkApply[A](f: Prod => A)(using a: Apply[TC]): TC[A]

  final def choose(using d: Decidable[TC]): TC[Cop] = mkChoose(identity)
  final def alt(using a: Alt[TC]): TC[Cop] = mkAlt(identity)
  final def divide(using d: Divide[TC]): TC[Prod] = mkDivide(identity)
  final def apply(using a: Apply[TC]): TC[Prod] = mkApply(identity)

  final def imapCop[B](cb: Cop => B)(bc: B => Cop): AndXorDeriving[TC, B, Prod] = new AndXorDeriving[TC, B, Prod] {
    def mkChoose[A](f: A => B)(using d: Decidable[TC]): TC[A] = self.mkChoose(a => bc(f(a)))
    def mkAlt[A](f: B => A)(using a: Alt[TC]): TC[A] = self.mkAlt(c => f(cb(c)))
    def mkDivide[A](f: A => Prod)(using a: Divide[TC]): TC[A] = self.mkDivide(f)
    def mkApply[A](f: Prod => A)(using a: Apply[TC]): TC[A] = self.mkApply(f)
  }

  final def imapProd[B](pb: Prod => B)(bp: B => Prod): AndXorDeriving[TC, Cop, B] = new AndXorDeriving[TC, Cop, B] {
    def mkChoose[A](f: A => Cop)(using d: Decidable[TC]): TC[A] = self.mkChoose(f)
    def mkAlt[A](f: Cop => A)(using a: Alt[TC]): TC[A] = self.mkAlt(f)
    def mkDivide[A](f: A => B)(using a: Divide[TC]): TC[A] = self.mkDivide(a => bp(f(a)))
    def mkApply[A](f: B => A)(using a: Apply[TC]): TC[A] = self.mkApply(a => f(pb(a)))
  }
}
