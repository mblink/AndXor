package andxor

import cats.Eq

object eq {
  val eqDeriving: AndXorIso.DerivingLabelledContravariant[Eq] = new AndXorIso.DerivingLabelledContravariant[Eq] {}
  implicit def eqToDeriving(@annotation.unused a: Eq.type): eqDeriving.type = eqDeriving
}
