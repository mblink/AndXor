package andxor

import cats.Show

object show {
  val showDeriving: AndXorIso.DerivingLabelledContravariant[Show] = new AndXorIso.DerivingLabelledContravariant[Show] {}
  implicit def showToDeriving(@annotation.unused a: Show.type): showDeriving.type = showDeriving
}
