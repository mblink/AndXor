package andxor

trait AndXorLift[A] { type AXO <: AndXor }

trait AndXorLiftLP {
  implicit def lp[A]: AndXorLift.Aux[A, AndXor1[A]] = new AndXorLift[A] { type AXO = AndXor1[A] }
}

object AndXorLift extends AndXorLiftLP {
  type Aux[A, AXO0] = AndXorLift[A] { type AXO = AXO0 }

  implicit def hp[A <: AndXor]: AndXorLift.Aux[A, A] = new AndXorLift[A] { type AXO = A }
}
