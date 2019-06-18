package andxor

import scalaz.{~>, \/}

trait FDecidable[T[_[_]]] extends FContravariant[T] {
  def choose2[Z[_], A1[_], A2[_]](a1: => T[A1], a2: => T[A2])(f: Z ~> Lambda[a => (A1[a] \/ A2[a])]): T[Z]
}

object FDecidable {
  def apply[T[_[_]]](implicit ev: FDecidable[T]): FDecidable[T] = ev
}
