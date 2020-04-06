package andxor.types

import cats.Eq

trait ADT {
  @newtype case class ADTValue[A <: Singleton](value: A)
  object ADTValue {
    implicit def eq[A <: Singleton]: Eq[ADTValue[A]] = Eq.instance((a1, a2) => a1.value == a2.value)
  }
}
