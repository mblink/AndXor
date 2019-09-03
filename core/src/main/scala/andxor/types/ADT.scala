package andxor.types

import scalaz.Equal

trait ADT {
  @newtype case class ADTValue[A <: Singleton](value: A)
  object ADTValue {
    implicit def equal[A <: Singleton]: Equal[ADTValue[A]] = Equal.equal((a1, a2) => a1.value == a2.value)
  }
}
