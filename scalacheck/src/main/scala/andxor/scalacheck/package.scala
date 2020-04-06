package andxor

import andxor.types.ADTValue
import cats.Monad
import org.scalacheck.{Arbitrary, Gen}

package object scalacheck {
  implicit val arbitraryInst: Monad[Arbitrary] with Alt[Arbitrary] = new Monad[Arbitrary] with Alt[Arbitrary] {
    def alt[A](a1: Arbitrary[A], a2: Arbitrary[A]): Arbitrary[A] = Arbitrary(Gen.oneOf(a1.arbitrary, a2.arbitrary))
    def flatMap[A, B](fa: Arbitrary[A])(f: A => Arbitrary[B]) = Arbitrary(fa.arbitrary.flatMap(f(_).arbitrary))
    def pure[A](a: A) = Arbitrary(Gen.sized(_ => Gen.const(a)))
    def tailRecM[A, B](a: A)(f: A => Arbitrary[Either[A, B]]): Arbitrary[B] =
      Arbitrary(f(a).arbitrary.flatMap(_.fold(tailRecM(_)(f), pure).arbitrary))
  }

  implicit def arbAdtVal[A <: Singleton](implicit a: ADTValue[A]): Arbitrary[ADTValue[A]] = arbitraryInst.point(a)
}
