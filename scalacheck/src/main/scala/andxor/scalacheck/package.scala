package andxor

import andxor.types.ADTValue
import scalaz.Monad
import org.scalacheck.{Arbitrary, Gen}

package object scalacheck {
  implicit val arbitraryInst: Monad[Arbitrary] with Alt[Arbitrary] = new Monad[Arbitrary] with Alt[Arbitrary] {
    def alt[A](a1: => Arbitrary[A], a2: => Arbitrary[A]): Arbitrary[A] = Arbitrary(Gen.oneOf(a1.arbitrary, a2.arbitrary))
    def bind[A, B](fa: Arbitrary[A])(f: A => Arbitrary[B]) = Arbitrary(fa.arbitrary.flatMap(f(_).arbitrary))
    def point[A](a: => A) = Arbitrary(Gen.sized(_ => Gen.const(a)))
    override def map[A, B](fa: Arbitrary[A])(f: A => B) = Arbitrary(fa.arbitrary.map(f))
  }

  implicit def arbAdtVal[A <: Singleton](implicit a: ADTValue[A]): Arbitrary[ADTValue[A]] = arbitraryInst.point(a)
}
