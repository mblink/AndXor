package andxor

import cats.{Id, Monad}
import org.scalacheck.{Arbitrary, Gen}
import scala.compiletime.summonAll

package object scalacheck {
  implicit val arbitraryInst: Monad[Arbitrary] with Alt[Arbitrary] = new Monad[Arbitrary] with Alt[Arbitrary] {
    def alt[A](a1: Arbitrary[A], a2: Arbitrary[A]): Arbitrary[A] = Arbitrary(Gen.oneOf(a1.arbitrary, a2.arbitrary))
    def flatMap[A, B](fa: Arbitrary[A])(f: A => Arbitrary[B]) = Arbitrary(fa.arbitrary.flatMap(f(_).arbitrary))
    def pure[A](a: A) = Arbitrary(Gen.sized(_ => Gen.const(a)))
    def tailRecM[A, B](a: A)(f: A => Arbitrary[Either[A, B]]): Arbitrary[B] =
      Arbitrary(f(a).arbitrary.flatMap(_.fold(tailRecM(_)(f), pure).arbitrary))
  }

  inline given singletonInstance[A](using inline v: ValueOf[A]): Arbitrary[A] = Arbitrary(Gen.const(v.value))

  extension (@annotation.unused x: Arbitrary.type) inline def derived[A]: Arbitrary[A] =
    scala.compiletime.summonFrom {
      case p: AndXorProdIso[A] =>
        given axoInstances: AndXorInstances[Arbitrary, p.Prod[Id]] =
          AndXorInstances(summonAll[Tuple.Map[p.Prod[Id], Arbitrary]])

        p.deriving[Arbitrary].apply

      case c: AndXorCopIso[A] =>
        given axoInstances: AndXorInstances[Arbitrary, c.Prod[Id]] =
          AndXorInstances(summonAll[Tuple.Map[c.Prod[Id], Arbitrary]])

        c.deriving[Arbitrary].alt
    }
}
