package andxor

import org.scalacheck.Arbitrary

trait Apply[F[_]] {
  def ap[A, B](fa: F[A])(fab: F[A => B]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Apply {
  sealed trait CatsApply extends TcHolder1 { type Tc[F[_]] = cats.Apply[F] }
  object CatsApply extends TcHolder1Companion[CatsApply]

  sealed trait ScalazApply extends TcHolder1 { type Tc[F[_]] = scalaz.Apply[F] }
  object ScalazApply extends TcHolder1Companion[ScalazApply]

  implicit def optionalApplyFromCatsApply[F[_], M[_[_]]](implicit ev: GetTc1[CatsApply, M], F: M[F]): Apply[F] = {
    import ev._

    new Apply[F] {
      override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = F.ap(fab)(fa)
      override def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)
    }
  }

  implicit def optionalApplyFromScalazApply[F[_], M[_[_]]](implicit ev: GetTc1[ScalazApply, M], F: M[F]): Apply[F] = {
    import ev._

    new Apply[F] {
      override def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = F.ap(fa)(fab)
      override def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)
    }
  }

  implicit val applyArbitrary: Apply[Arbitrary] = new Apply[Arbitrary] {
    def ap[A, B](fa: Arbitrary[A])(fab: Arbitrary[A => B]): Arbitrary[B] =
      Arbitrary(for {
        f <- fab.arbitrary
        a <- fa.arbitrary
      } yield f(a))

    def map[A, B](fa: Arbitrary[A])(f: A => B): Arbitrary[B] =
      Arbitrary(fa.arbitrary.map(f))
  }
}
