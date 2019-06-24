package andxor

import org.scalacheck.{Arbitrary, Gen, Prop, PropFromFun, Properties}
import scalaz.{~>, Applicative, Equal, Monad}
import scalaz.Id.Id
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.std.option._
import scalaz.syntax.apply._

object AndXorProperties {
  object arbitrary {
    implicit val altArbitrary: Alt[Arbitrary] = new Alt[Arbitrary] {
      def alt[A](a1: => Arbitrary[A], a2: => Arbitrary[A]): Arbitrary[A] = Arbitrary(Gen.oneOf(a1.arbitrary, a2.arbitrary))
      def point[A](a: => A): Arbitrary[A] = Arbitrary(Gen.const(a))
      def ap[A, B](fa: => Arbitrary[A])(f: => Arbitrary[A => B]): Arbitrary[B] =
        Monad[Arbitrary].ap(fa)(f)
    }

    implicit val optNtArb: Arbitrary[Option ~> Option] =
      Arbitrary(for {
        unit <- Arbitrary.arbitrary[Option[Unit]]
        res = new (Option ~> Option) { def apply[A](o: Option[A]): Option[A] = (o |@| unit)((a, _) => a) }
      } yield res)

    implicit def arbFMA[F[_[_]], M[_], A[_]](implicit fa: Arbitrary[F[A]], F: FFunctor[F], M: Applicative[M]): Arbitrary[F[Lambda[a => M[A[a]]]]] =
      Arbitrary(for {
        a <- fa.arbitrary
        res = F.map[A, Lambda[a => M[A[a]]]](a)(new (A ~> Lambda[a => M[A[a]]]) { def apply[a](aa: A[a]): M[A[a]] = M.point(aa) })
      } yield res)
  }

  private def newProperties(name: String)(f: Properties => Unit): Properties = {
    val p = new Properties(name)
    f(p)
    p
  }

  def resizeProp(p: Prop, max: Int): Prop = new PropFromFun(params => p(params.withSize(params.size % (max + 1))))

  object ffunctor {
    def identity[F[_[_]], X[_]](implicit F: FFunctor[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]): Prop =
      Prop.forAll(F.ffunctorLaw.identity[X] _)

    def composite[F[_[_]], X[_], Y[_], Z[_]](implicit F: FFunctor[F], af: Arbitrary[F[X]], axy: Arbitrary[X ~> Y],
                                            ayz: Arbitrary[Y ~> Z], ef: Equal[F[Z]]): Prop =
      Prop.forAll(F.ffunctorLaw.composite[X, Y, Z] _)

    def laws[F[_[_]]](implicit F: FFunctor[F], af: Arbitrary[F[Option]], axy: Arbitrary[Option ~> Option], ef: Equal[F[Option]]): Properties =
      newProperties("ffunctor") { p =>
        p.property("identity") = identity[F, Option]
        p.property("composite") = composite[F, Option, Option, Option]
        ()
      }
  }

  object ftraverse {
    def identity[F[_[_]], X[_], Y[_]](implicit F: FTraverse[F], afx: Arbitrary[F[X]], axy: Arbitrary[X ~> Y], ef: Equal[F[Y]]): Prop =
      Prop.forAll(F.ftraverseLaw.identity[X, Y] _)

    def purity[F[_[_]], G[_], X[_]](implicit F: FTraverse[F], afx: Arbitrary[F[X]], G: Applicative[G], ef: Equal[G[F[X]]]): Prop =
      Prop.forAll(F.ftraverseLaw.purity[G, X] _)

    def sequentialFusion[F[_[_]], N[_], M[_], A[_], B[_], C[_]](
        implicit fa: Arbitrary[F[A]], amb: Arbitrary[A ~> Lambda[a => M[B[a]]]], bnc: Arbitrary[B ~> Lambda[a => N[C[a]]]],
        F: FTraverse[F], N: Applicative[N], M: Applicative[M], MN: Equal[M[N[F[C]]]]): Prop =
      Prop.forAll(F.ftraverseLaw.sequentialFusion[N, M, A, B, C] _)

    def naturality[F[_[_]], N[_], M[_], A[_]](nat: (M ~> N))
        (implicit fma: Arbitrary[F[Lambda[a => M[A[a]]]]], F: FTraverse[F], N: Applicative[N], M: Applicative[M], NFA: Equal[N[F[A]]]): Prop =
      Prop.forAll(F.ftraverseLaw.naturality[N, M, A](nat) _)

    def parallelFusion[F[_[_]], N[_], M[_], A[_], B[_]](
        implicit fa: Arbitrary[F[A]], amb: Arbitrary[A ~> Lambda[a => M[B[a]]]], anb: Arbitrary[A ~> Lambda[a => N[B[a]]]],
        F: FTraverse[F], N: Applicative[N], M: Applicative[M], MN: Equal[(M[F[B]], N[F[B]])]): Prop =
      Prop.forAll(F.ftraverseLaw.parallelFusion[N, M, A, B] _)

    def laws[F[_[_]]](implicit fa: Arbitrary[F[Option]], F: FTraverse[F], EF: Equal[F[Option]]): Properties =
      newProperties("ftraverse") { p =>
        import arbitrary._
        import scalaz.std.list._
        import scalaz.std.tuple._
        import scalaz.std.vector._

        p.include(ffunctor.laws[F])
        p.property("identity") = identity[F, Option, Option]
        p.property("purity.list") = purity[F, List, Option]
        p.property("purity.stream") = purity[F, Vector, Option]
        p.property("sequential fusion") = sequentialFusion[F, Id, Id, Option, Option, Option]
        p.property("naturality") = naturality[F, Vector, List, Option](
          new (List ~> Vector) { def apply[A](l: List[A]): Vector[A] = l.toVector })
        p.property("parallel fusion") = parallelFusion[F, Id, Id, Option, Option]

        ()
      }
  }
}
