package andxor

import cats.{~>, Applicative, Eq, Id, Monad}
import cats.syntax.apply.*
import org.scalacheck.{Arbitrary, Gen, Prop, PropFromFun, Properties}

object AndXorProperties {
  object arbitrary {
    implicit val optNtArb: Arbitrary[Option ~> Option] =
      Arbitrary(for {
        unit <- Arbitrary.arbitrary[Option[Unit]]
        res = new (Option ~> Option) { def apply[A](o: Option[A]): Option[A] = (o, unit).mapN((a, _) => a) }
      } yield res)

    implicit def arbFMA[F[_[_]], M[_], A[_]](implicit fa: Arbitrary[F[A]], F: FFunctor[F], M: Applicative[M]): Arbitrary[F[[a] =>> M[A[a]]]] =
      Arbitrary(for {
        a <- fa.arbitrary
        res = F.map[A, [a] =>> M[A[a]]](a)(new (A ~> ([a] =>> M[A[a]])) { def apply[a](aa: A[a]): M[A[a]] = M.point(aa) })
      } yield res)
  }

  private def newProperties(name: String)(f: Properties => Unit): Properties = {
    val p = new Properties(name)
    f(p)
    p
  }

  def resizeProp(p: Prop, max: Int): Prop = new PropFromFun(params => p(params.withSize(params.size % (max + 1))))

  object ffunctor {
    def identity[F[_[_]], X[_]](implicit F: FFunctor[F], afx: Arbitrary[F[X]], ef: Eq[F[X]]): Prop =
      Prop.forAll(F.ffunctorLaw.identity[X] _)

    def composite[F[_[_]], X[_], Y[_], Z[_]](implicit F: FFunctor[F], af: Arbitrary[F[X]], axy: Arbitrary[X ~> Y],
                                            ayz: Arbitrary[Y ~> Z], ef: Eq[F[Z]]): Prop =
      Prop.forAll(F.ffunctorLaw.composite[X, Y, Z] _)

    def laws[F[_[_]]](implicit F: FFunctor[F], af: Arbitrary[F[Option]], axy: Arbitrary[Option ~> Option], ef: Eq[F[Option]]): Properties =
      newProperties("ffunctor") { p =>
        p.property.update("identity", identity[F, Option]): Unit
        p.property.update("composite", composite[F, Option, Option, Option]): Unit
      }
  }

  object ftraverse {
    def identity[F[_[_]], TC[_[_]]: FTraverse.LawHelper, X[_], Y[_]](implicit F: FTraverse[F, TC], TCId: TC[Id], afx: Arbitrary[F[X]], axy: Arbitrary[X ~> Y], ef: Eq[F[Y]]): Prop =
      Prop.forAll(F.ftraverseLaw.identity[X, Y] _)

    def purity[F[_[_]], TC[_[_]]: FTraverse.LawHelper, G[_], X[_]](implicit F: FTraverse[F, TC], afx: Arbitrary[F[X]], G: TC[G], AG: Applicative[G], ef: Eq[G[F[X]]]): Prop =
      Prop.forAll(F.ftraverseLaw.purity[G, X] _)

    def sequentialFusion[F[_[_]], TC[_[_]]: FTraverse.LawHelper, N[_], M[_], A[_], B[_], C[_]](
        implicit fa: Arbitrary[F[A]], amb: Arbitrary[A ~> ([a] =>> M[B[a]])], bnc: Arbitrary[B ~> ([a] =>> N[C[a]])],
        F: FTraverse[F, TC], N: TC[N], M: TC[M], MN: Eq[M[N[F[C]]]]): Prop =
      Prop.forAll(F.ftraverseLaw.sequentialFusion[N, M, A, B, C] _)

    def naturality[F[_[_]], TC[_[_]]: FTraverse.LawHelper, N[_], M[_], A[_]](nat: (M ~> N))
        (implicit fma: Arbitrary[F[[a] =>> M[A[a]]]], F: FTraverse[F, TC], N: TC[N], M: TC[M], NFA: Eq[N[F[A]]]): Prop =
      Prop.forAll(F.ftraverseLaw.naturality[N, M, A](nat) _)

    def parallelFusion[F[_[_]], TC[_[_]]: FTraverse.LawHelper, N[_], M[_], A[_], B[_]](
        implicit fa: Arbitrary[F[A]], amb: Arbitrary[A ~> ([a] =>> M[B[a]])], anb: Arbitrary[A ~> ([a] =>> N[B[a]])],
        F: FTraverse[F, TC], N: TC[N], M: TC[M], MN: Eq[(M[F[B]], N[F[B]])]): Prop =
      Prop.forAll(F.ftraverseLaw.parallelFusion[N, M, A, B] _)

    def laws[F[_[_]], TC[_[_]]](
      implicit fa: Arbitrary[F[Option]],
      F: FTraverse[F, TC],
      FH: FTraverse.LawHelper[TC],
      TCL: TC[List],
      TCV: TC[Vector],
      TCId: TC[Id],
      EF: Eq[F[Option]]
    ): Properties =
      newProperties("ftraverse") { p =>
        import arbitrary.*

        p.include(ffunctor.laws[F])
        p.property.update("identity", identity[F, TC, Option, Option]): Unit
        p.property.update("purity.list", purity[F, TC, List, Option]): Unit
        p.property.update("purity.stream", purity[F, TC, Vector, Option]): Unit
        p.property.update("sequential fusion", sequentialFusion[F, TC, Id, Id, Option, Option, Option]): Unit
        p.property.update("naturality", naturality[F, TC, Vector, List, Option](
          new (List ~> Vector) { def apply[A](l: List[A]): Vector[A] = l.toVector })): Unit
        p.property.update("parallel fusion", parallelFusion[F, TC, Id, Id, Option, Option]): Unit
      }
  }
}
