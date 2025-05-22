package andxor

import cats.{~>, Applicative, Apply, Eq, Functor, Id}

trait FTraverse[T[_[_]], TC[_[_]]] extends FFunctor[T] {
  def traverse[F[_], G[_], A[_]](tf: T[F])(f: F ~> ([a] =>> A[G[a]]))(using tc: TC[A]): A[T[G]]

  final def sequence[F[_], A[_]: TC](taf: T[[a] =>> A[F[a]]]): A[T[F]] =
    traverse[[a] =>> A[F[a]], F, A](taf)(new (([a] =>> A[F[a]]) ~> ([a] =>> A[F[a]])) {
      def apply[a](a: A[F[a]]): A[F[a]] = a
    })

  trait FTraverseLaw extends FFunctorLaw {
    protected val H: FTraverse.LawHelper[TC]

    final def identity[A[_], B[_]](ta: T[A], f: A ~> B)(using TB: Eq[T[B]], TCId: TC[Id]): Boolean =
      TB.eqv(traverse[A, B, Id](ta)(f), map(ta)(f))

    final def sequentialFusion[N[_], M[_], A[_], B[_], C[_]](ta: T[A], amb: A ~> ([a] =>> M[B[a]]), bnc: B ~> ([a] =>> N[C[a]]))(
        using N: TC[N],
        M: TC[M],
        MN: Eq[M[N[T[C]]]]
      ): Boolean = {
      type MN[X] = M[N[X]]
      val FM = H.toFunctor[M](M)
      val t1: MN[T[C]] = FM.map(traverse[A, B, M](ta)(amb))(tb => traverse[B, C, N](tb)(bnc))

      given TCMN: TC[MN] = H.compose(M, N)
      val t2: MN[T[C]] = traverse[A, C, MN](ta)(new (A ~> ([a] =>> MN[C[a]])) {
        def apply[a](aa: A[a]): MN[C[a]] = FM.map(amb(aa))(bnc(_))
      })

      MN.eqv(t1, t2)
    }

    final def purity[G[_]: Applicative, A[_]](ta: T[A])(using G: TC[G], GTA: Eq[G[T[A]]]): Boolean =
      GTA.eqv(
        traverse[A, A, G](ta)(new (A ~> ([a] =>> G[A[a]])) { def apply[a](aa: A[a]): G[A[a]] = Applicative[G].point(aa) }),
        Applicative[G].point(ta))

    final def naturality[N[_], M[_], A[_]](nat: M ~> N)(tma: T[[a] =>> M[A[a]]])(using N: TC[N], M: TC[M], NTA: Eq[N[T[A]]]): Boolean =
      NTA.eqv(
        nat[T[A]](sequence[A, M](tma)),
        sequence[A, N](map[[a] =>> M[A[a]], [a] =>> N[A[a]]](tma)(new (([a] =>> M[A[a]]) ~> ([a] =>> N[A[a]])) {
          def apply[a](maa: M[A[a]]): N[A[a]] = nat(maa)
        })))

    final def parallelFusion[N[_], M[_], A[_], B[_]](ta: T[A], amb: A ~> ([a] =>> M[B[a]]), anb: A ~> ([a] =>> N[B[a]]))(
        using N: TC[N], M: TC[M], MN: Eq[(M[T[B]], N[T[B]])]): Boolean = {
      type MN[X] = (M[X], N[X])
      val t1: MN[T[B]] = (traverse[A, B, M](ta)(amb), traverse[A, B, N](ta)(anb))

      given TCMN: TC[MN] = H.product(M, N)
      val t2: MN[T[B]] = traverse[A, B, MN](ta)(new (A ~> ([a] =>> MN[B[a]])) {
        def apply[a](aa: A[a]): MN[B[a]] = (amb(aa), anb(aa))
      })

      MN.eqv(t1, t2)
    }
  }
  final def ftraverseLaw(implicit LH: FTraverse.LawHelper[TC]): FTraverseLaw = new FTraverseLaw { val H = LH }
}

sealed trait FTraverseInstances
extends FTraverseEitherNInstances
with FTraverseTupleNInstances  {
  private[andxor] trait FX[TC[_[_]], X]
  extends FTraverse[[F[_]] =>> F[X], TC]
  with FFunctor.FX[X] {
    final def traverse[F[_], G[_], A[_]](tf: F[X])(f: F ~> ([a] =>> A[G[a]]))(using @annotation.unused tc: TC[A]): A[G[X]] =
      f(tf)
  }

  private[andxor] trait Tuple1[TC[f[_]] <: Functor[f], X]
  extends FTraverse[[F[_]] =>> F[X] *: EmptyTuple, TC]
  with FFunctor.Tuple1[X] {
    final def traverse[F[_], G[_], A[_]](tf: F[X] *: EmptyTuple)(f: F ~> ([a] =>> A[G[a]]))(using tc: TC[A]): A[G[X] *: EmptyTuple] =
      Functor[A].map(f(tf.head))(_ *: EmptyTuple)
  }

  private[andxor] trait TupleN[TC[f[_]] <: Apply[f], H, T[_[_]] <: Tuple]
  extends FTraverse[[F[_]] =>> F[H] *: T[F], TC]
  with FFunctor.TupleN[H, T] {
    protected val FT: FTraverse[T, TC]
    final def traverse[F[_], G[_], A[_]](tf: F[H] *: T[F])(f: F ~> ([a] =>> A[G[a]]))(using tc: TC[A]): A[G[H] *: T[G]] =
      Apply[A].map2(f(tf.head), FT.traverse(tf.tail)(f))(_ *: _)
  }

  private[andxor] trait Either[TC[f[_]] <: Functor[f], L, R[_[_]]]
  extends FTraverse[[F[_]] =>> F[L] |: R[F], TC]
  with FFunctor.Either[L, R] {
    protected val FR: FTraverse[R, TC]
    final def traverse[F[_], G[_], A[_]](tf: F[L] |: R[F])(f: F ~> ([a] =>> A[G[a]]))(using tc: TC[A]): A[G[L] |: R[G]] =
      tf match {
        case Left(fl) => Functor[A].map(f(fl))(Left(_))
        case Right(rf) => Functor[A].map(FR.traverse(rf)(f))(Right(_))
      }
  }

  // Base case for coproduct derivation
  final given fx[TC[_[_]], X]: FTraverse[[F[_]] =>> F[X], TC] =
    new FX[TC, X] {}

  // Base case for product derivation
  final given tuple1[TC[f[_]] <: Functor[f], X]: FTraverse[[F[_]] =>> F[X] *: EmptyTuple, TC] =
    new Tuple1[TC, X] {}
}

object FTraverse extends FTraverseInstances {
  @inline def apply[T[_[_]], TC[_[_]]](using ev: FTraverse[T, TC]): FTraverse[T, TC] = ev

  private[andxor] sealed trait LawHelper[TC[_[_]]] {
    def toFunctor[F[_]](f: TC[F]): Functor[F]
    def compose[F[_], G[_]](f: TC[F], g: TC[G]): TC[[a] =>> F[G[a]]]
    def product[F[_], G[_]](f: TC[F], g: TC[G]): TC[[a] =>> (F[a], G[a])]
  }

  object LawHelper {
    implicit val functor: LawHelper[Functor] = new LawHelper[Functor] {
      def toFunctor[F[_]](tc: Functor[F]): Functor[F] = tc
      def compose[F[_], G[_]](f: Functor[F], g: Functor[G]): Functor[[a] =>> F[G[a]]] = f.compose(g)
      def product[F[_], G[_]](F: Functor[F], G: Functor[G]): Functor[[a] =>> (F[a], G[a])] = new Functor[[a] =>> (F[a], G[a])] {
        def map[A, B](fa: (F[A], G[A]))(f: A => B): (F[B], G[B]) =
          (F.map(fa._1)(f), G.map(fa._2)(f))
      }
    }

    implicit val apply: LawHelper[Apply] = new LawHelper[Apply] {
      def toFunctor[F[_]](tc: Apply[F]): Functor[F] = tc
      def compose[F[_], G[_]](f: Apply[F], g: Apply[G]): Apply[[a] =>> F[G[a]]] = f.compose(g)
      def product[F[_], G[_]](F: Apply[F], G: Apply[G]): Apply[[a] =>> (F[a], G[a])] = new Apply[[a] =>> (F[a], G[a])] {
        def map[A, B](fa: (F[A], G[A]))(f: A => B): (F[B], G[B]) =
          (F.map(fa._1)(f), G.map(fa._2)(f))

        def ap[A, B](ff: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
          (F.ap(ff._1)(fa._1), G.ap(ff._2)(fa._2))
      }
    }
  }
}

trait FTraverseCompanion[FT[_[_[_]]]] {
  final def apply[T[_[_]]](using ev: FT[T]): FT[T] = ev
}

trait FTraverseFunctor[T[_[_]]] extends FTraverse[T, Functor]
object FTraverseFunctor extends FTraverseCompanion[[T[_[_]]] =>> FTraverse[T, Functor]]

trait FTraverseApply[T[_[_]]] extends FTraverse[T, Apply]
object FTraverseApply extends FTraverseCompanion[[T[_[_]]] =>> FTraverse[T, Apply]]

type FTraverseProd[T[_[_]]] = FTraverseApply[T]
val FTraverseProd = FTraverseApply
type FTraverseCop[T[_[_]]] = FTraverseFunctor[T]
val FTraverseCop = FTraverseFunctor
