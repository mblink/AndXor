package andxor

import scalaz.{~>, Applicative, Equal, Functor}
import scalaz.Id.Id

trait FTraverse0[T[_[_]], TC[_[_]]] extends FFunctor[T] {
  def traverse[F[_], G[_], A[_]: TC](tf: T[F])(f: F ~> Lambda[a => A[G[a]]]): A[T[G]]
  def sequence[F[_], A[_]: TC](taf: T[Lambda[a => A[F[a]]]]): A[T[F]]

  trait FTraverseLaw extends FFunctorLaw {
    def toFunctor[F[_]](f: TC[F]): Functor[F]
    def compose[F[_], G[_]](f: TC[F], g: TC[G]): TC[Lambda[a => F[G[a]]]]
    def product[F[_], G[_]](f: TC[F], g: TC[G]): TC[Lambda[a => (F[a], G[a])]]

    def identity[A[_], B[_]](ta: T[A], f: A ~> B)(implicit TB: Equal[T[B]], TCId: TC[Id]): Boolean =
      TB.equal(traverse[A, B, Id](ta)(f), map(ta)(f))

    def sequentialFusion[N[_], M[_], A[_], B[_], C[_]](ta: T[A], amb: A ~> Lambda[a => M[B[a]]], bnc: B ~> Lambda[a => N[C[a]]])(
        implicit N: TC[N], M: TC[M], MN: Equal[M[N[T[C]]]]): Boolean = {
      type MN[X] = M[N[X]]
      val FM = toFunctor[M](M)
      val t1: MN[T[C]] = FM.map(traverse[A, B, M](ta)(amb))(tb => traverse[B, C, N](tb)(bnc))
      val t2: MN[T[C]] = traverse[A, C, MN](ta)(new (A ~> Lambda[a => MN[C[a]]]) {
        def apply[a](aa: A[a]): MN[C[a]] = FM.map(amb(aa))(bnc(_))
      })(compose(M, N))
      MN.equal(t1, t2)
    }

    def purity[G[_]: Applicative, A[_]](ta: T[A])(implicit G: TC[G], GTA: Equal[G[T[A]]]): Boolean =
      GTA.equal(
        traverse[A, A, G](ta)(new (A ~> Lambda[a => G[A[a]]]) { def apply[a](aa: A[a]): G[A[a]] = Applicative[G].point(aa) }),
        Applicative[G].point(ta))

    def naturality[N[_], M[_], A[_]](nat: M ~> N)(tma: T[Lambda[a => M[A[a]]]])(implicit N: TC[N], M: TC[M], NTA: Equal[N[T[A]]]): Boolean =
      NTA.equal(
        nat[T[A]](sequence[A, M](tma)),
        sequence[A, N](map[Lambda[a => M[A[a]]], Lambda[a => N[A[a]]]](tma)(new (Lambda[a => M[A[a]]] ~> Lambda[a => N[A[a]]]) {
          def apply[a](maa: M[A[a]]): N[A[a]] = nat(maa)
        })))

    def parallelFusion[N[_], M[_], A[_], B[_]](ta: T[A], amb: A ~> Lambda[a => M[B[a]]], anb: A ~> Lambda[a => N[B[a]]])(
        implicit N: TC[N], M: TC[M], MN: Equal[(M[T[B]], N[T[B]])]): Boolean = {
      type MN[X] = (M[X], N[X])
      val t1: MN[T[B]] = (traverse[A, B, M](ta)(amb), traverse[A, B, N](ta)(anb))
      val t2: MN[T[B]] = traverse[A, B, MN](ta)(new (A ~> Lambda[a => MN[B[a]]]) {
        def apply[a](aa: A[a]): MN[B[a]] = (amb(aa), anb(aa))
      })(product(M, N))
      MN.equal(t1, t2)
    }
  }
  def ftraverseLaw: FTraverseLaw
}

trait FTraverse0Companion[FT[_[_[_]]]] {
  def apply[T[_[_]]](implicit ev: FT[T]): FT[T] = ev
}

trait FTraverseFunctor[T[_[_]]] extends FTraverse0[T, Functor] {
  val ftraverseLaw = new FTraverseLaw {
    def toFunctor[F[_]](tc: Functor[F]): Functor[F] = tc
    def compose[F[_], G[_]](f: Functor[F], g: Functor[G]): Functor[Lambda[a => F[G[a]]]] = f.compose(g)
    def product[F[_], G[_]](f: Functor[F], g: Functor[G]): Functor[Lambda[a => (F[a], G[a])]] = f.product(g)
  }
}

trait FTraverseApplicative[T[_[_]]] extends FTraverse0[T, Applicative] {
  val ftraverseLaw = new FTraverseLaw {
    def toFunctor[F[_]](tc: Applicative[F]): Functor[F] = tc
    def compose[F[_], G[_]](f: Applicative[F], g: Applicative[G]): Applicative[Lambda[a => F[G[a]]]] = f.compose(g)
    def product[F[_], G[_]](f: Applicative[F], g: Applicative[G]): Applicative[Lambda[a => (F[a], G[a])]] = f.product(g)
  }
}

trait FTraverse[T[_[_]], TC[_[_]]] extends FTraverse0[T, TC] {
  def sequence[F[_], A[_]: TC](taf: T[Lambda[a => A[F[a]]]]): A[T[F]] =
    traverse[Lambda[a => A[F[a]]], F, A](taf)(new (Lambda[a => A[F[a]]] ~> Lambda[a => A[F[a]]]) {
      def apply[a](a: A[F[a]]): A[F[a]] = a
    })
}
object FTraverse extends FTraverse0Companion[FTraverse[?[_[_]], Applicative]]
object FTraverseFunctor extends FTraverse0Companion[FTraverse[?[_[_]], Functor]]

trait FTraverseProd[T[_[_]]] extends FTraverse[T, Applicative] with FTraverseApplicative[T]
object FTraverseProd extends FTraverse0Companion[FTraverseProd]

trait FTraverseCop[T[_[_]]] extends FTraverse[T, Functor] with FTraverseFunctor[T]
object FTraverseCop extends FTraverse0Companion[FTraverseCop] {
  implicit def FConstFTraverseCop[X]: FTraverseCop[FConst[X]#T] = new FTraverseCop[FConst[X]#T] {
    type T[F[_]] = FConst[X]#T[F]

    def map[A[_], B[_]](fa: T[A])(f: A ~> B): T[B] = f(fa)
    def traverse[F[_], G[_], A[_]: Functor](tf: T[F])(f: F ~> Lambda[a => A[G[a]]]): A[T[G]] =
      map[F, Lambda[a => A[G[a]]]](tf)(f)
  }
}
