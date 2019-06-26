package andxor

import scalaz.{~>, Applicative, Equal}
import scalaz.Id.Id

trait FTraverse0[T[_[_]]] extends FFunctor[T] {
  def traverse[F[_], G[_], A[_]: Applicative](tf: T[F])(f: F ~> Lambda[a => A[G[a]]]): A[T[G]]
  def sequence[F[_], A[_]: Applicative](taf: T[Lambda[a => A[F[a]]]]): A[T[F]]

  trait FTraverseLaw extends FFunctorLaw {
    def identity[A[_], B[_]](ta: T[A], f: A ~> B)(implicit TB: Equal[T[B]]): Boolean =
      TB.equal(traverse[A, B, Id](ta)(f), map(ta)(f))

    def sequentialFusion[N[_], M[_], A[_], B[_], C[_]](ta: T[A], amb: A ~> Lambda[a => M[B[a]]], bnc: B ~> Lambda[a => N[C[a]]])(
        implicit N: Applicative[N], M: Applicative[M], MN: Equal[M[N[T[C]]]]): Boolean = {
      type MN[X] = M[N[X]]
      val t1: MN[T[C]] = M.map(traverse[A, B, M](ta)(amb))(tb => traverse[B, C, N](tb)(bnc))
      val t2: MN[T[C]] = traverse[A, C, MN](ta)(new (A ~> Lambda[a => MN[C[a]]]) {
        def apply[a](aa: A[a]): MN[C[a]] = M.map(amb(aa))(bnc(_))
      })(M.compose(N))
      MN.equal(t1, t2)
    }

    def purity[G[_], A[_]](ta: T[A])(implicit G: Applicative[G], GTA: Equal[G[T[A]]]): Boolean =
      GTA.equal(
        traverse[A, A, G](ta)(new (A ~> Lambda[a => G[A[a]]]) { def apply[a](aa: A[a]): G[A[a]] = G.point(aa) }),
        G.point(ta))

    def naturality[N[_], M[_], A[_]](nat: M ~> N)(tma: T[Lambda[a => M[A[a]]]])(implicit N: Applicative[N], M: Applicative[M], NTA: Equal[N[T[A]]]): Boolean =
      NTA.equal(
        nat[T[A]](sequence[A, M](tma)),
        sequence[A, N](map[Lambda[a => M[A[a]]], Lambda[a => N[A[a]]]](tma)(new (Lambda[a => M[A[a]]] ~> Lambda[a => N[A[a]]]) {
          def apply[a](maa: M[A[a]]): N[A[a]] = nat(maa)
        })))

    def parallelFusion[N[_], M[_], A[_], B[_]](ta: T[A], amb: A ~> Lambda[a => M[B[a]]], anb: A ~> Lambda[a => N[B[a]]])(
        implicit N: Applicative[N], M: Applicative[M], MN: Equal[(M[T[B]], N[T[B]])]): Boolean = {
      type MN[X] = (M[X], N[X])
      val t1: MN[T[B]] = (traverse[A, B, M](ta)(amb), traverse[A, B, N](ta)(anb))
      val t2: MN[T[B]] = traverse[A, B, MN](ta)(new (A ~> Lambda[a => MN[B[a]]]) {
        def apply[a](aa: A[a]): MN[B[a]] = (amb(aa), anb(aa))
      })(M.product(N))
      MN.equal(t1, t2)
    }
  }
  def ftraverseLaw = new FTraverseLaw {}
}

trait FTraverse[T[_[_]]] extends FTraverse0[T] {
  def sequence[F[_], A[_]: Applicative](taf: T[Lambda[a => A[F[a]]]]): A[T[F]] =
    traverse[Lambda[a => A[F[a]]], F, A](taf)(new (Lambda[a => A[F[a]]] ~> Lambda[a => A[F[a]]]) {
      def apply[a](a: A[F[a]]): A[F[a]] = a
    })
}

trait FSequence[T[_[_]]] extends FTraverse0[T] {
  def traverse[F[_], G[_], A[_]: Applicative](tf: T[F])(f: F ~> Lambda[a => A[G[a]]]): A[T[G]] =
    sequence(map[F, Lambda[a => A[G[a]]]](tf)(f))
}

trait FTraverse0Companion { def apply[T[_[_]]](implicit ev: FTraverse0[T]): FTraverse0[T] = ev }
object FTraverse extends FTraverse0Companion
object FSequence extends FTraverse0Companion
