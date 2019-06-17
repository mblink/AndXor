package andxor

import scalaz.{~>, Const, Monoid}
import scalaz.syntax.monoid._

trait FRepresentable[T[_[_]]] extends FFunctor[T] {
  type FRep[T0[_[_]], A]
  type Product[F[_], G[_], A] = (F[A], G[A])

  def tabulate[F[_]](f: FRep[T, ?] ~> F): T[F]
  def index[F[_]](tf: T[F]): FRep[T, ?] ~> F

  def zipWith[F[_], G[_], H[_]](t: T[F], u: T[G])(f: F ~> Lambda[a => G[a] => H[a]]): T[H] =
    tabulate(new (FRep[T, ?] ~> H) { def apply[a](r: FRep[T, a]): H[a] = f(index(t)(r))(index(u)(r)) })

  def zipWith3[F[_], G[_], H[_], K[_]](t: T[F], u: T[G], v: T[H])(f: F ~> Lambda[a => G[a] => H[a] => K[a]]): T[K] =
    tabulate(new (FRep[T, ?] ~> K) { def apply[a](r: FRep[T, a]): K[a] = f(index(t)(r))(index(u)(r))(index(v)(r)) })

  def zip[F[_], G[_]](t: T[F], u: T[G]): T[Product[F, G, ?]] =
    zipWith[F, G, Product[F, G, ?]](t, u)(new (F ~> Lambda[a => G[a] => Product[F, G, a]]) {
      def apply[a](fa: F[a]): G[a] => Product[F, G, a] = ga => (fa, ga)
    })
}

object FRepresentable {
  type Wrap[T[_[_]], F[_]] = T[F]

  def apply[T[_[_]]](implicit ev: FRepresentable[T]): FRepresentable[T] = ev

  implicit def fRepMonoid[T[_[_]], C: Monoid](implicit T: FRepresentable[T]): Monoid[T[Const[C, ?]]] = new Monoid[T[Const[C, ?]]] {
    def zero: T[Const[C, ?]] =
      T.tabulate(new (T.FRep[T, ?] ~> Const[C, ?]) {
        def apply[a](r: T.FRep[T, a]): Const[C, a] = mzero[Const[C, a]]
      })

    def append(t: T[Const[C, ?]], u: => T[Const[C, ?]]): T[Const[C, ?]] =
      T.zipWith(t, u)(new (Const[C, ?] ~> Lambda[a => Const[C, a] => Const[C, a]]) {
        def apply[a](c1: Const[C, a]): Const[C, a] => Const[C, a] = c2 => c1 |+| c2
      })
  }
}
