package andxor

import cats.{~>, Id}
import cats.data.Chain

trait Uncons[F[_], G[_]] {
  def apply[A](fa: F[A]): (Option[G[A]], F[A])
}

object Uncons {
  private def inst[F[_], G[_]](f: F ~> Lambda[a => (Option[G[a]], F[a])]): Uncons[F, G] =
    new Uncons[F, G] {
      def apply[A](fa: F[A]): (Option[G[A]], F[A]) = f(fa)
    }

  implicit val unconsChain: Uncons[Chain, Id] = inst[Chain, Id](Lambda[Chain ~> Lambda[a => (Option[Id[a]], Chain[a])]](_ match {
    case Chain.==:(h, t) => (Some(h), t)
    case _ => (None, Chain.empty)
  }))

  implicit val unconsList: Uncons[List, Id] = inst[List, Id](Lambda[List ~> Lambda[a => (Option[Id[a]], List[a])]](_ match {
    case h :: t => (Some(h), t)
    case Nil => (None, List.empty)
  }))

  implicit val unconsVector: Uncons[Vector, Id] = inst[Vector, Id](Lambda[Vector ~> Lambda[a => (Option[Id[a]], Vector[a])]](_ match {
    case h +: t => (Some(h), t)
    case _ => (None, Vector.empty)
  }))
}
