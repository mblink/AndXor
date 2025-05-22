package andxor

import cats.{~>, Id}
import cats.data.Chain

trait Uncons[F[_], G[_]] {
  def apply[A](fa: F[A]): (Option[G[A]], F[A])
}

object Uncons {
  private def inst[F[_], G[_]](f: F ~> λ[a => (Option[G[a]], F[a])]): Uncons[F, G] =
    new Uncons[F, G] {
      def apply[A](fa: F[A]): (Option[G[A]], F[A]) = f(fa)
    }

  implicit val unconsChain: Uncons[Chain, Id] = inst[Chain, Id](new (Chain ~> λ[a => (Option[Id[a]], Chain[a])]) {
    def apply[A](x: Chain[A]): (Option[Id[A]], Chain[A]) = x match {
      case Chain.==:(h, t) => (Some(h), t)
      case _ => (None, Chain.empty)
    }
  })

  implicit val unconsList: Uncons[List, Id] = inst[List, Id](new (List ~> λ[a => (Option[Id[a]], List[a])]) {
    def apply[A](x: List[A]): (Option[Id[A]], List[A]) = x match {
      case h :: t => (Some(h), t)
      case Nil => (None, List.empty)
    }
  })

  implicit val unconsVector: Uncons[Vector, Id] = inst[Vector, Id](new (Vector ~> λ[a => (Option[Id[a]], Vector[a])]) {
    def apply[A](x: Vector[A]): (Option[Id[A]], Vector[A]) = x match {
      case h +: t => (Some(h), t)
      case _ => (None, Vector.empty)
    }
  })
}
