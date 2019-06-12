package andxor

import scalaz.Id.Id

trait Uncons[F[_]] {
  def apply[A](fa: F[A]): (Option[A], F[A])
}

trait Uncons0[Prod[_[_]], Cop[_[_]]] {
  def apply[F[_]](p: Prod[F])(implicit U: Uncons[F]): (Option[Cop[Id]], Prod[F])
}

object Uncons {
  implicit val unconsList: Uncons[List] = new Uncons[List] {
    def apply[A](fa: List[A]): (Option[A], List[A]) = fa match {
      case h :: t => (Some(h), t)
      case Nil => (None, List.empty[A])
    }
  }

  implicit val unconsVector: Uncons[Vector] = new Uncons[Vector] {
    def apply[A](fa: Vector[A]): (Option[A], Vector[A]) = fa match {
      case h +: t => (Some(h), t)
      case _ => (None, Vector.empty[A])
    }
  }
}
