package andxor

import scalaz.Id.Id

trait Uncons[F[_], G[_]] {
  def apply[A](fa: F[A]): (Option[G[A]], F[A])
}

object Uncons {
  implicit val unconsList: Uncons[List, Id] = new Uncons[List, Id] {
    def apply[A](fa: List[A]): (Option[A], List[A]) = fa match {
      case h :: t => (Some(h), t)
      case Nil => (None, List.empty[A])
    }
  }

  implicit val unconsVector: Uncons[Vector, Id] = new Uncons[Vector, Id] {
    def apply[A](fa: Vector[A]): (Option[A], Vector[A]) = fa match {
      case h +: t => (Some(h), t)
      case _ => (None, Vector.empty[A])
    }
  }
}
