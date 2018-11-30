package andxor

import scala.language.higherKinds

trait Uncons[F[_]] {
  def apply[A](fa: F[A]): (Option[A], F[A])
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
