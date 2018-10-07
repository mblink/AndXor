package andxor

import scala.language.higherKinds
import scalaz.{Foldable, ApplicativePlus}

trait Uncons[F[_]] {
  def apply[A](fa: F[A]): (Option[A], F[A])
}

trait LPUncons {

  implicit def unconsFAS[F[_]](implicit F0: Foldable[F], AP: ApplicativePlus[F]):
  Uncons[F] =
    new Uncons[F] {
      def apply[A](fa: F[A]) =
        F0.foldLeft(fa, (0, (Option.empty[A], AP.empty[A])))((a, e) => a._1 match {
          case 0 => (1, (Some(e), a._2._2))
          case x => (x + 1, (a._2._1, AP.plus(a._2._2, AP.point(e))))
        })._2
    }
}

object Uncons extends LPUncons {

  implicit val unconsList: Uncons[List] = new Uncons[List] {
    def apply[A](fa: List[A]) = fa match {
      case h :: t => (Some(h), t)
      case Nil => (None, List.empty[A])
    }
  }
}
