package andxor

import cats.{Apply, Id}

trait AndXor0 extends AndXor {
  final type Prod[F[_]] = F[Unit]
  final type Cop[F[_]] = F[Nothing]

  def deriving[TC[_], F[_]](implicit tc: TC[F[Unit]]): AndXorProdDeriving[TC, Prod[F]] =
    new AndXorProdDeriving[TC, Prod[F]] {
      def mkDivide[B](f: B => Prod[F])(implicit a: Divide[TC]): TC[B] = a.contramap(tc)(f)
      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[TC]): TC[B] = a.map(tc)(f)
    }

  def derivingId[TC[_]](implicit tc: TC[Unit]): AndXorProdDeriving[TC, Prod[Id]] = deriving[TC, Id]
}

object AndXor0 extends AndXor0
