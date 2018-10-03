package andxor
import scala.language.higherKinds
import scalaz.{Apply, Monoid, \/, ~>}
import scalaz.Id.Id
import scalaz.syntax.either._

trait AndXorK2[F[_], A1, A2] extends AndXor {
  type Prod = (F[A1], F[A2])
  type Cop = (F[A1] \/ F[A2])
  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose2(a0, a1)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly2(a0, a1)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide2(a0, a1)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply2(a0, a1)((i0, i1) => f((i0, i1)))
    }

  object instances {

    implicit val inja0: Inj[Cop, F[A1]] =
      Inj.instance(_.left[F[A2]])

    implicit val inja1: Inj[Cop, F[A2]] =
      Inj.instance(_.right[F[A1]])

    implicit def lifta0(implicit M: Monoid[Prod]): Inj[Prod, F[A1]] = {
      val (_, a0) =
        M.zero
      Inj.instance((_, a0))
    }

    implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, F[A2]] = {
      val (a0, _) =
        M.zero
      Inj.instance((a0, _))
    }

  }

  import instances._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK2[F, A1, A2]#Prod => AndXorK2[G, A1, A2]#Prod =
    (p: AndXorK2[F, A1, A2]#Prod) => (nt(p._1), nt(p._2))

  def transformC[G[_]](nt: (F ~> G)): AndXorK2[F, A1, A2]#Cop => AndXorK2[G, A1, A2]#Cop =
    (p: AndXorK2[F, A1, A2]#Cop) => p.bimap(nt(_), nt(_))
}

object AndXorK2 {

  def apply[F[_], A1, A2]: AndXorK2[F, A1, A2] =
    new AndXorK2[F, A1, A2] {}
}

trait AndXorF2[A1, A2] {
  type Repr[F[_]] = AndXorK2[F, A1, A2]
  def apply[F[_]]: Repr[F] =
    new AndXorK2[F, A1, A2] {}
}

object AndXorF2 {
  def apply[A1, A2]: AndXorF2[A1, A2] =
    new AndXorF2[A1, A2] {}
}

trait AndXor2[A1, A2] extends AndXorK2[Id, A1, A2]

object AndXor2 {
  def apply[A1, A2]: AndXor2[A1, A2] =
    new AndXor2[A1, A2] {}
}
