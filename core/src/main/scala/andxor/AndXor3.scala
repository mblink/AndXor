package andxor
import scala.language.higherKinds
import scalaz.{Apply, Monoid, \/, ~>}
import scalaz.Id.Id
import scalaz.syntax.either._

trait AndXorK3[F[_], A1, A2, A3] extends AndXor {
  type Prod = (F[A1], F[A2], F[A3])
  type Cop = (F[A1] \/ (F[A2] \/ F[A3]))
  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose3(a0, a1, a2)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly3(a0, a1, a2)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide3(a0, a1, a2)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply3(a0, a1, a2)((i0, i1, i2) => f((i0, i1, i2)))
    }

  object instances {

    implicit val inja0: Inj[Cop, F[A1]] =
      Inj.instance(_.left[(F[A2] \/ F[A3])])

    implicit val inja1: Inj[Cop, F[A2]] =
      Inj.instance(_.left[F[A3]].right[F[A1]])

    implicit val inja2: Inj[Cop, F[A3]] =
      Inj.instance(_.right[F[A2]].right[F[A1]])

    implicit def lifta0(implicit M: Monoid[Prod]): Inj[Prod, F[A1]] = {
      val (_, a0, a1) =
        M.zero
      Inj.instance((_, a0, a1))
    }

    implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, F[A2]] = {
      val (a0, _, a1) =
        M.zero
      Inj.instance((a0, _, a1))
    }

    implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, F[A3]] = {
      val (a0, a1, _) =
        M.zero
      Inj.instance((a0, a1, _))
    }

  }

  import instances._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK3[F, A1, A2, A3]#Prod => AndXorK3[G, A1, A2, A3]#Prod =
    (p: AndXorK3[F, A1, A2, A3]#Prod) => (nt(p._1), nt(p._2), nt(p._3))

  def transformC[G[_]](nt: (F ~> G)): AndXorK3[F, A1, A2, A3]#Cop => AndXorK3[G, A1, A2, A3]#Cop =
    (p: AndXorK3[F, A1, A2, A3]#Cop) => p.bimap(nt(_), _.bimap(nt(_), nt(_)))
}

object AndXorK3 {

  def apply[F[_], A1, A2, A3]: AndXorK3[F, A1, A2, A3] =
    new AndXorK3[F, A1, A2, A3] {}
}

trait AndXorF3[A1, A2, A3] {
  type Repr[F[_]] = AndXorK3[F, A1, A2, A3]
  def apply[F[_]]: Repr[F] =
    new AndXorK3[F, A1, A2, A3] {}
}

object AndXorF3 {
  def apply[A1, A2, A3]: AndXorF3[A1, A2, A3] =
    new AndXorF3[A1, A2, A3] {}
}

trait AndXor3[A1, A2, A3] extends AndXorK3[Id, A1, A2, A3]

object AndXor3 {
  def apply[A1, A2, A3]: AndXor3[A1, A2, A3] =
    new AndXor3[A1, A2, A3] {}
}
