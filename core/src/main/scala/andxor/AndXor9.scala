package andxor
import scala.language.higherKinds
import scalaz.{Apply, Functor, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.syntax.either._

trait AndXorK9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9] extends AndXor {
  type Prod = (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9])
  type Cop = (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ F[A9]))))))))
  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]], a3: G[F[A4]], a4: G[F[A5]], a5: G[F[A6]], a6: G[F[A7]], a7: G[F[A8]], a8: G[F[A9]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose9(a0, a1, a2, a3, a4, a5, a6, a7, a8)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly9(a0, a1, a2, a3, a4, a5, a6, a7, a8)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide9(a0, a1, a2, a3, a4, a5, a6, a7, a8)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply9(a0, a1, a2, a3, a4, a5, a6, a7, a8)((i0, i1, i2, i3, i4, i5, i6, i7, i8) => f((i0, i1, i2, i3, i4, i5, i6, i7, i8)))
    }

  object instances {

    implicit val inja0: Inj[Cop, F[A1]] =
      Inj.instance(_.left[(F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ F[A9])))))))])

    implicit val inja1: Inj[Cop, F[A2]] =
      Inj.instance(_.left[(F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ F[A9]))))))].right[F[A1]])

    implicit val inja2: Inj[Cop, F[A3]] =
      Inj.instance(_.left[(F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ F[A9])))))].right[F[A2]].right[F[A1]])

    implicit val inja3: Inj[Cop, F[A4]] =
      Inj.instance(_.left[(F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ F[A9]))))].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja4: Inj[Cop, F[A5]] =
      Inj.instance(_.left[(F[A6] \/ (F[A7] \/ (F[A8] \/ F[A9])))].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja5: Inj[Cop, F[A6]] =
      Inj.instance(_.left[(F[A7] \/ (F[A8] \/ F[A9]))].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja6: Inj[Cop, F[A7]] =
      Inj.instance(_.left[(F[A8] \/ F[A9])].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja7: Inj[Cop, F[A8]] =
      Inj.instance(_.left[F[A9]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja8: Inj[Cop, F[A9]] =
      Inj.instance(_.right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit def lifta0(implicit M: Monoid[Prod]): Inj[Prod, F[A1]] = {
      val (_, a0, a1, a2, a3, a4, a5, a6, a7) =
        M.zero
      Inj.instance((_, a0, a1, a2, a3, a4, a5, a6, a7))
    }

    implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, F[A2]] = {
      val (a0, _, a1, a2, a3, a4, a5, a6, a7) =
        M.zero
      Inj.instance((a0, _, a1, a2, a3, a4, a5, a6, a7))
    }

    implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, F[A3]] = {
      val (a0, a1, _, a2, a3, a4, a5, a6, a7) =
        M.zero
      Inj.instance((a0, a1, _, a2, a3, a4, a5, a6, a7))
    }

    implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, F[A4]] = {
      val (a0, a1, a2, _, a3, a4, a5, a6, a7) =
        M.zero
      Inj.instance((a0, a1, a2, _, a3, a4, a5, a6, a7))
    }

    implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, F[A5]] = {
      val (a0, a1, a2, a3, _, a4, a5, a6, a7) =
        M.zero
      Inj.instance((a0, a1, a2, a3, _, a4, a5, a6, a7))
    }

    implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, F[A6]] = {
      val (a0, a1, a2, a3, a4, _, a5, a6, a7) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, _, a5, a6, a7))
    }

    implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, F[A7]] = {
      val (a0, a1, a2, a3, a4, a5, _, a6, a7) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, _, a6, a7))
    }

    implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, F[A8]] = {
      val (a0, a1, a2, a3, a4, a5, a6, _, a7) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, _, a7))
    }

    implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, F[A9]] = {
      val (a0, a1, a2, a3, a4, a5, a6, a7, _) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, _))
    }

  }

  import instances._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]#Prod => AndXorK9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9]#Prod =
    (p: AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]#Prod) => (nt(p._1), nt(p._2), nt(p._3), nt(p._4), nt(p._5), nt(p._6), nt(p._7), nt(p._8), nt(p._9))

  def transformC[G[_]](nt: (F ~> G)): AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]#Cop => AndXorK9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9]#Cop =
    (p: AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]#Cop) => p.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))))))

  // format: off
  def sequenceP(prod: Prod)(A: Apply[F]): F[AndXorK9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]#Prod] = {
    val (a0, a1, a2, a3, a4, a5, a6, a7, a8) = prod
    A.ap(a8)(
    A.ap(a7)(
    A.ap(a6)(
    A.ap(a5)(
    A.ap(a4)(
    A.ap(a3)(
    A.ap(a2)(
    A.ap(a1)(
     A.map(a0)(((i0: A1, i1: A2, i2: A3, i3: A4, i4: A5, i5: A6, i6: A7, i7: A8, i8: A9) =>
    (i0, i1, i2, i3, i4, i5, i6, i7, i8)).curried)))))))))
  }
  // format: on
}

object AndXorK9 {

  def apply[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] =
    new AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] {}
}

trait AndXorF9[A1, A2, A3, A4, A5, A6, A7, A8, A9] {
  type Repr[F[_]] = AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]
  def apply[F[_]]: Repr[F] =
    new AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] {}
}

object AndXorF9 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9]: AndXorF9[A1, A2, A3, A4, A5, A6, A7, A8, A9] =
    new AndXorF9[A1, A2, A3, A4, A5, A6, A7, A8, A9] {}
}

trait AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9] extends AndXorK9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

object AndXor9 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9]: AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9] =
    new AndXor9[A1, A2, A3, A4, A5, A6, A7, A8, A9] {}
}
