package andxor
import scala.language.higherKinds
import scalaz.{Apply, Functor, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.syntax.either._

trait AndXorK5[F[_], A1, A2, A3, A4, A5] extends AndXor {
  type Prod = (F[A1], F[A2], F[A3], F[A4], F[A5])
  type Cop = (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ F[A5]))))
  val AndXorF = AndXorF5[A1, A2, A3, A4, A5]
  type AndXor[G[_]] = AndXorF5[A1, A2, A3, A4, A5]#Repr[G]
  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]], a3: G[F[A4]], a4: G[F[A5]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose5(a0, a1, a2, a3, a4)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly5(a0, a1, a2, a3, a4)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide5(a0, a1, a2, a3, a4)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply5(a0, a1, a2, a3, a4)((i0, i1, i2, i3, i4) => f((i0, i1, i2, i3, i4)))
    }

  object instances {

    implicit val inja0: Inj[Cop, F[A1]] =
      Inj.instance(_.left[(F[A2] \/ (F[A3] \/ (F[A4] \/ F[A5])))])

    implicit val inja1: Inj[Cop, F[A2]] =
      Inj.instance(_.left[(F[A3] \/ (F[A4] \/ F[A5]))].right[F[A1]])

    implicit val inja2: Inj[Cop, F[A3]] =
      Inj.instance(_.left[(F[A4] \/ F[A5])].right[F[A2]].right[F[A1]])

    implicit val inja3: Inj[Cop, F[A4]] =
      Inj.instance(_.left[F[A5]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja4: Inj[Cop, F[A5]] =
      Inj.instance(_.right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit def lifta0(implicit M: Monoid[Prod]): Inj[Prod, F[A1]] = {
      val (_, a0, a1, a2, a3) =
        M.zero
      Inj.instance((_, a0, a1, a2, a3))
    }

    implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, F[A2]] = {
      val (a0, _, a1, a2, a3) =
        M.zero
      Inj.instance((a0, _, a1, a2, a3))
    }

    implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, F[A3]] = {
      val (a0, a1, _, a2, a3) =
        M.zero
      Inj.instance((a0, a1, _, a2, a3))
    }

    implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, F[A4]] = {
      val (a0, a1, a2, _, a3) =
        M.zero
      Inj.instance((a0, a1, a2, _, a3))
    }

    implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, F[A5]] = {
      val (a0, a1, a2, a3, _) =
        M.zero
      Inj.instance((a0, a1, a2, a3, _))
    }

  }

  import instances._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK5[F, A1, A2, A3, A4, A5]#Prod => AndXorK5[G, A1, A2, A3, A4, A5]#Prod =
    (p: AndXorK5[F, A1, A2, A3, A4, A5]#Prod) => (nt(p._1), nt(p._2), nt(p._3), nt(p._4), nt(p._5))

  def transformC[G[_]](nt: (F ~> G)): AndXorK5[F, A1, A2, A3, A4, A5]#Cop => AndXorK5[G, A1, A2, A3, A4, A5]#Cop =
    (p: AndXorK5[F, A1, A2, A3, A4, A5]#Cop) => p.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))

  // format: off
  def sequenceP(prod: Prod)(A: Apply[F]): F[AndXorK5[Id, A1, A2, A3, A4, A5]#Prod] = {
    val (a0, a1, a2, a3, a4) = prod
    A.ap(a4)(
    A.ap(a3)(
    A.ap(a2)(
    A.ap(a1)(
     A.map(a0)(((i0: A1, i1: A2, i2: A3, i3: A4, i4: A5) =>
    (i0, i1, i2, i3, i4)).curried)))))
  }
  // format: on

  def foldMap[C](p: AndXor[List]#Prod)(map: AndXor[Id]#Cop => C)(implicit O: Ordering[AndXorK5[Id, A1, A2, A3, A4, A5]#Cop], M: Monoid[C]): C = {
    val TL = AndXorF[List]
    val TI = AndXorF[Id]
    import scala.collection.mutable.{PriorityQueue => PQ}
    import TI.instances._
    def uncons(p: TL.Prod): (List[TI.Cop], TL.Prod) =
      (
        List(
          p._1.headOption.map(TI.inj(_: A1)),
          p._2.headOption.map(TI.inj(_: A2)),
          p._3.headOption.map(TI.inj(_: A3)),
          p._4.headOption.map(TI.inj(_: A4)),
          p._5.headOption.map(TI.inj(_: A5))
        ).flatten,
        (
          p._1.headOption.map(_ => p._1.tail).getOrElse(p._1),
          p._2.headOption.map(_ => p._2.tail).getOrElse(p._2),
          p._3.headOption.map(_ => p._3.tail).getOrElse(p._3),
          p._4.headOption.map(_ => p._4.tail).getOrElse(p._4),
          p._5.headOption.map(_ => p._5.tail).getOrElse(p._5)
        )
      )
    @scala.annotation.tailrec
    def go(prod: TL.Prod, q: PQ[TI.Cop], out: C): C =
      prod match {
        case (Nil, Nil, Nil, Nil, Nil) =>
          q.foldLeft(out)((acc, el) => M.append(acc, map(el)))
        case (as0, as1, as2, as3, as4) =>
          q.isEmpty match {
            case true => {
              val (hs, ts) = uncons(prod)
              q ++= hs
              go(ts, q, out)
            }
            case false =>
              q.dequeue match {
                case -\/(x) =>
                  go((as0.tail, as1, as2, as3, as4), q ++= as0.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(-\/(x)) =>
                  go((as0, as1.tail, as2, as3, as4), q ++= as1.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(\/-(-\/(x))) =>
                  go((as0, as1, as2.tail, as3, as4), q ++= as2.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(\/-(\/-(-\/(x)))) =>
                  go((as0, as1, as2, as3.tail, as4), q ++= as3.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(\/-(\/-(\/-(x)))) =>
                  go((as0, as1, as2, as3, as4.tail), q ++= as4.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))

              }
          }
      }
    val Q = new scala.collection.mutable.PriorityQueue[TI.Cop]()
    val (hs, ts) = uncons(p)
    Q ++= hs
    go(ts, Q, M.zero)
  }
}

object AndXorK5 {

  def apply[F[_], A1, A2, A3, A4, A5]: AndXorK5[F, A1, A2, A3, A4, A5] =
    new AndXorK5[F, A1, A2, A3, A4, A5] {}
}

trait AndXorF5[A1, A2, A3, A4, A5] {
  type Repr[F[_]] = AndXorK5[F, A1, A2, A3, A4, A5]
  def apply[F[_]]: Repr[F] =
    new AndXorK5[F, A1, A2, A3, A4, A5] {}
}

object AndXorF5 {
  def apply[A1, A2, A3, A4, A5]: AndXorF5[A1, A2, A3, A4, A5] =
    new AndXorF5[A1, A2, A3, A4, A5] {}
}

trait AndXor5[A1, A2, A3, A4, A5] extends AndXorK5[Id, A1, A2, A3, A4, A5]

object AndXor5 {
  def apply[A1, A2, A3, A4, A5]: AndXor5[A1, A2, A3, A4, A5] =
    new AndXor5[A1, A2, A3, A4, A5] {}

}
