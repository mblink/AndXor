package andxor
import scala.language.higherKinds
import scalaz.{Apply, Functor, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.syntax.either._

trait AndXorK4[F[_], A1, A2, A3, A4] extends AndXor {
  type Prod = (F[A1], F[A2], F[A3], F[A4])
  type Cop = (F[A1] \/ (F[A2] \/ (F[A3] \/ F[A4])))
  val AndXorF = AndXorF4[A1, A2, A3, A4]
  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]], a3: G[F[A4]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose4(a0, a1, a2, a3)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly4(a0, a1, a2, a3)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide4(a0, a1, a2, a3)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply4(a0, a1, a2, a3)((i0, i1, i2, i3) => f((i0, i1, i2, i3)))
    }

  object instances {

    implicit val inja0: Inj[Cop, F[A1]] =
      Inj.instance(_.left[(F[A2] \/ (F[A3] \/ F[A4]))])

    implicit val inja1: Inj[Cop, F[A2]] =
      Inj.instance(_.left[(F[A3] \/ F[A4])].right[F[A1]])

    implicit val inja2: Inj[Cop, F[A3]] =
      Inj.instance(_.left[F[A4]].right[F[A2]].right[F[A1]])

    implicit val inja3: Inj[Cop, F[A4]] =
      Inj.instance(_.right[F[A3]].right[F[A2]].right[F[A1]])

    implicit def lifta0(implicit M: Monoid[Prod]): Inj[Prod, F[A1]] = {
      val (_, a0, a1, a2) =
        M.zero
      Inj.instance((_, a0, a1, a2))
    }

    implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, F[A2]] = {
      val (a0, _, a1, a2) =
        M.zero
      Inj.instance((a0, _, a1, a2))
    }

    implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, F[A3]] = {
      val (a0, a1, _, a2) =
        M.zero
      Inj.instance((a0, a1, _, a2))
    }

    implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, F[A4]] = {
      val (a0, a1, a2, _) =
        M.zero
      Inj.instance((a0, a1, a2, _))
    }

  }

  import instances._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK4[F, A1, A2, A3, A4]#Prod => AndXorK4[G, A1, A2, A3, A4]#Prod =
    (p: AndXorK4[F, A1, A2, A3, A4]#Prod) => (nt(p._1), nt(p._2), nt(p._3), nt(p._4))

  def transformC[G[_]](nt: (F ~> G)): AndXorK4[F, A1, A2, A3, A4]#Cop => AndXorK4[G, A1, A2, A3, A4]#Cop =
    (p: AndXorK4[F, A1, A2, A3, A4]#Cop) => p.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_))))

  // format: off
  def sequenceP(prod: Prod)(A: Apply[F]): F[AndXorK4[Id, A1, A2, A3, A4]#Prod] = {
    val (a0, a1, a2, a3) = prod
    A.ap(a3)(
    A.ap(a2)(
    A.ap(a1)(
     A.map(a0)(((i0: A1, i1: A2, i2: A3, i3: A4) =>
    (i0, i1, i2, i3)).curried))))
  }
  // format: on

}

object AndXorK4 {

  def apply[F[_], A1, A2, A3, A4]: AndXorK4[F, A1, A2, A3, A4] =
    new AndXorK4[F, A1, A2, A3, A4] {}
}

trait AndXorF4[A1, A2, A3, A4] {
  type Repr[F[_]] = AndXorK4[F, A1, A2, A3, A4]
  def apply[F[_]]: Repr[F] =
    new AndXorK4[F, A1, A2, A3, A4] {}
}

object AndXorF4 {
  def apply[A1, A2, A3, A4]: AndXorF4[A1, A2, A3, A4] =
    new AndXorF4[A1, A2, A3, A4] {}
}

trait AndXor4[A1, A2, A3, A4] extends AndXorK4[Id, A1, A2, A3, A4]

object AndXor4 {
  def apply[A1, A2, A3, A4]: AndXor4[A1, A2, A3, A4] =
    new AndXor4[A1, A2, A3, A4] {}

  def foldMap[A1, A2, A3, A4, C](p: AndXorK4[List, A1, A2, A3, A4]#Prod)(map: AndXorK4[Id, A1, A2, A3, A4]#Cop => C)(implicit O: Ordering[AndXorK4[Id, A1, A2, A3, A4]#Cop], M: Monoid[C]): C = {
    val T = new AndXorF4[A1, A2, A3, A4] {}
    val TL = T[List]
    val TI = T[Id]
    import scala.collection.mutable.{PriorityQueue => PQ}
    import TI.instances._
    def uncons(p: TL.Prod): (List[TI.Cop], TL.Prod) =
      (
        List(p._1.headOption.map(TI.inj(_: A1)), p._2.headOption.map(TI.inj(_: A2)), p._3.headOption.map(TI.inj(_: A3)), p._4.headOption.map(TI.inj(_: A4))).flatten,
        (
          p._1.headOption.map(_ => p._1.tail).getOrElse(p._1),
          p._2.headOption.map(_ => p._2.tail).getOrElse(p._2),
          p._3.headOption.map(_ => p._3.tail).getOrElse(p._3),
          p._4.headOption.map(_ => p._4.tail).getOrElse(p._4)
        )
      )
    @scala.annotation.tailrec
    def go(prod: TL.Prod, q: PQ[TI.Cop], out: C): C =
      prod match {
        case (Nil, Nil, Nil, Nil) =>
          q.foldLeft(out)((acc, el) => M.append(acc, map(el)))
        case (as0, as1, as2, as3) =>
          q.isEmpty match {
            case true => {
              val (hs, ts) = uncons(prod)
              q ++ hs
              go(ts, q, out)
            }
            case false =>
              q.dequeue match {
                case -\/(x) =>
                  go((as0.tail, as1, as2, as3), q, M.append(out, map(TI.inj(x))))
                case \/-(-\/(x)) =>
                  go((as0, as1.tail, as2, as3), q, M.append(out, map(TI.inj(x))))
                case \/-(\/-(-\/(x))) =>
                  go((as0, as1, as2.tail, as3), q, M.append(out, map(TI.inj(x))))
                case \/-(\/-(\/-(x))) =>
                  go((as0, as1, as2, as3.tail), q, M.append(out, map(TI.inj(x))))

              }
          }
      }
    val Q = new scala.collection.mutable.PriorityQueue[TI.Cop]()
    val (hs, ts) = uncons(p)
    Q ++ hs
    go(ts, Q, M.zero)
  }
}
