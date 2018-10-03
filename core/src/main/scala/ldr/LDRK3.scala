package ldr
import scala.language.higherKinds
import scalaz.{Apply, Functor, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.syntax.apply._
import scalaz.syntax.either._

trait LDRK3[F[_], A1, A2, A3] extends LDR {
  type Prod = (F[A1], F[A2], F[A3])
  type Cop = (F[A1] \/ (F[A2] \/ F[A3]))
  val LDRF = LDRF3[A1, A2, A3]
  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]]): ComposeLDR[G, Cop, Prod] =
    new ComposeLDR[G, Cop, Prod] {
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

  def transformP[G[_]](nt: (F ~> G)): LDRK3[F, A1, A2, A3]#Prod => LDRK3[G, A1, A2, A3]#Prod =
    (p: LDRK3[F, A1, A2, A3]#Prod) => (nt(p._1), nt(p._2), nt(p._3))

  def transformC[G[_]](nt: (F ~> G)): LDRK3[F, A1, A2, A3]#Cop => LDRK3[G, A1, A2, A3]#Cop =
    (p: LDRK3[F, A1, A2, A3]#Cop) => p.bimap(nt(_), _.bimap(nt(_), nt(_)))

  def sequenceP(prod: Prod)(A: Apply[F]): F[LDRK3[Id, A1, A2, A3]#Prod] =
    A.tuple3(prod._1, prod._2, prod._3)

  import scalaz.{-\/, \/-}
  def sequenceC(c: Cop)(implicit FF: Functor[F]): F[LDRK3[Id, A1, A2, A3]#Cop] = {
    val t = LDRF[Id]
    import t.instances._
    c match {
      case -\/(x) => FF.map(x)(t.inj(_))
      case \/-(-\/(x)) => FF.map(x)(t.inj(_))
      case \/-(\/-(x)) => FF.map(x)(t.inj(_))
    }
  }

}

object LDRK3 {

  def apply[F[_], A1, A2, A3]: LDRK3[F, A1, A2, A3] =
    new LDRK3[F, A1, A2, A3] {}

  def foldMap[A1, A2, A3, C](p: LDRK3[List, A1, A2, A3]#Prod)(
    map: LDRK3[Id, A1, A2, A3]#Cop => C)(
      implicit O: Ordering[LDRK3[Id, A1, A2, A3]#Cop], M: Monoid[C]): C = {
    val T = new LDRF3[A1, A2, A3] {}
    val TL = T[List]
    val TI = T[Id]
    import scala.collection.mutable.{PriorityQueue => PQ}
    import TI.instances._

    def uncons(p: TL.Prod): (List[TI.Cop], TL.Prod) =
      (List(p._1.headOption.map(TI.inj(_: A1)),
           p._2.headOption.map(TI.inj(_: A2)),
           p._3.headOption.map(TI.inj(_: A3))).flatten,
       (p._1.headOption.map(_ => p._1.tail).getOrElse(p._1),
        p._2.headOption.map(_ => p._2.tail).getOrElse(p._2),
        p._3.headOption.map(_ => p._3.tail).getOrElse(p._3)))

    @scala.annotation.tailrec
    def go(cop: TL.Prod, q: PQ[TI.Cop], out: C): C =
      cop match {
        case (Nil, Nil, Nil) =>
          q.foldLeft(out)((acc, el) => M.append(acc, map(el)))
        case (a1s, a2s, a3s) => q.isEmpty match {
          case true => {
            val (hs, ts) = uncons(cop)
            q ++ hs
            go(ts, q, out)
          }
          case false => q.dequeue match {
            case -\/(x) =>
              go((a1s.tail, a2s, a3s), q, M.append(out, map(TI.inj(x))))
            case \/-(-\/(x)) =>
              go((a1s, a2s.tail, a3s), q, M.append(out, map(TI.inj(x))))
            case \/-(\/-(x)) =>
              go((a1s, a2s, a3s.tail), q, M.append(out, map(TI.inj(x))))
          }
        }
      }

    val Q = new scala.collection.mutable.PriorityQueue[TI.Cop]()
    val (hs, ts) = uncons(p)
    Q ++ hs
    go(ts, Q, M.zero)
  }
}

trait LDRF3[A1, A2, A3] {
  type Repr[F[_]] = LDRK3[F, A1, A2, A3]
  def apply[F[_]]: Repr[F] =
    new LDRK3[F, A1, A2, A3] {}
}

object LDRF3 {
  def apply[A1, A2, A3]: LDRF3[A1, A2, A3] =
    new LDRF3[A1, A2, A3] {}
}

trait LDR3[A1, A2, A3] extends LDRK3[Id, A1, A2, A3]

object LDR3 {
  def apply[A1, A2, A3]: LDR3[A1, A2, A3] =
    new LDR3[A1, A2, A3] {}
}
