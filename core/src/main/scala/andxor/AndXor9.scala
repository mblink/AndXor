package andxor

import andxor.tuple._
import andxor.types.{Cop9, Prod9}
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id

trait AndXorK9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9] extends AndXor {
  type Prod = Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]
  object Prod { def apply(p: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9])): Prod = Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](p) }

  type Cop = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]
  object Cop { def apply(c: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ F[A9]))))))))): Cop = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](c) }

  val AndXorF = AndXorF9[A1, A2, A3, A4, A5, A6, A7, A8, A9]
  type AndXor[G[_]] = AndXorF.Repr[G]

  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]], a3: G[F[A4]], a4: G[F[A5]], a5: G[F[A6]], a6: G[F[A7]], a7: G[F[A8]], a8: G[F[A9]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose9(a0, a1, a2, a3, a4, a5, a6, a7, a8)(f(_).run)

      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly9(a0, a1, a2, a3, a4, a5, a6, a7, a8)(x => f(Cop(x)))

      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide9(a0, a1, a2, a3, a4, a5, a6, a7, a8)(f(_).run)

      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply9(a0, a1, a2, a3, a4, a5, a6, a7, a8) {
          case (i0, i1, i2, i3, i4, i5, i6, i7, i8) =>
            f(Prod((i0, i1, i2, i3, i4, i5, i6, i7, i8)))
        }

    }

  val injEv = combine[Inj[Cop, ?]].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj[Prod, ?]].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]#Prod => AndXorK9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9]#Prod =
    (p: AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]#Prod) => {
      val pr = p.run
      Prod9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9]((nt(pr.t1), nt(pr.t2), nt(pr.t3), nt(pr.t4), nt(pr.t5), nt(pr.t6), nt(pr.t7), nt(pr.t8), nt(pr.t9)))
    }

  def transformC[G[_]](nt: (F ~> G)): AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]#Cop => AndXorK9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9]#Cop =
    (p: AndXorK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]#Cop) =>
      Cop9[G, A1, A2, A3, A4, A5, A6, A7, A8, A9](
        p.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))))))
      )

  def subst1[G[_]]: AndXor9[G[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9]] = AndXor9[G[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9]]

  def subst2[G[_]]: AndXor9[F[A1], G[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9]] = AndXor9[F[A1], G[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9]]

  def subst3[G[_]]: AndXor9[F[A1], F[A2], G[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9]] = AndXor9[F[A1], F[A2], G[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9]]

  def subst4[G[_]]: AndXor9[F[A1], F[A2], F[A3], G[A4], F[A5], F[A6], F[A7], F[A8], F[A9]] = AndXor9[F[A1], F[A2], F[A3], G[A4], F[A5], F[A6], F[A7], F[A8], F[A9]]

  def subst5[G[_]]: AndXor9[F[A1], F[A2], F[A3], F[A4], G[A5], F[A6], F[A7], F[A8], F[A9]] = AndXor9[F[A1], F[A2], F[A3], F[A4], G[A5], F[A6], F[A7], F[A8], F[A9]]

  def subst6[G[_]]: AndXor9[F[A1], F[A2], F[A3], F[A4], F[A5], G[A6], F[A7], F[A8], F[A9]] = AndXor9[F[A1], F[A2], F[A3], F[A4], F[A5], G[A6], F[A7], F[A8], F[A9]]

  def subst7[G[_]]: AndXor9[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], G[A7], F[A8], F[A9]] = AndXor9[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], G[A7], F[A8], F[A9]]

  def subst8[G[_]]: AndXor9[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], G[A8], F[A9]] = AndXor9[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], G[A8], F[A9]]

  def subst9[G[_]]: AndXor9[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], G[A9]] = AndXor9[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], G[A9]]

  // format: off
  def sequenceP(prod: Prod)(implicit A: Apply[F]): F[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = {
    val p = prod.run
    A.map(
    A.ap(p.t9)(
    A.ap(p.t8)(
    A.ap(p.t7)(
    A.ap(p.t6)(
    A.ap(p.t5)(
    A.ap(p.t4)(
    A.ap(p.t3)(
    A.ap(p.t2)(
    A.map(p.t1)((i0: A1) => (i1: A2) => (i2: A3) => (i3: A4) => (i4: A5) => (i5: A6) => (i6: A7) => (i7: A8) => (i8: A9) =>
      (i0, i1, i2, i3, i4, i5, i6, i7, i8)))))))))))(Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9](_))
  }

  def sequenceC(cop: Cop)(implicit FF: Functor[F]): F[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
    cop.run match {
      case -\/(x) => FF.map(x)(y => Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9](-\/(y)))
      case \/-(-\/(x)) => FF.map(x)(y => Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(-\/(y))))
      case \/-(\/-(-\/(x))) => FF.map(x)(y => Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(-\/(y)))))
      case \/-(\/-(\/-(-\/(x)))) => FF.map(x)(y => Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(-\/(y))))))
      case \/-(\/-(\/-(\/-(-\/(x))))) => FF.map(x)(y => Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(-\/(y)))))))
      case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => FF.map(x)(y => Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(-\/(y))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => FF.map(x)(y => Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => FF.map(x)(y => Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))) => FF.map(x)(y => Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y))))))))))
    }

  def extractC[B](c: Cop)(implicit inj: Inj[Option[B], Cop]): Option[B] = inj(c)

  def extractP[B](p: Prod)(implicit inj: Inj[B, Prod]): B = inj(p)

  def foldMap[G[_], C](p: AndXor[G]#Prod)(map: AndXor[Id]#Cop => C)(
      implicit O: Ordering[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]], M: Monoid[C], PE: PlusEmpty[G], U: Uncons[G]): C = {
    import scala.collection.mutable.{PriorityQueue => PQ}

    val TG = AndXorF[G]
    val TI = AndXorF[Id]

    def uncons(p: TG.Prod): (List[TI.Cop], TG.Prod) = {
      val pr = p.run
      val ht1 = U(pr.t1)
      val ht2 = U(pr.t2)
      val ht3 = U(pr.t3)
      val ht4 = U(pr.t4)
      val ht5 = U(pr.t5)
      val ht6 = U(pr.t6)
      val ht7 = U(pr.t7)
      val ht8 = U(pr.t8)
      val ht9 = U(pr.t9)
      (List(ht1._1.map(TI.inj(_: Id[A1])), ht2._1.map(TI.inj(_: Id[A2])), ht3._1.map(TI.inj(_: Id[A3])), ht4._1.map(TI.inj(_: Id[A4])), ht5._1.map(TI.inj(_: Id[A5])), ht6._1.map(TI.inj(_: Id[A6])), ht7._1.map(TI.inj(_: Id[A7])), ht8._1.map(TI.inj(_: Id[A8])), ht9._1.map(TI.inj(_: Id[A9]))).flatten,
        TG.Prod((ht1._2, ht2._2, ht3._2, ht4._2, ht5._2, ht6._2, ht7._2, ht8._2, ht9._2)))
    }
    @scala.annotation.tailrec
    def go(prod: TG.Prod, q: PQ[TI.Cop], out: C): C =
      (prod.run.==((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8], PE.empty[A9]))) match {
        case true =>
          q.foldLeft(out)((acc, el) => M.append(acc, map(el)))
        case false => q.isEmpty match {
          case true => {
            val (hs, ts) = uncons(prod)
            q ++= hs
            go(ts, q, out)
          }
          case false => q.dequeue.run match {
            case -\/(x) => {
              val pr = prod.run
              val (h, t) = U(pr.t1)
              go(TG.Prod((t, pr.t2, pr.t3, pr.t4, pr.t5, pr.t6, pr.t7, pr.t8, pr.t9)),
                q ++= h.map(TI.inj(_: Id[A1])), M.append(out, map(TI.inj(x))))
          }
          case \/-(-\/(x)) => {
              val pr = prod.run
              val (h, t) = U(pr.t2)
              go(TG.Prod((pr.t1, t, pr.t3, pr.t4, pr.t5, pr.t6, pr.t7, pr.t8, pr.t9)),
                q ++= h.map(TI.inj(_: Id[A2])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(-\/(x))) => {
              val pr = prod.run
              val (h, t) = U(pr.t3)
              go(TG.Prod((pr.t1, pr.t2, t, pr.t4, pr.t5, pr.t6, pr.t7, pr.t8, pr.t9)),
                q ++= h.map(TI.inj(_: Id[A3])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(-\/(x)))) => {
              val pr = prod.run
              val (h, t) = U(pr.t4)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, t, pr.t5, pr.t6, pr.t7, pr.t8, pr.t9)),
                q ++= h.map(TI.inj(_: Id[A4])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(-\/(x))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t5)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, t, pr.t6, pr.t7, pr.t8, pr.t9)),
                q ++= h.map(TI.inj(_: Id[A5])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t6)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, pr.t5, t, pr.t7, pr.t8, pr.t9)),
                q ++= h.map(TI.inj(_: Id[A6])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t7)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, pr.t5, pr.t6, t, pr.t8, pr.t9)),
                q ++= h.map(TI.inj(_: Id[A7])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t8)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, pr.t5, pr.t6, pr.t7, t, pr.t9)),
                q ++= h.map(TI.inj(_: Id[A8])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t9)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, pr.t5, pr.t6, pr.t7, pr.t8, t)),
                q ++= h.map(TI.inj(_: Id[A9])), M.append(out, map(TI.inj(x))))
          }

          }
        }
      }
    val Q = new PQ[TI.Cop]()(O)
    val (hs, ts) = uncons(p)
    Q ++= hs
    go(ts, Q, M.zero)
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
