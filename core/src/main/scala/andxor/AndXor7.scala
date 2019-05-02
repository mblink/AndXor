package andxor

import andxor.tuple._
import andxor.types.{Cop7, Prod7}
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id

trait AndXorK7[F[_], A1, A2, A3, A4, A5, A6, A7] extends AndXor {
  type Prod = Prod7[F, A1, A2, A3, A4, A5, A6, A7]
  object Prod { def apply(p: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7])): Prod = Prod7[F, A1, A2, A3, A4, A5, A6, A7](p) }

  type Cop = Cop7[F, A1, A2, A3, A4, A5, A6, A7]
  object Cop { def apply(c: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ F[A7]))))))): Cop = Cop7[F, A1, A2, A3, A4, A5, A6, A7](c) }

  val AndXorF = AndXorF7[A1, A2, A3, A4, A5, A6, A7]
  type AndXor[G[_]] = AndXorF.Repr[G]

  def combineId[G[_]](implicit @scalaz.unused ev: F[_] =:= Id[_], a0: G[A1], a1: G[A2], a2: G[A3], a3: G[A4], a4: G[A5], a5: G[A6], a6: G[A7])
      : ComposeAndXor[G, Cop7[Id, A1, A2, A3, A4, A5, A6, A7], Prod7[Id, A1, A2, A3, A4, A5, A6, A7]] =
    AndXor7[A1, A2, A3, A4, A5, A6, A7].combine[G]

  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]], a3: G[F[A4]], a4: G[F[A5]], a5: G[F[A6]], a6: G[F[A7]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose7(a0, a1, a2, a3, a4, a5, a6)(f(_).run)

      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly7(a0, a1, a2, a3, a4, a5, a6)(x => f(Cop(x)))

      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide7(a0, a1, a2, a3, a4, a5, a6)(f(_).run)

      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply7(a0, a1, a2, a3, a4, a5, a6) {
          case (i0, i1, i2, i3, i4, i5, i6) =>
            f(Prod((i0, i1, i2, i3, i4, i5, i6)))
        }

    }

  val injEv = combine[Inj[Cop, ?]].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj[Prod, ?]].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK7[F, A1, A2, A3, A4, A5, A6, A7]#Prod => AndXorK7[G, A1, A2, A3, A4, A5, A6, A7]#Prod =
    (p: AndXorK7[F, A1, A2, A3, A4, A5, A6, A7]#Prod) => {
      val pr = p.run
      Prod7[G, A1, A2, A3, A4, A5, A6, A7]((nt(pr.t1), nt(pr.t2), nt(pr.t3), nt(pr.t4), nt(pr.t5), nt(pr.t6), nt(pr.t7)))
    }

  def transformC[G[_]](nt: (F ~> G)): AndXorK7[F, A1, A2, A3, A4, A5, A6, A7]#Cop => AndXorK7[G, A1, A2, A3, A4, A5, A6, A7]#Cop =
    (p: AndXorK7[F, A1, A2, A3, A4, A5, A6, A7]#Cop) =>
      Cop7[G, A1, A2, A3, A4, A5, A6, A7](
        p.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))))
      )

  def subst1[G[_]]: AndXor7[G[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7]] = AndXor7[G[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7]]

  def subst2[G[_]]: AndXor7[F[A1], G[A2], F[A3], F[A4], F[A5], F[A6], F[A7]] = AndXor7[F[A1], G[A2], F[A3], F[A4], F[A5], F[A6], F[A7]]

  def subst3[G[_]]: AndXor7[F[A1], F[A2], G[A3], F[A4], F[A5], F[A6], F[A7]] = AndXor7[F[A1], F[A2], G[A3], F[A4], F[A5], F[A6], F[A7]]

  def subst4[G[_]]: AndXor7[F[A1], F[A2], F[A3], G[A4], F[A5], F[A6], F[A7]] = AndXor7[F[A1], F[A2], F[A3], G[A4], F[A5], F[A6], F[A7]]

  def subst5[G[_]]: AndXor7[F[A1], F[A2], F[A3], F[A4], G[A5], F[A6], F[A7]] = AndXor7[F[A1], F[A2], F[A3], F[A4], G[A5], F[A6], F[A7]]

  def subst6[G[_]]: AndXor7[F[A1], F[A2], F[A3], F[A4], F[A5], G[A6], F[A7]] = AndXor7[F[A1], F[A2], F[A3], F[A4], F[A5], G[A6], F[A7]]

  def subst7[G[_]]: AndXor7[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], G[A7]] = AndXor7[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], G[A7]]

  // format: off
  def sequenceP(prod: Prod)(implicit A: Apply[F]): F[Prod7[Id, A1, A2, A3, A4, A5, A6, A7]] = {
    val p = prod.run
    A.map(
    A.ap(p.t7)(
    A.ap(p.t6)(
    A.ap(p.t5)(
    A.ap(p.t4)(
    A.ap(p.t3)(
    A.ap(p.t2)(
    A.map(p.t1)((i0: A1) => (i1: A2) => (i2: A3) => (i3: A4) => (i4: A5) => (i5: A6) => (i6: A7) =>
      (i0, i1, i2, i3, i4, i5, i6)))))))))(Prod7[Id, A1, A2, A3, A4, A5, A6, A7](_))
  }

  def sequenceC(cop: Cop)(implicit FF: Functor[F]): F[Cop7[Id, A1, A2, A3, A4, A5, A6, A7]] =
    cop.run match {
      case -\/(x) => FF.map(x)(y => Cop7[Id, A1, A2, A3, A4, A5, A6, A7](-\/(y)))
      case \/-(-\/(x)) => FF.map(x)(y => Cop7[Id, A1, A2, A3, A4, A5, A6, A7](\/-(-\/(y))))
      case \/-(\/-(-\/(x))) => FF.map(x)(y => Cop7[Id, A1, A2, A3, A4, A5, A6, A7](\/-(\/-(-\/(y)))))
      case \/-(\/-(\/-(-\/(x)))) => FF.map(x)(y => Cop7[Id, A1, A2, A3, A4, A5, A6, A7](\/-(\/-(\/-(-\/(y))))))
      case \/-(\/-(\/-(\/-(-\/(x))))) => FF.map(x)(y => Cop7[Id, A1, A2, A3, A4, A5, A6, A7](\/-(\/-(\/-(\/-(-\/(y)))))))
      case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => FF.map(x)(y => Cop7[Id, A1, A2, A3, A4, A5, A6, A7](\/-(\/-(\/-(\/-(\/-(-\/(y))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(x)))))) => FF.map(x)(y => Cop7[Id, A1, A2, A3, A4, A5, A6, A7](\/-(\/-(\/-(\/-(\/-(\/-(y))))))))
    }

  def extractC[B](c: Cop)(implicit inj: Inj[Option[B], Cop]): Option[B] = inj(c)

  def extractP[B](p: Prod)(implicit inj: Inj[B, Prod]): B = inj(p)

  def foldMap[G[_], C](p: AndXor[G]#Prod)(map: AndXor[Id]#Cop => C)(
      implicit O: Ordering[Cop7[Id, A1, A2, A3, A4, A5, A6, A7]], M: Monoid[C], PE: PlusEmpty[G], U: Uncons[G]): C = {
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
      (List(ht1._1.map(TI.inj(_: Id[A1])), ht2._1.map(TI.inj(_: Id[A2])), ht3._1.map(TI.inj(_: Id[A3])), ht4._1.map(TI.inj(_: Id[A4])), ht5._1.map(TI.inj(_: Id[A5])), ht6._1.map(TI.inj(_: Id[A6])), ht7._1.map(TI.inj(_: Id[A7]))).flatten,
        TG.Prod((ht1._2, ht2._2, ht3._2, ht4._2, ht5._2, ht6._2, ht7._2)))
    }
    @scala.annotation.tailrec
    def go(prod: TG.Prod, q: PQ[TI.Cop], out: C): C =
      (prod.run.==((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7]))) match {
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
              go(TG.Prod((t, pr.t2, pr.t3, pr.t4, pr.t5, pr.t6, pr.t7)),
                q ++= h.map(TI.inj(_: Id[A1])), M.append(out, map(TI.inj(x))))
          }
          case \/-(-\/(x)) => {
              val pr = prod.run
              val (h, t) = U(pr.t2)
              go(TG.Prod((pr.t1, t, pr.t3, pr.t4, pr.t5, pr.t6, pr.t7)),
                q ++= h.map(TI.inj(_: Id[A2])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(-\/(x))) => {
              val pr = prod.run
              val (h, t) = U(pr.t3)
              go(TG.Prod((pr.t1, pr.t2, t, pr.t4, pr.t5, pr.t6, pr.t7)),
                q ++= h.map(TI.inj(_: Id[A3])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(-\/(x)))) => {
              val pr = prod.run
              val (h, t) = U(pr.t4)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, t, pr.t5, pr.t6, pr.t7)),
                q ++= h.map(TI.inj(_: Id[A4])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(-\/(x))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t5)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, t, pr.t6, pr.t7)),
                q ++= h.map(TI.inj(_: Id[A5])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t6)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, pr.t5, t, pr.t7)),
                q ++= h.map(TI.inj(_: Id[A6])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(x)))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t7)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, pr.t5, pr.t6, t)),
                q ++= h.map(TI.inj(_: Id[A7])), M.append(out, map(TI.inj(x))))
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

object AndXorK7 {

  def apply[F[_], A1, A2, A3, A4, A5, A6, A7]: AndXorK7[F, A1, A2, A3, A4, A5, A6, A7] =
    new AndXorK7[F, A1, A2, A3, A4, A5, A6, A7] {}
}

trait AndXorF7[A1, A2, A3, A4, A5, A6, A7] {
  type Repr[F[_]] = AndXorK7[F, A1, A2, A3, A4, A5, A6, A7]
  def apply[F[_]]: Repr[F] =
    new AndXorK7[F, A1, A2, A3, A4, A5, A6, A7] {}
}

object AndXorF7 {
  def apply[A1, A2, A3, A4, A5, A6, A7]: AndXorF7[A1, A2, A3, A4, A5, A6, A7] =
    new AndXorF7[A1, A2, A3, A4, A5, A6, A7] {}
}

trait AndXor7[A1, A2, A3, A4, A5, A6, A7] extends AndXorK7[Id, A1, A2, A3, A4, A5, A6, A7]

object AndXor7 {
  def apply[A1, A2, A3, A4, A5, A6, A7]: AndXor7[A1, A2, A3, A4, A5, A6, A7] =
    new AndXor7[A1, A2, A3, A4, A5, A6, A7] {}
}
