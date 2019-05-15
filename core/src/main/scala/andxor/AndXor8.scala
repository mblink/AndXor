package andxor

import andxor.types.{Cop8, Prod8}
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXorK8[F[_], A1, A2, A3, A4, A5, A6, A7, A8] extends AndXor {
  type Prod = Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]
  object Prod { def apply(p: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8])): Prod = Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8](p) }

  type Cop = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]
  object Cop { def apply(c: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ F[A8])))))))): Cop = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](c) }

  val AndXorF = AndXorF8[A1, A2, A3, A4, A5, A6, A7, A8]
  type AndXor[G[_]] = AndXorF.Repr[G]

  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]], a3: G[F[A4]], a4: G[F[A5]], a5: G[F[A6]], a6: G[F[A7]], a7: G[F[A8]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose8(a0, a1, a2, a3, a4, a5, a6, a7)(f(_).run)

      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly8(a0, a1, a2, a3, a4, a5, a6, a7)(x => f(Cop(x)))

      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide8(a0, a1, a2, a3, a4, a5, a6, a7)(f(_).run)

      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply8(a0, a1, a2, a3, a4, a5, a6, a7) {
          case (i0, i1, i2, i3, i4, i5, i6, i7) =>
            f(Prod((i0, i1, i2, i3, i4, i5, i6, i7)))
        }

    }

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit val injEv: Inj[Cop, Cop] = combine[Inj[Cop, ?]].choose
    implicit def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj[Prod, ?]].divide
    implicit def injCopToProdEv(implicit M: Monoid[Prod]): Inj[Prod, Cop] = combine[Inj[Prod, ?]].choose
    implicit val injProdToVecCopEv: Inj[Vector[Cop], Prod] = combine[Inj[Vector[Cop], ?]].divide
  }

  def transformP[G[_]](nt: (F ~> G)): AndXorK8[F, A1, A2, A3, A4, A5, A6, A7, A8]#Prod => AndXorK8[G, A1, A2, A3, A4, A5, A6, A7, A8]#Prod =
    (p: AndXorK8[F, A1, A2, A3, A4, A5, A6, A7, A8]#Prod) => {
      val pr = p.run
      Prod8[G, A1, A2, A3, A4, A5, A6, A7, A8]((nt(pr._1), nt(pr._2), nt(pr._3), nt(pr._4), nt(pr._5), nt(pr._6), nt(pr._7), nt(pr._8)))
    }

  def transformC[G[_]](nt: (F ~> G)): AndXorK8[F, A1, A2, A3, A4, A5, A6, A7, A8]#Cop => AndXorK8[G, A1, A2, A3, A4, A5, A6, A7, A8]#Cop =
    (p: AndXorK8[F, A1, A2, A3, A4, A5, A6, A7, A8]#Cop) =>
      Cop8[G, A1, A2, A3, A4, A5, A6, A7, A8](
        p.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_))))))))
      )

  def subst1[G[_]]: AndXor8[G[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8]] = AndXor8[G[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8]]

  def subst2[G[_]]: AndXor8[F[A1], G[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8]] = AndXor8[F[A1], G[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8]]

  def subst3[G[_]]: AndXor8[F[A1], F[A2], G[A3], F[A4], F[A5], F[A6], F[A7], F[A8]] = AndXor8[F[A1], F[A2], G[A3], F[A4], F[A5], F[A6], F[A7], F[A8]]

  def subst4[G[_]]: AndXor8[F[A1], F[A2], F[A3], G[A4], F[A5], F[A6], F[A7], F[A8]] = AndXor8[F[A1], F[A2], F[A3], G[A4], F[A5], F[A6], F[A7], F[A8]]

  def subst5[G[_]]: AndXor8[F[A1], F[A2], F[A3], F[A4], G[A5], F[A6], F[A7], F[A8]] = AndXor8[F[A1], F[A2], F[A3], F[A4], G[A5], F[A6], F[A7], F[A8]]

  def subst6[G[_]]: AndXor8[F[A1], F[A2], F[A3], F[A4], F[A5], G[A6], F[A7], F[A8]] = AndXor8[F[A1], F[A2], F[A3], F[A4], F[A5], G[A6], F[A7], F[A8]]

  def subst7[G[_]]: AndXor8[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], G[A7], F[A8]] = AndXor8[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], G[A7], F[A8]]

  def subst8[G[_]]: AndXor8[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], G[A8]] = AndXor8[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], G[A8]]

  // format: off
  def sequenceP(prod: Prod)(implicit A: Apply[F]): F[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = {
    val p = prod.run
    A.map(
    A.ap(p._8)(
    A.ap(p._7)(
    A.ap(p._6)(
    A.ap(p._5)(
    A.ap(p._4)(
    A.ap(p._3)(
    A.ap(p._2)(
    A.map(p._1)((i0: A1) => (i1: A2) => (i2: A3) => (i3: A4) => (i4: A5) => (i5: A6) => (i6: A7) => (i7: A8) =>
      (i0, i1, i2, i3, i4, i5, i6, i7))))))))))(Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8](_))
  }

  def sequenceC(cop: Cop)(implicit FF: Functor[F]): F[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] =
    cop.run match {
      case -\/(x) => FF.map(x)(y => Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8](-\/(y)))
      case \/-(-\/(x)) => FF.map(x)(y => Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8](\/-(-\/(y))))
      case \/-(\/-(-\/(x))) => FF.map(x)(y => Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(-\/(y)))))
      case \/-(\/-(\/-(-\/(x)))) => FF.map(x)(y => Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(\/-(-\/(y))))))
      case \/-(\/-(\/-(\/-(-\/(x))))) => FF.map(x)(y => Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(\/-(\/-(-\/(y)))))))
      case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => FF.map(x)(y => Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(\/-(\/-(\/-(-\/(y))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => FF.map(x)(y => Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))) => FF.map(x)(y => Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(\/-(\/-(\/-(\/-(\/-(y)))))))))
    }

  def extractC[B](c: Cop)(implicit inj: Inj[Option[B], Cop]): Option[B] = inj(c)

  def extractP[B](p: Prod)(implicit inj: Inj[B, Prod]): B = inj(p)

  def foldMap[G[_], C](p: AndXor[G]#Prod)(map: AndXor[Id]#Cop => C)(
      implicit O: Ordering[AndXor[Id]#Cop], M: Monoid[C], PE: PlusEmpty[G], U: Uncons[G]): C = {
    import scala.collection.mutable.{PriorityQueue => PQ}

    val TG = AndXorF[G]
    val TI = AndXorF[Id]

    def uncons(p: TG.Prod): (List[TI.Cop], TG.Prod) = {
      val pr = p.run
      val ht1 = U(pr._1)
      val ht2 = U(pr._2)
      val ht3 = U(pr._3)
      val ht4 = U(pr._4)
      val ht5 = U(pr._5)
      val ht6 = U(pr._6)
      val ht7 = U(pr._7)
      val ht8 = U(pr._8)
      (List(ht1._1.map(TI.inj(_: Id[A1])), ht2._1.map(TI.inj(_: Id[A2])), ht3._1.map(TI.inj(_: Id[A3])), ht4._1.map(TI.inj(_: Id[A4])), ht5._1.map(TI.inj(_: Id[A5])), ht6._1.map(TI.inj(_: Id[A6])), ht7._1.map(TI.inj(_: Id[A7])), ht8._1.map(TI.inj(_: Id[A8]))).flatten,
        TG.Prod((ht1._2, ht2._2, ht3._2, ht4._2, ht5._2, ht6._2, ht7._2, ht8._2)))
    }
    @scala.annotation.tailrec
    def go(prod: TG.Prod, q: PQ[TI.Cop], out: C): C =
      (prod.run.==((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8]))) match {
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
              val (h, t) = U(pr._1)
              go(TG.Prod((t, pr._2, pr._3, pr._4, pr._5, pr._6, pr._7, pr._8)),
                q ++= h.map(TI.inj(_: Id[A1])), M.append(out, map(TI.inj(x))))
          }
          case \/-(-\/(x)) => {
              val pr = prod.run
              val (h, t) = U(pr._2)
              go(TG.Prod((pr._1, t, pr._3, pr._4, pr._5, pr._6, pr._7, pr._8)),
                q ++= h.map(TI.inj(_: Id[A2])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(-\/(x))) => {
              val pr = prod.run
              val (h, t) = U(pr._3)
              go(TG.Prod((pr._1, pr._2, t, pr._4, pr._5, pr._6, pr._7, pr._8)),
                q ++= h.map(TI.inj(_: Id[A3])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(-\/(x)))) => {
              val pr = prod.run
              val (h, t) = U(pr._4)
              go(TG.Prod((pr._1, pr._2, pr._3, t, pr._5, pr._6, pr._7, pr._8)),
                q ++= h.map(TI.inj(_: Id[A4])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(-\/(x))))) => {
              val pr = prod.run
              val (h, t) = U(pr._5)
              go(TG.Prod((pr._1, pr._2, pr._3, pr._4, t, pr._6, pr._7, pr._8)),
                q ++= h.map(TI.inj(_: Id[A5])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => {
              val pr = prod.run
              val (h, t) = U(pr._6)
              go(TG.Prod((pr._1, pr._2, pr._3, pr._4, pr._5, t, pr._7, pr._8)),
                q ++= h.map(TI.inj(_: Id[A6])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => {
              val pr = prod.run
              val (h, t) = U(pr._7)
              go(TG.Prod((pr._1, pr._2, pr._3, pr._4, pr._5, pr._6, t, pr._8)),
                q ++= h.map(TI.inj(_: Id[A7])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))) => {
              val pr = prod.run
              val (h, t) = U(pr._8)
              go(TG.Prod((pr._1, pr._2, pr._3, pr._4, pr._5, pr._6, pr._7, t)),
                q ++= h.map(TI.inj(_: Id[A8])), M.append(out, map(TI.inj(x))))
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

object AndXorK8 {

  def apply[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: AndXorK8[F, A1, A2, A3, A4, A5, A6, A7, A8] =
    new AndXorK8[F, A1, A2, A3, A4, A5, A6, A7, A8] {}
}

trait AndXorF8[A1, A2, A3, A4, A5, A6, A7, A8] {
  type Repr[F[_]] = AndXorK8[F, A1, A2, A3, A4, A5, A6, A7, A8]
  def apply[F[_]]: Repr[F] =
    new AndXorK8[F, A1, A2, A3, A4, A5, A6, A7, A8] {}
}

object AndXorF8 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8]: AndXorF8[A1, A2, A3, A4, A5, A6, A7, A8] =
    new AndXorF8[A1, A2, A3, A4, A5, A6, A7, A8] {}
}

trait AndXor8[A1, A2, A3, A4, A5, A6, A7, A8] extends AndXorK8[Id, A1, A2, A3, A4, A5, A6, A7, A8]

object AndXor8 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8]: AndXor8[A1, A2, A3, A4, A5, A6, A7, A8] =
    new AndXor8[A1, A2, A3, A4, A5, A6, A7, A8] {}
}
