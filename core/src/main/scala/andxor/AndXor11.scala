package andxor

import andxor.types.{Cop11, Prod11}
import scala.annotation.tailrec
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXorK11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] extends AndXor {
  type Prod = Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
  object Prod { def apply(p: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11])): Prod = Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](p) }

  type Cop = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
  object Cop {
    def apply(c: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ F[A11]))))))))))): Cop = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](c)
  }

  val AndXorF = AndXorF11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
  type AndXor[G[_]] = AndXorF.Repr[G]

  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]], a3: G[F[A4]], a4: G[F[A5]], a5: G[F[A6]], a6: G[F[A7]], a7: G[F[A8]], a8: G[F[A9]], a9: G[F[A10]], a10: G[F[A11]])
      : ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose11(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)(f(_).run)

      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly11(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)(x => f(Cop(x)))

      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide11(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)(f(_).run)

      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply11(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) {
          case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10) =>
            f(Prod((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10)))
        }

    }

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit val injEv: Inj[Cop, Cop] = combine[Inj[Cop, ?]].choose
    implicit def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj[Prod, ?]].divide
    implicit def injCopToProdEv(implicit M: Monoid[Prod]): Inj[Prod, Cop] = combine[Inj[Prod, ?]].choose
    implicit val injProdToVecCopEv: Inj[Vector[Cop], Prod] = combine[Inj[Vector[Cop], ?]].divide
  }

  def transformP[G[_]](nt: (F ~> G)): AndXorK11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]#Prod => AndXorK11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]#Prod =
    (p: AndXorK11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]#Prod) => {
      val pr = p.run
      Prod11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((nt(pr._1), nt(pr._2), nt(pr._3), nt(pr._4), nt(pr._5), nt(pr._6), nt(pr._7), nt(pr._8), nt(pr._9), nt(pr._10), nt(pr._11)))
    }

  def transformC[G[_]](nt: (F ~> G)): AndXorK11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]#Cop => AndXorK11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]#Cop =
    (p: AndXorK11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]#Cop) =>
      Cop11[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        p.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))))))))
      )

  def subst1[G[_]]: AndXor11[G[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]] = AndXor11[G[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]]

  def subst2[G[_]]: AndXor11[F[A1], G[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]] = AndXor11[F[A1], G[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]]

  def subst3[G[_]]: AndXor11[F[A1], F[A2], G[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]] = AndXor11[F[A1], F[A2], G[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]]

  def subst4[G[_]]: AndXor11[F[A1], F[A2], F[A3], G[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]] = AndXor11[F[A1], F[A2], F[A3], G[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]]

  def subst5[G[_]]: AndXor11[F[A1], F[A2], F[A3], F[A4], G[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]] = AndXor11[F[A1], F[A2], F[A3], F[A4], G[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]]

  def subst6[G[_]]: AndXor11[F[A1], F[A2], F[A3], F[A4], F[A5], G[A6], F[A7], F[A8], F[A9], F[A10], F[A11]] = AndXor11[F[A1], F[A2], F[A3], F[A4], F[A5], G[A6], F[A7], F[A8], F[A9], F[A10], F[A11]]

  def subst7[G[_]]: AndXor11[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], G[A7], F[A8], F[A9], F[A10], F[A11]] = AndXor11[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], G[A7], F[A8], F[A9], F[A10], F[A11]]

  def subst8[G[_]]: AndXor11[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], G[A8], F[A9], F[A10], F[A11]] = AndXor11[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], G[A8], F[A9], F[A10], F[A11]]

  def subst9[G[_]]: AndXor11[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], G[A9], F[A10], F[A11]] = AndXor11[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], G[A9], F[A10], F[A11]]

  def subst10[G[_]]: AndXor11[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], G[A10], F[A11]] = AndXor11[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], G[A10], F[A11]]

  def subst11[G[_]]: AndXor11[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], G[A11]] = AndXor11[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], G[A11]]

  // format: off
  def sequenceP(prod: Prod)(implicit A: Apply[F]): F[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = {
    val p = prod.run
    A.map(
    A.ap(p._11)(
    A.ap(p._10)(
    A.ap(p._9)(
    A.ap(p._8)(
    A.ap(p._7)(
    A.ap(p._6)(
    A.ap(p._5)(
    A.ap(p._4)(
    A.ap(p._3)(
    A.ap(p._2)(
    A.map(p._1)((i0: A1) => (i1: A2) => (i2: A3) => (i3: A4) => (i4: A5) => (i5: A6) => (i6: A7) => (i7: A8) => (i8: A9) => (i9: A10) => (i10: A11) =>
      (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10)))))))))))))(Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](_))
  }

  def sequenceC(cop: Cop)(implicit FF: Functor[F]): F[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
    cop.run match {
      case -\/(x) => FF.map(x)(y => Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](-\/(y)))
      case \/-(-\/(x)) => FF.map(x)(y => Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(-\/(y))))
      case \/-(\/-(-\/(x))) => FF.map(x)(y => Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(-\/(y)))))
      case \/-(\/-(\/-(-\/(x)))) => FF.map(x)(y => Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(-\/(y))))))
      case \/-(\/-(\/-(\/-(-\/(x))))) => FF.map(x)(y => Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(-\/(y)))))))
      case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => FF.map(x)(y => Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(-\/(y))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => FF.map(x)(y => Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => FF.map(x)(y => Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => FF.map(x)(y => Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => FF.map(x)(y => Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))) => FF.map(x)(y => Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y))))))))))))
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
      val ht9 = U(pr._9)
      val ht10 = U(pr._10)
      val ht11 = U(pr._11)
      (List(ht1._1.map(TI.inj(_: Id[A1])), ht2._1.map(TI.inj(_: Id[A2])), ht3._1.map(TI.inj(_: Id[A3])), ht4._1.map(TI.inj(_: Id[A4])), ht5._1.map(TI.inj(_: Id[A5])), ht6._1.map(TI.inj(_: Id[A6])), ht7._1.map(TI.inj(_: Id[A7])), ht8._1.map(TI.inj(_: Id[A8])), ht9._1.map(TI.inj(_: Id[A9])), ht10._1.map(TI.inj(_: Id[A10])), ht11._1.map(TI.inj(_: Id[A11]))).flatten,
        TG.Prod((ht1._2, ht2._2, ht3._2, ht4._2, ht5._2, ht6._2, ht7._2, ht8._2, ht9._2, ht10._2, ht11._2)))
    }

    @tailrec
    def appendAll(out: C, q: PQ[TI.Cop]): C =
      q.isEmpty match {
        case true => out
        case false =>
          val newOut = M.append(out, map(q.dequeue))
          appendAll(newOut, q)
      }

    @tailrec
    def go(prod: TG.Prod, q: PQ[TI.Cop], out: C): C =
      (prod.run.==((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8], PE.empty[A9], PE.empty[A10], PE.empty[A11]))) match {
        case true => appendAll(out, q)
        case false => q.isEmpty match {
          case true => {
            val (hs, ts) = uncons(prod)
            q ++= hs
            go(ts, q, out)
          }
          case false => q.dequeue.run match {
            case dj @ -\/(_) =>
              val pr = prod.run
              val (h, t) = U(pr._1)
              go(TG.Prod((t, pr._2, pr._3, pr._4, pr._5, pr._6, pr._7, pr._8, pr._9, pr._10, pr._11)),
                q ++= h.map(TI.inj(_: Id[A1])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(-\/(_)) =>
              val pr = prod.run
              val (h, t) = U(pr._2)
              go(TG.Prod((pr._1, t, pr._3, pr._4, pr._5, pr._6, pr._7, pr._8, pr._9, pr._10, pr._11)),
                q ++= h.map(TI.inj(_: Id[A2])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(-\/(_))) =>
              val pr = prod.run
              val (h, t) = U(pr._3)
              go(TG.Prod((pr._1, pr._2, t, pr._4, pr._5, pr._6, pr._7, pr._8, pr._9, pr._10, pr._11)),
                q ++= h.map(TI.inj(_: Id[A3])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(-\/(_)))) =>
              val pr = prod.run
              val (h, t) = U(pr._4)
              go(TG.Prod((pr._1, pr._2, pr._3, t, pr._5, pr._6, pr._7, pr._8, pr._9, pr._10, pr._11)),
                q ++= h.map(TI.inj(_: Id[A4])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(-\/(_))))) =>
              val pr = prod.run
              val (h, t) = U(pr._5)
              go(TG.Prod((pr._1, pr._2, pr._3, pr._4, t, pr._6, pr._7, pr._8, pr._9, pr._10, pr._11)),
                q ++= h.map(TI.inj(_: Id[A5])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(-\/(_)))))) =>
              val pr = prod.run
              val (h, t) = U(pr._6)
              go(TG.Prod((pr._1, pr._2, pr._3, pr._4, pr._5, t, pr._7, pr._8, pr._9, pr._10, pr._11)),
                q ++= h.map(TI.inj(_: Id[A6])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))) =>
              val pr = prod.run
              val (h, t) = U(pr._7)
              go(TG.Prod((pr._1, pr._2, pr._3, pr._4, pr._5, pr._6, t, pr._8, pr._9, pr._10, pr._11)),
                q ++= h.map(TI.inj(_: Id[A7])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))) =>
              val pr = prod.run
              val (h, t) = U(pr._8)
              go(TG.Prod((pr._1, pr._2, pr._3, pr._4, pr._5, pr._6, pr._7, t, pr._9, pr._10, pr._11)),
                q ++= h.map(TI.inj(_: Id[A8])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))) =>
              val pr = prod.run
              val (h, t) = U(pr._9)
              go(TG.Prod((pr._1, pr._2, pr._3, pr._4, pr._5, pr._6, pr._7, pr._8, t, pr._10, pr._11)),
                q ++= h.map(TI.inj(_: Id[A9])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))) =>
              val pr = prod.run
              val (h, t) = U(pr._10)
              go(TG.Prod((pr._1, pr._2, pr._3, pr._4, pr._5, pr._6, pr._7, pr._8, pr._9, t, pr._11)),
                q ++= h.map(TI.inj(_: Id[A10])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(_)))))))))) =>
              val pr = prod.run
              val (h, t) = U(pr._11)
              go(TG.Prod((pr._1, pr._2, pr._3, pr._4, pr._5, pr._6, pr._7, pr._8, pr._9, pr._10, t)),
                q ++= h.map(TI.inj(_: Id[A11])), M.append(out, map(TI.Cop(dj))))

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

object AndXorK11 {

  def apply[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: AndXorK11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
    new AndXorK11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}
}

trait AndXorF11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {
  type Repr[F[_]] = AndXorK11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
  def apply[F[_]]: Repr[F] =
    new AndXorK11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}
}

object AndXorF11 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: AndXorF11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
    new AndXorF11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}
}

trait AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] extends AndXorK11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

object AndXor11 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] =
    new AndXor11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}
}
