package andxor

import andxor.types.{Cop6, Prod6}
import scala.annotation.tailrec
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXorK6[F[_], A1, A2, A3, A4, A5, A6] extends AndXor {
  type Prod = Prod6[F, A1, A2, A3, A4, A5, A6]
  object Prod { def apply(p: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6])): Prod = Prod6[F, A1, A2, A3, A4, A5, A6](p) }

  type Cop = Cop6[F, A1, A2, A3, A4, A5, A6]
  object Cop { def apply(c: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ F[A6])))))): Cop = Cop6[F, A1, A2, A3, A4, A5, A6](c) }

  val AndXorF = AndXorF6[A1, A2, A3, A4, A5, A6]
  type AndXor[G[_]] = AndXorF.Repr[G]

  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]], a3: G[F[A4]], a4: G[F[A5]], a5: G[F[A6]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose6(a0, a1, a2, a3, a4, a5)(f(_).run)

      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly6(a0, a1, a2, a3, a4, a5)(x => f(Cop(x)))

      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide6(a0, a1, a2, a3, a4, a5)(f(_).run)

      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply6(a0, a1, a2, a3, a4, a5) {
          case (i0, i1, i2, i3, i4, i5) =>
            f(Prod((i0, i1, i2, i3, i4, i5)))
        }

    }

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit val injEv: Inj[Cop, Cop] = combine[Inj[Cop, ?]].choose
    implicit def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj[Prod, ?]].divide
    implicit def injCopToProdEv(implicit M: Monoid[Prod]): Inj[Prod, Cop] = combine[Inj[Prod, ?]].choose
    implicit val injProdToVecCopEv: Inj[Vector[Cop], Prod] = combine[Inj[Vector[Cop], ?]].divide
  }

  def transformP[G[_]](nt: (F ~> G)): AndXorK6[F, A1, A2, A3, A4, A5, A6]#Prod => AndXorK6[G, A1, A2, A3, A4, A5, A6]#Prod =
    (p: AndXorK6[F, A1, A2, A3, A4, A5, A6]#Prod) => {
      val pr = p.run
      Prod6[G, A1, A2, A3, A4, A5, A6]((nt(pr._1), nt(pr._2), nt(pr._3), nt(pr._4), nt(pr._5), nt(pr._6)))
    }

  def transformC[G[_]](nt: (F ~> G)): AndXorK6[F, A1, A2, A3, A4, A5, A6]#Cop => AndXorK6[G, A1, A2, A3, A4, A5, A6]#Cop =
    (p: AndXorK6[F, A1, A2, A3, A4, A5, A6]#Cop) =>
      Cop6[G, A1, A2, A3, A4, A5, A6](
        p.run.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_))))))
      )

  def subst1[G[_]]: AndXor6[G[A1], F[A2], F[A3], F[A4], F[A5], F[A6]] = AndXor6[G[A1], F[A2], F[A3], F[A4], F[A5], F[A6]]

  def subst2[G[_]]: AndXor6[F[A1], G[A2], F[A3], F[A4], F[A5], F[A6]] = AndXor6[F[A1], G[A2], F[A3], F[A4], F[A5], F[A6]]

  def subst3[G[_]]: AndXor6[F[A1], F[A2], G[A3], F[A4], F[A5], F[A6]] = AndXor6[F[A1], F[A2], G[A3], F[A4], F[A5], F[A6]]

  def subst4[G[_]]: AndXor6[F[A1], F[A2], F[A3], G[A4], F[A5], F[A6]] = AndXor6[F[A1], F[A2], F[A3], G[A4], F[A5], F[A6]]

  def subst5[G[_]]: AndXor6[F[A1], F[A2], F[A3], F[A4], G[A5], F[A6]] = AndXor6[F[A1], F[A2], F[A3], F[A4], G[A5], F[A6]]

  def subst6[G[_]]: AndXor6[F[A1], F[A2], F[A3], F[A4], F[A5], G[A6]] = AndXor6[F[A1], F[A2], F[A3], F[A4], F[A5], G[A6]]

  // format: off
  def sequenceP(prod: Prod)(implicit A: Apply[F]): F[Prod6[Id, A1, A2, A3, A4, A5, A6]] = {
    val p = prod.run
    A.map(
    A.ap(p._6)(
    A.ap(p._5)(
    A.ap(p._4)(
    A.ap(p._3)(
    A.ap(p._2)(
    A.map(p._1)((i0: A1) => (i1: A2) => (i2: A3) => (i3: A4) => (i4: A5) => (i5: A6) =>
      (i0, i1, i2, i3, i4, i5))))))))(Prod6[Id, A1, A2, A3, A4, A5, A6](_))
  }

  def sequenceC(cop: Cop)(implicit FF: Functor[F]): F[Cop6[Id, A1, A2, A3, A4, A5, A6]] =
    cop.run match {
      case -\/(x) => FF.map(x)(y => Cop6[Id, A1, A2, A3, A4, A5, A6](-\/(y)))
      case \/-(-\/(x)) => FF.map(x)(y => Cop6[Id, A1, A2, A3, A4, A5, A6](\/-(-\/(y))))
      case \/-(\/-(-\/(x))) => FF.map(x)(y => Cop6[Id, A1, A2, A3, A4, A5, A6](\/-(\/-(-\/(y)))))
      case \/-(\/-(\/-(-\/(x)))) => FF.map(x)(y => Cop6[Id, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(-\/(y))))))
      case \/-(\/-(\/-(\/-(-\/(x))))) => FF.map(x)(y => Cop6[Id, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(\/-(-\/(y)))))))
      case \/-(\/-(\/-(\/-(\/-(x))))) => FF.map(x)(y => Cop6[Id, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(\/-(\/-(y)))))))
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
      (List(ht1._1.map(TI.inj(_: Id[A1])), ht2._1.map(TI.inj(_: Id[A2])), ht3._1.map(TI.inj(_: Id[A3])), ht4._1.map(TI.inj(_: Id[A4])), ht5._1.map(TI.inj(_: Id[A5])), ht6._1.map(TI.inj(_: Id[A6]))).flatten,
        TG.Prod((ht1._2, ht2._2, ht3._2, ht4._2, ht5._2, ht6._2)))
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
      (prod.run.==((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6]))) match {
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
              go(TG.Prod((t, pr._2, pr._3, pr._4, pr._5, pr._6)),
                q ++= h.map(TI.inj(_: Id[A1])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(-\/(_)) =>
              val pr = prod.run
              val (h, t) = U(pr._2)
              go(TG.Prod((pr._1, t, pr._3, pr._4, pr._5, pr._6)),
                q ++= h.map(TI.inj(_: Id[A2])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(-\/(_))) =>
              val pr = prod.run
              val (h, t) = U(pr._3)
              go(TG.Prod((pr._1, pr._2, t, pr._4, pr._5, pr._6)),
                q ++= h.map(TI.inj(_: Id[A3])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(-\/(_)))) =>
              val pr = prod.run
              val (h, t) = U(pr._4)
              go(TG.Prod((pr._1, pr._2, pr._3, t, pr._5, pr._6)),
                q ++= h.map(TI.inj(_: Id[A4])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(-\/(_))))) =>
              val pr = prod.run
              val (h, t) = U(pr._5)
              go(TG.Prod((pr._1, pr._2, pr._3, pr._4, t, pr._6)),
                q ++= h.map(TI.inj(_: Id[A5])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(_))))) =>
              val pr = prod.run
              val (h, t) = U(pr._6)
              go(TG.Prod((pr._1, pr._2, pr._3, pr._4, pr._5, t)),
                q ++= h.map(TI.inj(_: Id[A6])), M.append(out, map(TI.Cop(dj))))

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

object AndXorK6 {

  def apply[F[_], A1, A2, A3, A4, A5, A6]: AndXorK6[F, A1, A2, A3, A4, A5, A6] =
    new AndXorK6[F, A1, A2, A3, A4, A5, A6] {}
}

trait AndXorF6[A1, A2, A3, A4, A5, A6] {
  type Repr[F[_]] = AndXorK6[F, A1, A2, A3, A4, A5, A6]
  def apply[F[_]]: Repr[F] =
    new AndXorK6[F, A1, A2, A3, A4, A5, A6] {}
}

object AndXorF6 {
  def apply[A1, A2, A3, A4, A5, A6]: AndXorF6[A1, A2, A3, A4, A5, A6] =
    new AndXorF6[A1, A2, A3, A4, A5, A6] {}
}

trait AndXor6[A1, A2, A3, A4, A5, A6] extends AndXorK6[Id, A1, A2, A3, A4, A5, A6]

object AndXor6 {
  def apply[A1, A2, A3, A4, A5, A6]: AndXor6[A1, A2, A3, A4, A5, A6] =
    new AndXor6[A1, A2, A3, A4, A5, A6] {}
}
