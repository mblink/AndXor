package andxor

import andxor.tuple._
import andxor.types.{Cop12, Prod12}
import scala.annotation.tailrec
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXorK12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] extends AndXor {
  type Prod = Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  object Prod { def apply(p: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12])): Prod = Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](p) }

  type Cop = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  object Cop {
    def apply(c: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ F[A12])))))))))))): Cop =
      Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](c)
  }

  val AndXorF = AndXorF12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  type AndXor[G[_]] = AndXorF.Repr[G]

  def combineId[G[_]](
      implicit @scalaz.unused ev: F[_] =:= Id[_],
      a0: G[A1],
      a1: G[A2],
      a2: G[A3],
      a3: G[A4],
      a4: G[A5],
      a5: G[A6],
      a6: G[A7],
      a7: G[A8],
      a8: G[A9],
      a9: G[A10],
      a10: G[A11],
      a11: G[A12]
  ): ComposeAndXor[G, Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
    AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].combine[G]

  def combine[G[_]](
      implicit a0: G[F[A1]],
      a1: G[F[A2]],
      a2: G[F[A3]],
      a3: G[F[A4]],
      a4: G[F[A5]],
      a5: G[F[A6]],
      a6: G[F[A7]],
      a7: G[F[A8]],
      a8: G[F[A9]],
      a9: G[F[A10]],
      a10: G[F[A11]],
      a11: G[F[A12]]
  ): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose12(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(f(_).run)

      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly12(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(x => f(Cop(x)))

      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide12(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(f(_).run)

      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply12(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) {
          case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11) =>
            f(Prod((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11)))
        }

    }

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit val injEv: Inj[Cop, Cop] = combine[Inj[Cop, ?]].choose
    implicit def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj[Prod, ?]].divide
    implicit def injCopToProdEv(implicit M: Monoid[Prod]): Inj[Prod, Cop] = combine[Inj[Prod, ?]].choose
    implicit val injProdToVecCopEv: Inj[Vector[Cop], Prod] = combine[Inj[Vector[Cop], ?]].divide
  }

  def transformP[G[_]](nt: (F ~> G)): AndXorK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod => AndXorK12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod =
    (p: AndXorK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Prod) => {
      val pr = p.run
      Prod12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        (nt(pr.t1), nt(pr.t2), nt(pr.t3), nt(pr.t4), nt(pr.t5), nt(pr.t6), nt(pr.t7), nt(pr.t8), nt(pr.t9), nt(pr.t10), nt(pr.t11), nt(pr.t12))
      )
    }

  def transformC[G[_]](nt: (F ~> G)): AndXorK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop => AndXorK12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop =
    (p: AndXorK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]#Cop) =>
      Cop12[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        p.run.bimap(
          nt(_),
          _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))))))))
        )
      )

  def subst1[G[_]]: AndXor12[G[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]] =
    AndXor12[G[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]]

  def subst2[G[_]]: AndXor12[F[A1], G[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]] =
    AndXor12[F[A1], G[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]]

  def subst3[G[_]]: AndXor12[F[A1], F[A2], G[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]] =
    AndXor12[F[A1], F[A2], G[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]]

  def subst4[G[_]]: AndXor12[F[A1], F[A2], F[A3], G[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]] =
    AndXor12[F[A1], F[A2], F[A3], G[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]]

  def subst5[G[_]]: AndXor12[F[A1], F[A2], F[A3], F[A4], G[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]] =
    AndXor12[F[A1], F[A2], F[A3], F[A4], G[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]]

  def subst6[G[_]]: AndXor12[F[A1], F[A2], F[A3], F[A4], F[A5], G[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]] =
    AndXor12[F[A1], F[A2], F[A3], F[A4], F[A5], G[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]]

  def subst7[G[_]]: AndXor12[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], G[A7], F[A8], F[A9], F[A10], F[A11], F[A12]] =
    AndXor12[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], G[A7], F[A8], F[A9], F[A10], F[A11], F[A12]]

  def subst8[G[_]]: AndXor12[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], G[A8], F[A9], F[A10], F[A11], F[A12]] =
    AndXor12[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], G[A8], F[A9], F[A10], F[A11], F[A12]]

  def subst9[G[_]]: AndXor12[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], G[A9], F[A10], F[A11], F[A12]] =
    AndXor12[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], G[A9], F[A10], F[A11], F[A12]]

  def subst10[G[_]]: AndXor12[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], G[A10], F[A11], F[A12]] =
    AndXor12[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], G[A10], F[A11], F[A12]]

  def subst11[G[_]]: AndXor12[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], G[A11], F[A12]] =
    AndXor12[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], G[A11], F[A12]]

  def subst12[G[_]]: AndXor12[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], G[A12]] =
    AndXor12[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], G[A12]]

  // format: off
  def sequenceP(prod: Prod)(implicit A: Apply[F]): F[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = {
    val p = prod.run
    A.map(
    A.ap(p.t12)(
    A.ap(p.t11)(
    A.ap(p.t10)(
    A.ap(p.t9)(
    A.ap(p.t8)(
    A.ap(p.t7)(
    A.ap(p.t6)(
    A.ap(p.t5)(
    A.ap(p.t4)(
    A.ap(p.t3)(
    A.ap(p.t2)(
    A.map(p.t1)((i0: A1) => (i1: A2) => (i2: A3) => (i3: A4) => (i4: A5) => (i5: A6) => (i6: A7) => (i7: A8) => (i8: A9) => (i9: A10) => (i10: A11) => (i11: A12) =>
      (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11))))))))))))))(Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](_))
  }

  def sequenceC(cop: Cop)(implicit FF: Functor[F]): F[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
    cop.run match {
      case -\/(x) => FF.map(x)(y => Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](-\/(y)))
      case \/-(-\/(x)) => FF.map(x)(y => Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(-\/(y))))
      case \/-(\/-(-\/(x))) => FF.map(x)(y => Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(-\/(y)))))
      case \/-(\/-(\/-(-\/(x)))) => FF.map(x)(y => Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(-\/(y))))))
      case \/-(\/-(\/-(\/-(-\/(x))))) => FF.map(x)(y => Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(-\/(y)))))))
      case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => FF.map(x)(y => Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(-\/(y))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => FF.map(x)(y => Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => FF.map(x)(y => Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => FF.map(x)(y => Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => FF.map(x)(y => Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => FF.map(x)(y => Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))) => FF.map(x)(y => Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y)))))))))))))
    }

  def extractC[B](c: Cop)(implicit inj: Inj[Option[B], Cop]): Option[B] = inj(c)

  def extractP[B](p: Prod)(implicit inj: Inj[B, Prod]): B = inj(p)

  def foldMap[G[_], C](p: AndXor[G]#Prod)(map: AndXor[Id]#Cop => C)(
      implicit O: Ordering[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]], M: Monoid[C], PE: PlusEmpty[G], U: Uncons[G]): C = {
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
      val ht10 = U(pr.t10)
      val ht11 = U(pr.t11)
      val ht12 = U(pr.t12)
      (List(ht1._1.map(TI.inj(_: Id[A1])), ht2._1.map(TI.inj(_: Id[A2])), ht3._1.map(TI.inj(_: Id[A3])), ht4._1.map(TI.inj(_: Id[A4])), ht5._1.map(TI.inj(_: Id[A5])), ht6._1.map(TI.inj(_: Id[A6])), ht7._1.map(TI.inj(_: Id[A7])), ht8._1.map(TI.inj(_: Id[A8])), ht9._1.map(TI.inj(_: Id[A9])), ht10._1.map(TI.inj(_: Id[A10])), ht11._1.map(TI.inj(_: Id[A11])), ht12._1.map(TI.inj(_: Id[A12]))).flatten,
        TG.Prod((ht1._2, ht2._2, ht3._2, ht4._2, ht5._2, ht6._2, ht7._2, ht8._2, ht9._2, ht10._2, ht11._2, ht12._2)))
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
      (prod.run.==((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8], PE.empty[A9], PE.empty[A10], PE.empty[A11], PE.empty[A12]))) match {
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
              val (h, t) = U(pr.t1)
              go(TG.Prod((t, pr.t2, pr.t3, pr.t4, pr.t5, pr.t6, pr.t7, pr.t8, pr.t9, pr.t10, pr.t11, pr.t12)),
                q ++= h.map(TI.inj(_: Id[A1])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(-\/(_)) =>
              val pr = prod.run
              val (h, t) = U(pr.t2)
              go(TG.Prod((pr.t1, t, pr.t3, pr.t4, pr.t5, pr.t6, pr.t7, pr.t8, pr.t9, pr.t10, pr.t11, pr.t12)),
                q ++= h.map(TI.inj(_: Id[A2])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(-\/(_))) =>
              val pr = prod.run
              val (h, t) = U(pr.t3)
              go(TG.Prod((pr.t1, pr.t2, t, pr.t4, pr.t5, pr.t6, pr.t7, pr.t8, pr.t9, pr.t10, pr.t11, pr.t12)),
                q ++= h.map(TI.inj(_: Id[A3])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(-\/(_)))) =>
              val pr = prod.run
              val (h, t) = U(pr.t4)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, t, pr.t5, pr.t6, pr.t7, pr.t8, pr.t9, pr.t10, pr.t11, pr.t12)),
                q ++= h.map(TI.inj(_: Id[A4])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(-\/(_))))) =>
              val pr = prod.run
              val (h, t) = U(pr.t5)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, t, pr.t6, pr.t7, pr.t8, pr.t9, pr.t10, pr.t11, pr.t12)),
                q ++= h.map(TI.inj(_: Id[A5])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(-\/(_)))))) =>
              val pr = prod.run
              val (h, t) = U(pr.t6)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, pr.t5, t, pr.t7, pr.t8, pr.t9, pr.t10, pr.t11, pr.t12)),
                q ++= h.map(TI.inj(_: Id[A6])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))) =>
              val pr = prod.run
              val (h, t) = U(pr.t7)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, pr.t5, pr.t6, t, pr.t8, pr.t9, pr.t10, pr.t11, pr.t12)),
                q ++= h.map(TI.inj(_: Id[A7])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))) =>
              val pr = prod.run
              val (h, t) = U(pr.t8)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, pr.t5, pr.t6, pr.t7, t, pr.t9, pr.t10, pr.t11, pr.t12)),
                q ++= h.map(TI.inj(_: Id[A8])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))) =>
              val pr = prod.run
              val (h, t) = U(pr.t9)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, pr.t5, pr.t6, pr.t7, pr.t8, t, pr.t10, pr.t11, pr.t12)),
                q ++= h.map(TI.inj(_: Id[A9])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))) =>
              val pr = prod.run
              val (h, t) = U(pr.t10)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, pr.t5, pr.t6, pr.t7, pr.t8, pr.t9, t, pr.t11, pr.t12)),
                q ++= h.map(TI.inj(_: Id[A10])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))) =>
              val pr = prod.run
              val (h, t) = U(pr.t11)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, pr.t5, pr.t6, pr.t7, pr.t8, pr.t9, pr.t10, t, pr.t12)),
                q ++= h.map(TI.inj(_: Id[A11])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(_))))))))))) =>
              val pr = prod.run
              val (h, t) = U(pr.t12)
              go(TG.Prod((pr.t1, pr.t2, pr.t3, pr.t4, pr.t5, pr.t6, pr.t7, pr.t8, pr.t9, pr.t10, pr.t11, t)),
                q ++= h.map(TI.inj(_: Id[A12])), M.append(out, map(TI.Cop(dj))))

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

object AndXorK12 {

  def apply[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: AndXorK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
    new AndXorK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}
}

trait AndXorF12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {
  type Repr[F[_]] = AndXorK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  def apply[F[_]]: Repr[F] =
    new AndXorK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}
}

object AndXorF12 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: AndXorF12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
    new AndXorF12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}
}

trait AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] extends AndXorK12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

object AndXor12 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
    new AndXor12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}
}
