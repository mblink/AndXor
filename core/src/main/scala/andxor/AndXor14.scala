package andxor

import andxor.tuple._
import andxor.types.{Cop14, Prod14}
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id

trait AndXorK14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] extends AndXor {
  type Prod = Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  object Prod {
    def apply(p: (F[A1], (F[A2], (F[A3], (F[A4], (F[A5], (F[A6], (F[A7], (F[A8], (F[A9], (F[A10], (F[A11], (F[A12], (F[A13], F[A14])))))))))))))): Prod =
      Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](p)
  }

  type Cop = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  object Cop {
    def apply(c: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14])))))))))))))): Cop =
      Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](c)
  }

  val AndXorF = AndXorF14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  type AndXor[G[_]] = AndXorF.Repr[G]

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
      a11: G[F[A12]],
      a12: G[F[A13]],
      a13: G[F[A14]]
  ): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(f(_).run)

      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(x => f(Cop(x)))

      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(f(_).run)

      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) {
          case (i0, (i1, (i2, (i3, (i4, (i5, (i6, (i7, (i8, (i9, (i10, (i11, (i12, i13))))))))))))) =>
            f(Prod((i0, (i1, (i2, (i3, (i4, (i5, (i6, (i7, (i8, (i9, (i10, (i11, (i12, i13)))))))))))))))
        }

    }

  val injEv = combine[Inj[Cop, ?]].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj[Prod, ?]].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Prod => AndXorK14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Prod =
    (p: AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Prod) => {
      val pr = p.run
      Prod14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        (nt(pr.t1), (nt(pr.t2), (nt(pr.t3), (nt(pr.t4), (nt(pr.t5), (nt(pr.t6), (nt(pr.t7), (nt(pr.t8), (nt(pr.t9), (nt(pr.t10), (nt(pr.t11), (nt(pr.t12), (nt(pr.t13), nt(pr.t14))))))))))))))
      )
    }

  def transformC[G[_]](nt: (F ~> G)): AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Cop => AndXorK14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Cop =
    (p: AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Cop) =>
      Cop14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        p.run.bimap(
          nt(_),
          _.bimap(
            nt(_),
            _.bimap(
              nt(_),
              _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))))))))
            )
          )
        )
      )

  def subst1[G[_]]: AndXor14[G[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]] =
    AndXor14[G[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]]

  def subst2[G[_]]: AndXor14[F[A1], G[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]] =
    AndXor14[F[A1], G[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]]

  def subst3[G[_]]: AndXor14[F[A1], F[A2], G[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]] =
    AndXor14[F[A1], F[A2], G[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]]

  def subst4[G[_]]: AndXor14[F[A1], F[A2], F[A3], G[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]] =
    AndXor14[F[A1], F[A2], F[A3], G[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]]

  def subst5[G[_]]: AndXor14[F[A1], F[A2], F[A3], F[A4], G[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]] =
    AndXor14[F[A1], F[A2], F[A3], F[A4], G[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]]

  def subst6[G[_]]: AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], G[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]] =
    AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], G[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]]

  def subst7[G[_]]: AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], G[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]] =
    AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], G[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]]

  def subst8[G[_]]: AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], G[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]] =
    AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], G[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]]

  def subst9[G[_]]: AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], G[A9], F[A10], F[A11], F[A12], F[A13], F[A14]] =
    AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], G[A9], F[A10], F[A11], F[A12], F[A13], F[A14]]

  def subst10[G[_]]: AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], G[A10], F[A11], F[A12], F[A13], F[A14]] =
    AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], G[A10], F[A11], F[A12], F[A13], F[A14]]

  def subst11[G[_]]: AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], G[A11], F[A12], F[A13], F[A14]] =
    AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], G[A11], F[A12], F[A13], F[A14]]

  def subst12[G[_]]: AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], G[A12], F[A13], F[A14]] =
    AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], G[A12], F[A13], F[A14]]

  def subst13[G[_]]: AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], G[A13], F[A14]] =
    AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], G[A13], F[A14]]

  def subst14[G[_]]: AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], G[A14]] =
    AndXor14[F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], G[A14]]

  // format: off
  def sequenceP(prod: Prod)(implicit A: Apply[F]): F[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = {
    val p = prod.run
    A.map(
    A.ap(p.t14)(
    A.ap(p.t13)(
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
    A.map(p.t1)((i0: A1) => (i1: A2) => (i2: A3) => (i3: A4) => (i4: A5) => (i5: A6) => (i6: A7) => (i7: A8) => (i8: A9) => (i9: A10) => (i10: A11) => (i11: A12) => (i12: A13) => (i13: A14) =>
      (i0, (i1, (i2, (i3, (i4, (i5, (i6, (i7, (i8, (i9, (i10, (i11, (i12, i13))))))))))))))))))))))))))))(Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](_))
  }

  def sequenceC(cop: Cop)(implicit FF: Functor[F]): F[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
    cop.run match {
      case -\/(x) => FF.map(x)(y => Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](-\/(y)))
      case \/-(-\/(x)) => FF.map(x)(y => Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(-\/(y))))
      case \/-(\/-(-\/(x))) => FF.map(x)(y => Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(-\/(y)))))
      case \/-(\/-(\/-(-\/(x)))) => FF.map(x)(y => Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(-\/(y))))))
      case \/-(\/-(\/-(\/-(-\/(x))))) => FF.map(x)(y => Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(-\/(y)))))))
      case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => FF.map(x)(y => Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(-\/(y))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => FF.map(x)(y => Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => FF.map(x)(y => Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => FF.map(x)(y => Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => FF.map(x)(y => Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => FF.map(x)(y => Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => FF.map(x)(y => Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => FF.map(x)(y => Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))) => FF.map(x)(y => Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y)))))))))))))))
    }

  def extractC[B](c: Cop)(implicit inj: Inj[Option[B], Cop]): Option[B] = inj(c)

  def extractP[B](p: Prod)(implicit inj: Inj[B, Prod]): B = inj(p)

  def foldMap[G[_], C](p: AndXor[G]#Prod)(map: AndXor[Id]#Cop => C)(
      implicit O: Ordering[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]], M: Monoid[C], PE: PlusEmpty[G], U: Uncons[G]): C = {
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
      val ht13 = U(pr.t13)
      val ht14 = U(pr.t14)
      (List(ht1._1.map(TI.inj(_: Id[A1])), ht2._1.map(TI.inj(_: Id[A2])), ht3._1.map(TI.inj(_: Id[A3])), ht4._1.map(TI.inj(_: Id[A4])), ht5._1.map(TI.inj(_: Id[A5])), ht6._1.map(TI.inj(_: Id[A6])), ht7._1.map(TI.inj(_: Id[A7])), ht8._1.map(TI.inj(_: Id[A8])), ht9._1.map(TI.inj(_: Id[A9])), ht10._1.map(TI.inj(_: Id[A10])), ht11._1.map(TI.inj(_: Id[A11])), ht12._1.map(TI.inj(_: Id[A12])), ht13._1.map(TI.inj(_: Id[A13])), ht14._1.map(TI.inj(_: Id[A14]))).flatten,
        TG.Prod((ht1._2, (ht2._2, (ht3._2, (ht4._2, (ht5._2, (ht6._2, (ht7._2, (ht8._2, (ht9._2, (ht10._2, (ht11._2, (ht12._2, (ht13._2, ht14._2)))))))))))))))
    }
    @scala.annotation.tailrec
    def go(prod: TG.Prod, q: PQ[TI.Cop], out: C): C =
      (prod.run.==((PE.empty[A1], (PE.empty[A2], (PE.empty[A3], (PE.empty[A4], (PE.empty[A5], (PE.empty[A6], (PE.empty[A7], (PE.empty[A8], (PE.empty[A9], (PE.empty[A10], (PE.empty[A11], (PE.empty[A12], (PE.empty[A13], PE.empty[A14]))))))))))))))) match {
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
              go(TG.Prod((t, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, pr.t14)))))))))))))),
                q ++= h.map(TI.inj(_: Id[A1])), M.append(out, map(TI.inj(x))))
          }
          case \/-(-\/(x)) => {
              val pr = prod.run
              val (h, t) = U(pr.t2)
              go(TG.Prod((pr.t1, (t, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, pr.t14)))))))))))))),
                q ++= h.map(TI.inj(_: Id[A2])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(-\/(x))) => {
              val pr = prod.run
              val (h, t) = U(pr.t3)
              go(TG.Prod((pr.t1, (pr.t2, (t, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, pr.t14)))))))))))))),
                q ++= h.map(TI.inj(_: Id[A3])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(-\/(x)))) => {
              val pr = prod.run
              val (h, t) = U(pr.t4)
              go(TG.Prod((pr.t1, (pr.t2, (pr.t3, (t, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, pr.t14)))))))))))))),
                q ++= h.map(TI.inj(_: Id[A4])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(-\/(x))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t5)
              go(TG.Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (t, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, pr.t14)))))))))))))),
                q ++= h.map(TI.inj(_: Id[A5])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t6)
              go(TG.Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (t, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, pr.t14)))))))))))))),
                q ++= h.map(TI.inj(_: Id[A6])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t7)
              go(TG.Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (t, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, pr.t14)))))))))))))),
                q ++= h.map(TI.inj(_: Id[A7])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t8)
              go(TG.Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (t, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, pr.t14)))))))))))))),
                q ++= h.map(TI.inj(_: Id[A8])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t9)
              go(TG.Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (t, (pr.t10, (pr.t11, (pr.t12, (pr.t13, pr.t14)))))))))))))),
                q ++= h.map(TI.inj(_: Id[A9])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t10)
              go(TG.Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (t, (pr.t11, (pr.t12, (pr.t13, pr.t14)))))))))))))),
                q ++= h.map(TI.inj(_: Id[A10])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t11)
              go(TG.Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (t, (pr.t12, (pr.t13, pr.t14)))))))))))))),
                q ++= h.map(TI.inj(_: Id[A11])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t12)
              go(TG.Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (t, (pr.t13, pr.t14)))))))))))))),
                q ++= h.map(TI.inj(_: Id[A12])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t13)
              go(TG.Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (t, pr.t14)))))))))))))),
                q ++= h.map(TI.inj(_: Id[A13])), M.append(out, map(TI.inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t14)
              go(TG.Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, t)))))))))))))),
                q ++= h.map(TI.inj(_: Id[A14])), M.append(out, map(TI.inj(x))))
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

object AndXorK14 {

  def apply[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
    new AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}
}

trait AndXorF14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {
  type Repr[F[_]] = AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  def apply[F[_]]: Repr[F] =
    new AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}
}

object AndXorF14 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: AndXorF14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
    new AndXorF14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}
}

trait AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] extends AndXorK14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

object AndXor14 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
    new AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}
}
