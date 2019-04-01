package andxor

import andxor.tuple._
import andxor.types._
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id

trait AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] extends AndXor {
  type Prod[F[_]] = Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
  object Prod { def apply[F[_]](p: (F[A1], (F[A2], (F[A3], (F[A4], (F[A5], (F[A6], (F[A7], (F[A8], (F[A9], (F[A10], (F[A11], (F[A12], (F[A13], (F[A14], F[A15]))))))))))))))): Prod[F] = Prod15(p) }

  type Cop[F[_]] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
  object Cop {
    def apply[F[_]](c: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ F[A15]))))))))))))))): Cop[F] =
      Cop15(c)
  }

  def combine[F[_], G[_]](
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
      a13: G[F[A14]],
      a14: G[F[A15]]
  ): ComposeAndXor[F, G, Cop, Prod] =
    new ComposeAndXor[F, G, Cop, Prod] {
      def mkChoose[B](f: B => Cop[F])(implicit d: Decidable[G]): G[B] =
        Combine.choose15(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(f(_).run)

      def mkAlt[B](f: Cop[F] => B)(implicit a: Alt[G]): G[B] =
        Combine.altly15(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(x => f(Cop(x)))

      def mkDivide[B](f: B => Prod[F])(implicit d: Divide[G]): G[B] =
        Combine.divide15(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(f(_).run)

      def mkApply[B](f: Prod[F] => B)(implicit a: Apply[G]): G[B] =
        Combine.apply15(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) {
          case (i0, (i1, (i2, (i3, (i4, (i5, (i6, (i7, (i8, (i9, (i10, (i11, (i12, (i13, i14)))))))))))))) =>
            f(Prod((i0, (i1, (i2, (i3, (i4, (i5, (i6, (i7, (i8, (i9, (i10, (i11, (i12, (i13, i14))))))))))))))))
        }
    }

  def injEv[F[_]] = combine[F, Inj.Aux[Cop[F]]#Out].choose
  def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = combine[F, Inj.Aux[Prod[F]]#Out].divide

  def transformP[F[_], G[_]](
      nt: (F ~> G)
  ): AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]#Prod[F] => AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]#Prod[G] =
    (p: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]#Prod[F]) => {
      val pr = p.run
      Prod[G](
        (
          nt(pr.t1),
          (nt(pr.t2), (nt(pr.t3), (nt(pr.t4), (nt(pr.t5), (nt(pr.t6), (nt(pr.t7), (nt(pr.t8), (nt(pr.t9), (nt(pr.t10), (nt(pr.t11), (nt(pr.t12), (nt(pr.t13), (nt(pr.t14), nt(pr.t15))))))))))))))
        )
      )
    }

  def transformC[F[_], G[_]](
      nt: (F ~> G)
  ): AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]#Cop[F] => AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]#Cop[G] =
    (p: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]#Cop[F]) =>
      Cop[G](
        p.run.bimap(
          nt(_),
          _.bimap(
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
      )

  def subst1[B]: AndXor15[B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = AndXor15[B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

  def subst2[B]: AndXor15[A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = AndXor15[A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

  def subst3[B]: AndXor15[A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = AndXor15[A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

  def subst4[B]: AndXor15[A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = AndXor15[A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

  def subst5[B]: AndXor15[A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = AndXor15[A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

  def subst6[B]: AndXor15[A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15] = AndXor15[A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15]

  def subst7[B]: AndXor15[A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15] = AndXor15[A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15]

  def subst8[B]: AndXor15[A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15] = AndXor15[A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15]

  def subst9[B]: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15] = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15]

  def subst10[B]: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15] = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15]

  def subst11[B]: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15] = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15]

  def subst12[B]: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15] = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15]

  def subst13[B]: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15] = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15]

  def subst14[B]: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15] = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15]

  def subst15[B]: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B] = AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B]

  // format: off
  def sequenceP[F[_]](prod: Prod[F])(implicit A: Apply[F]): F[Prod[Id]] = {
    val p = prod.run
    A.map(
    A.ap(p.t15)(
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
    A.map(p.t1)((i0: A1) => (i1: A2) => (i2: A3) => (i3: A4) => (i4: A5) => (i5: A6) => (i6: A7) => (i7: A8) => (i8: A9) => (i9: A10) => (i10: A11) => (i11: A12) => (i12: A13) => (i13: A14) => (i14: A15) =>
      (i0, (i1, (i2, (i3, (i4, (i5, (i6, (i7, (i8, (i9, (i10, (i11, (i12, (i13, i14))))))))))))))))))))))))))))))(Prod[Id](_))
  }

  def sequenceC[F[_]](cop: Cop[F])(implicit FF: Functor[F]): F[Cop[Id]] =
    cop.run match {
      case -\/(x) => FF.map(FF.map(x)(y => -\/(y)))(Cop[Id](_))
      case \/-(-\/(x)) => FF.map(FF.map(x)(y => \/-(-\/(y))))(Cop[Id](_))
      case \/-(\/-(-\/(x))) => FF.map(FF.map(x)(y => \/-(\/-(-\/(y)))))(Cop[Id](_))
      case \/-(\/-(\/-(-\/(x)))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(-\/(y))))))(Cop[Id](_))
      case \/-(\/-(\/-(\/-(-\/(x))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(-\/(y)))))))(Cop[Id](_))
      case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(-\/(y))))))))(Cop[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))(Cop[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))(Cop[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))(Cop[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))(Cop[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))(Cop[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))(Cop[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))(Cop[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))(Cop[Id](_))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))) => FF.map(FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y))))))))))))))))(Cop[Id](_))
    }

  def extractC[F[_], B](c: Cop[F])(implicit inj: Inj[Option[B], Cop[F]]): Option[B] = inj(c)

  def extractP[F[_], B](p: Prod[F])(implicit inj: Inj[B, Prod[F]]): B = inj(p)

  def foldMap[F[_], C](p: Prod[F])(map: Cop[F] => C)(implicit M: Monoid[C]): C = {
    val pr = p.run
    M.append(map(inj(pr.t1)), M.append(map(inj(pr.t2)), M.append(map(inj(pr.t3)), M.append(map(inj(pr.t4)), M.append(map(inj(pr.t5)), M.append(map(inj(pr.t6)), M.append(map(inj(pr.t7)), M.append(map(inj(pr.t8)), M.append(map(inj(pr.t9)), M.append(map(inj(pr.t10)), M.append(map(inj(pr.t11)), M.append(map(inj(pr.t12)), M.append(map(inj(pr.t13)), M.append(map(inj(pr.t14)), map(inj(pr.t15))))))))))))))))
  }

  def foldMapId[F[_], C](p: Prod[F])(map: Cop[Id] => C)(
      implicit O: Ordering[Cop[Id]], M: Monoid[C], PE: PlusEmpty[F], U: Uncons[F]): C = {
    import scala.collection.mutable.{PriorityQueue => PQ}
    def uncons(p: Prod[F]): (List[Cop[Id]], Prod[F]) = {
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
      val ht15 = U(pr.t15)
      (List(ht1._1.map(inj(_: Id[A1])), ht2._1.map(inj(_: Id[A2])), ht3._1.map(inj(_: Id[A3])), ht4._1.map(inj(_: Id[A4])), ht5._1.map(inj(_: Id[A5])), ht6._1.map(inj(_: Id[A6])), ht7._1.map(inj(_: Id[A7])), ht8._1.map(inj(_: Id[A8])), ht9._1.map(inj(_: Id[A9])), ht10._1.map(inj(_: Id[A10])), ht11._1.map(inj(_: Id[A11])), ht12._1.map(inj(_: Id[A12])), ht13._1.map(inj(_: Id[A13])), ht14._1.map(inj(_: Id[A14])), ht15._1.map(inj(_: Id[A15]))).flatten,
        Prod[F]((ht1._2, (ht2._2, (ht3._2, (ht4._2, (ht5._2, (ht6._2, (ht7._2, (ht8._2, (ht9._2, (ht10._2, (ht11._2, (ht12._2, (ht13._2, (ht14._2, ht15._2))))))))))))))))
    }
    @scala.annotation.tailrec
    def go(prod: Prod[F], q: PQ[Cop[Id]], out: C): C =
      (prod.run.==((PE.empty[A1], (PE.empty[A2], (PE.empty[A3], (PE.empty[A4], (PE.empty[A5], (PE.empty[A6], (PE.empty[A7], (PE.empty[A8], (PE.empty[A9], (PE.empty[A10], (PE.empty[A11], (PE.empty[A12], (PE.empty[A13], (PE.empty[A14], PE.empty[A15])))))))))))))))) match {
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
              go(Prod((t, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, pr.t15))))))))))))))),
                q ++= h.map(inj(_: Id[A1])), M.append(out, map(inj(x))))
          }
          case \/-(-\/(x)) => {
              val pr = prod.run
              val (h, t) = U(pr.t2)
              go(Prod((pr.t1, (t, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, pr.t15))))))))))))))),
                q ++= h.map(inj(_: Id[A2])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(-\/(x))) => {
              val pr = prod.run
              val (h, t) = U(pr.t3)
              go(Prod((pr.t1, (pr.t2, (t, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, pr.t15))))))))))))))),
                q ++= h.map(inj(_: Id[A3])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(-\/(x)))) => {
              val pr = prod.run
              val (h, t) = U(pr.t4)
              go(Prod((pr.t1, (pr.t2, (pr.t3, (t, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, pr.t15))))))))))))))),
                q ++= h.map(inj(_: Id[A4])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(-\/(x))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t5)
              go(Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (t, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, pr.t15))))))))))))))),
                q ++= h.map(inj(_: Id[A5])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t6)
              go(Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (t, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, pr.t15))))))))))))))),
                q ++= h.map(inj(_: Id[A6])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t7)
              go(Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (t, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, pr.t15))))))))))))))),
                q ++= h.map(inj(_: Id[A7])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t8)
              go(Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (t, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, pr.t15))))))))))))))),
                q ++= h.map(inj(_: Id[A8])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t9)
              go(Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (t, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, pr.t15))))))))))))))),
                q ++= h.map(inj(_: Id[A9])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t10)
              go(Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (t, (pr.t11, (pr.t12, (pr.t13, (pr.t14, pr.t15))))))))))))))),
                q ++= h.map(inj(_: Id[A10])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t11)
              go(Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (t, (pr.t12, (pr.t13, (pr.t14, pr.t15))))))))))))))),
                q ++= h.map(inj(_: Id[A11])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t12)
              go(Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (t, (pr.t13, (pr.t14, pr.t15))))))))))))))),
                q ++= h.map(inj(_: Id[A12])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t13)
              go(Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (t, (pr.t14, pr.t15))))))))))))))),
                q ++= h.map(inj(_: Id[A13])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t14)
              go(Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (t, pr.t15))))))))))))))),
                q ++= h.map(inj(_: Id[A14])), M.append(out, map(inj(x))))
          }
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))) => {
              val pr = prod.run
              val (h, t) = U(pr.t15)
              go(Prod((pr.t1, (pr.t2, (pr.t3, (pr.t4, (pr.t5, (pr.t6, (pr.t7, (pr.t8, (pr.t9, (pr.t10, (pr.t11, (pr.t12, (pr.t13, (pr.t14, t))))))))))))))),
                q ++= h.map(inj(_: Id[A15])), M.append(out, map(inj(x))))
          }

          }
        }
      }
    val Q = new PQ[Cop[Id]]()(O)
    val (hs, ts) = uncons(p)
    Q ++= hs
    go(ts, Q, M.zero)
  }
  // format: on
}

object AndXor15 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
    new AndXor15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {}
}
