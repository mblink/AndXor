package andxor

import andxor.types.{Cop22, Prod22, TCDeps22}
import scala.annotation.tailrec
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] extends AndXor {
  type Prod[F[_]] = Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
  object Prod {
    def apply[F[_]](
        p: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20], F[A21], F[A22])
    ): Prod[F] = Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](p)
  }

  type Cop[F[_]] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
  object Cop {
    def apply[F[_]](
        c: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[
          A19
        ] \/ (F[A20] \/ (F[A21] \/ F[A22])))))))))))))))))))))
    ): Cop[F] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](c)
  }

  type TCDeps[TC[_], F[_]] = TCDeps22[TC, F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

  def mkChoose[TC[_], F[_], B](f: B => Cop[F])(implicit d: Decidable[TC], tcs: TCDeps[TC, F]): TC[B] =
    Combine.choose22(
      tcs.a0,
      tcs.a1,
      tcs.a2,
      tcs.a3,
      tcs.a4,
      tcs.a5,
      tcs.a6,
      tcs.a7,
      tcs.a8,
      tcs.a9,
      tcs.a10,
      tcs.a11,
      tcs.a12,
      tcs.a13,
      tcs.a14,
      tcs.a15,
      tcs.a16,
      tcs.a17,
      tcs.a18,
      tcs.a19,
      tcs.a20,
      tcs.a21
    )(f(_).run)

  def mkAlt[TC[_], F[_], B](f: Cop[F] => B)(implicit a: Alt[TC], tcs: TCDeps[TC, F]): TC[B] =
    Combine.altly22(
      tcs.a0,
      tcs.a1,
      tcs.a2,
      tcs.a3,
      tcs.a4,
      tcs.a5,
      tcs.a6,
      tcs.a7,
      tcs.a8,
      tcs.a9,
      tcs.a10,
      tcs.a11,
      tcs.a12,
      tcs.a13,
      tcs.a14,
      tcs.a15,
      tcs.a16,
      tcs.a17,
      tcs.a18,
      tcs.a19,
      tcs.a20,
      tcs.a21
    )(x => f(Cop(x)))

  def mkDivide[TC[_], F[_], B](f: B => Prod[F])(implicit d: Divide[TC], tcs: TCDeps[TC, F]): TC[B] =
    Combine.divide22(
      tcs.a0,
      tcs.a1,
      tcs.a2,
      tcs.a3,
      tcs.a4,
      tcs.a5,
      tcs.a6,
      tcs.a7,
      tcs.a8,
      tcs.a9,
      tcs.a10,
      tcs.a11,
      tcs.a12,
      tcs.a13,
      tcs.a14,
      tcs.a15,
      tcs.a16,
      tcs.a17,
      tcs.a18,
      tcs.a19,
      tcs.a20,
      tcs.a21
    )(f(_).run)

  def mkApply[TC[_], F[_], B](f: Prod[F] => B)(implicit a: Apply[TC], tcs: TCDeps[TC, F]): TC[B] =
    Combine.apply22(
      tcs.a0,
      tcs.a1,
      tcs.a2,
      tcs.a3,
      tcs.a4,
      tcs.a5,
      tcs.a6,
      tcs.a7,
      tcs.a8,
      tcs.a9,
      tcs.a10,
      tcs.a11,
      tcs.a12,
      tcs.a13,
      tcs.a14,
      tcs.a15,
      tcs.a16,
      tcs.a17,
      tcs.a18,
      tcs.a19,
      tcs.a20,
      tcs.a21
    ) {
      case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21) =>
        f(Prod((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21)))
    }

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = choose[Inj[Cop[F], ?], F]
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = divide[Inj[Prod[F], ?], F]
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = choose[Inj[Prod[F], ?], F]
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = divide[Inj[Vector[Cop[F]], ?], F]
  }

  def transformP[F[_], G[_]](nt: (F ~> G)): Prod[F] => Prod[G] =
    (p: Prod[F]) =>
      Prod[G](
        (
          nt(p.t1),
          nt(p.t2),
          nt(p.t3),
          nt(p.t4),
          nt(p.t5),
          nt(p.t6),
          nt(p.t7),
          nt(p.t8),
          nt(p.t9),
          nt(p.t10),
          nt(p.t11),
          nt(p.t12),
          nt(p.t13),
          nt(p.t14),
          nt(p.t15),
          nt(p.t16),
          nt(p.t17),
          nt(p.t18),
          nt(p.t19),
          nt(p.t20),
          nt(p.t21),
          nt(p.t22)
        )
      )

  def transformC[F[_], G[_]](nt: (F ~> G)): Cop[F] => Cop[G] =
    (c: Cop[F]) =>
      Cop[G](
        c.run.bimap(
          nt(_),
          _.bimap(
            nt(_),
            _.bimap(
              nt(_),
              _.bimap(
                nt(_),
                _.bimap(
                  nt(_),
                  _.bimap(
                    nt(_),
                    _.bimap(
                      nt(_),
                      _.bimap(
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
                  )
                )
              )
            )
          )
        )
      )

  // format: off
  def sequenceP[F[_]](p: Prod[F])(implicit A: Apply[F]): F[Prod[Id]] =
    A.map(
    A.ap(p.t22)(
    A.ap(p.t21)(
    A.ap(p.t20)(
    A.ap(p.t19)(
    A.ap(p.t18)(
    A.ap(p.t17)(
    A.ap(p.t16)(
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
    A.map(p.t1)((i0: A1) => (i1: A2) => (i2: A3) => (i3: A4) => (i4: A5) => (i5: A6) => (i6: A7) => (i7: A8) => (i8: A9) => (i9: A10) => (i10: A11) => (i11: A12) => (i12: A13) => (i13: A14) => (i14: A15) => (i15: A16) => (i16: A17) => (i17: A18) => (i18: A19) => (i19: A20) => (i20: A21) => (i21: A22) =>
      (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21))))))))))))))))))))))))(Prod[Id](_))

  def sequenceC[F[_]](cop: Cop[F])(implicit FF: Functor[F]): F[Cop[Id]] =
    cop.run match {
      case -\/(x) => FF.map(x)(y => Cop[Id](-\/(y)))
      case \/-(-\/(x)) => FF.map(x)(y => Cop[Id](\/-(-\/(y))))
      case \/-(\/-(-\/(x))) => FF.map(x)(y => Cop[Id](\/-(\/-(-\/(y)))))
      case \/-(\/-(\/-(-\/(x)))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(-\/(y))))))
      case \/-(\/-(\/-(\/-(-\/(x))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(-\/(y)))))))
      case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(-\/(y))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y))))))))))))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(y)))))))))))))))))))))))
      case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))))))) => FF.map(x)(y => Cop[Id](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(y)))))))))))))))))))))))
    }

  def extractC[F[_], B](c: Cop[F])(implicit inj: Inj[Option[B], Cop[F]]): Option[B] = inj(c)

  def extractP[F[_], B](p: Prod[F])(implicit inj: Inj[B, Prod[F]]): B = inj(p)

  def foldMap[G[_], C](p: Prod[G])(map: Cop[Id] => C)(implicit O: Ordering[Cop[Id]], M: Monoid[C], PE: PlusEmpty[G], U: Uncons[G]): C = {
    import scala.collection.mutable.{PriorityQueue => PQ}

    def uncons(p: Prod[G]): (List[Cop[Id]], Prod[G]) = {
      val ht1 = U(p.t1)
      val ht2 = U(p.t2)
      val ht3 = U(p.t3)
      val ht4 = U(p.t4)
      val ht5 = U(p.t5)
      val ht6 = U(p.t6)
      val ht7 = U(p.t7)
      val ht8 = U(p.t8)
      val ht9 = U(p.t9)
      val ht10 = U(p.t10)
      val ht11 = U(p.t11)
      val ht12 = U(p.t12)
      val ht13 = U(p.t13)
      val ht14 = U(p.t14)
      val ht15 = U(p.t15)
      val ht16 = U(p.t16)
      val ht17 = U(p.t17)
      val ht18 = U(p.t18)
      val ht19 = U(p.t19)
      val ht20 = U(p.t20)
      val ht21 = U(p.t21)
      val ht22 = U(p.t22)
      (List(ht1._1.map(injId(_: A1)), ht2._1.map(injId(_: A2)), ht3._1.map(injId(_: A3)), ht4._1.map(injId(_: A4)), ht5._1.map(injId(_: A5)), ht6._1.map(injId(_: A6)), ht7._1.map(injId(_: A7)), ht8._1.map(injId(_: A8)), ht9._1.map(injId(_: A9)), ht10._1.map(injId(_: A10)), ht11._1.map(injId(_: A11)), ht12._1.map(injId(_: A12)), ht13._1.map(injId(_: A13)), ht14._1.map(injId(_: A14)), ht15._1.map(injId(_: A15)), ht16._1.map(injId(_: A16)), ht17._1.map(injId(_: A17)), ht18._1.map(injId(_: A18)), ht19._1.map(injId(_: A19)), ht20._1.map(injId(_: A20)), ht21._1.map(injId(_: A21)), ht22._1.map(injId(_: A22))).flatten,
        Prod[G]((ht1._2, ht2._2, ht3._2, ht4._2, ht5._2, ht6._2, ht7._2, ht8._2, ht9._2, ht10._2, ht11._2, ht12._2, ht13._2, ht14._2, ht15._2, ht16._2, ht17._2, ht18._2, ht19._2, ht20._2, ht21._2, ht22._2)))
    }

    @tailrec
    def appendAll(out: C, q: PQ[Cop[Id]]): C =
      q.isEmpty match {
        case true => out
        case false =>
          val newOut = M.append(out, map(q.dequeue))
          appendAll(newOut, q)
      }

    @tailrec
    def go(prod: Prod[G], q: PQ[Cop[Id]], out: C): C =
      (prod.run.==((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8], PE.empty[A9], PE.empty[A10], PE.empty[A11], PE.empty[A12], PE.empty[A13], PE.empty[A14], PE.empty[A15], PE.empty[A16], PE.empty[A17], PE.empty[A18], PE.empty[A19], PE.empty[A20], PE.empty[A21], PE.empty[A22]))) match {
        case true => appendAll(out, q)
        case false => q.isEmpty match {
          case true => {
            val (hs, ts) = uncons(prod)
            q ++= hs
            go(ts, q, out)
          }
          case false => q.dequeue.run match {
            case dj @ -\/(_) =>
              val (h, t) = U(prod.t1)
              go(Prod[G]((t, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A1)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(-\/(_)) =>
              val (h, t) = U(prod.t2)
              go(Prod[G]((prod.t1, t, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A2)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(-\/(_))) =>
              val (h, t) = U(prod.t3)
              go(Prod[G]((prod.t1, prod.t2, t, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A3)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(-\/(_)))) =>
              val (h, t) = U(prod.t4)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, t, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A4)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(-\/(_))))) =>
              val (h, t) = U(prod.t5)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, t, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A5)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(-\/(_)))))) =>
              val (h, t) = U(prod.t6)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, t, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A6)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))) =>
              val (h, t) = U(prod.t7)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, t, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A7)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))) =>
              val (h, t) = U(prod.t8)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, t, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A8)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))) =>
              val (h, t) = U(prod.t9)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, t, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A9)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))) =>
              val (h, t) = U(prod.t10)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, t, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A10)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))) =>
              val (h, t) = U(prod.t11)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, t, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A11)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))))) =>
              val (h, t) = U(prod.t12)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, t, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A12)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))))) =>
              val (h, t) = U(prod.t13)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, t, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A13)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))))))) =>
              val (h, t) = U(prod.t14)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, t, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A14)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))))))) =>
              val (h, t) = U(prod.t15)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, t, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A15)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))))))))) =>
              val (h, t) = U(prod.t16)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, t, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A16)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))))))))) =>
              val (h, t) = U(prod.t17)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, t, prod.t18, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A17)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))))))))))) =>
              val (h, t) = U(prod.t18)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, t, prod.t19, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A18)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))))))))))) =>
              val (h, t) = U(prod.t19)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, t, prod.t20, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A19)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))))))))))))) =>
              val (h, t) = U(prod.t20)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, t, prod.t21, prod.t22)),
                q ++= h.map(injId(_: A20)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))))))))))))) =>
              val (h, t) = U(prod.t21)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, t, prod.t22)),
                q ++= h.map(injId(_: A21)), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(_))))))))))))))))))))) =>
              val (h, t) = U(prod.t22)
              go(Prod[G]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21, t)),
                q ++= h.map(injId(_: A22)), M.append(out, map(Cop[Id](dj))))

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

object AndXor22 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
      : AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
    new AndXor22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {}
}
