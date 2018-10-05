package andxor
import scala.language.higherKinds
import scalaz.{Apply, Functor, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.syntax.either._

trait AndXorK14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] extends AndXor {
  type Prod = (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14])
  type Cop = (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14])))))))))))))
  val AndXorF = AndXorF14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  type AndXor[G[_]] = AndXorF14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Repr[G]
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
        Combine.choose14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(
          (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13) => f((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13))
        )
    }

  object instances {

    implicit val inja0: Inj[Cop, F[A1]] =
      Inj.instance(_.left[(F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14]))))))))))))])

    implicit val inja1: Inj[Cop, F[A2]] =
      Inj.instance(_.left[(F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14])))))))))))].right[F[A1]])

    implicit val inja2: Inj[Cop, F[A3]] =
      Inj.instance(_.left[(F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14]))))))))))].right[F[A2]].right[F[A1]])

    implicit val inja3: Inj[Cop, F[A4]] =
      Inj.instance(_.left[(F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14])))))))))].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja4: Inj[Cop, F[A5]] =
      Inj.instance(_.left[(F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14]))))))))].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja5: Inj[Cop, F[A6]] =
      Inj.instance(_.left[(F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14])))))))].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja6: Inj[Cop, F[A7]] =
      Inj.instance(_.left[(F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14]))))))].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja7: Inj[Cop, F[A8]] =
      Inj.instance(_.left[(F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14])))))].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja8: Inj[Cop, F[A9]] =
      Inj.instance(_.left[(F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14]))))].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja9: Inj[Cop, F[A10]] =
      Inj.instance(_.left[(F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14])))].right[F[A9]].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja10: Inj[Cop, F[A11]] =
      Inj.instance(_.left[(F[A12] \/ (F[A13] \/ F[A14]))].right[F[A10]].right[F[A9]].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja11: Inj[Cop, F[A12]] =
      Inj.instance(_.left[(F[A13] \/ F[A14])].right[F[A11]].right[F[A10]].right[F[A9]].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja12: Inj[Cop, F[A13]] =
      Inj.instance(_.left[F[A14]].right[F[A12]].right[F[A11]].right[F[A10]].right[F[A9]].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit val inja13: Inj[Cop, F[A14]] =
      Inj.instance(_.right[F[A13]].right[F[A12]].right[F[A11]].right[F[A10]].right[F[A9]].right[F[A8]].right[F[A7]].right[F[A6]].right[F[A5]].right[F[A4]].right[F[A3]].right[F[A2]].right[F[A1]])

    implicit def lifta0(implicit M: Monoid[Prod]): Inj[Prod, F[A1]] = {
      val (_, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((_, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, F[A2]] = {
      val (a0, _, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, _, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, F[A3]] = {
      val (a0, a1, _, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, _, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, F[A4]] = {
      val (a0, a1, a2, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, _, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, F[A5]] = {
      val (a0, a1, a2, a3, _, a4, a5, a6, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, _, a4, a5, a6, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, F[A6]] = {
      val (a0, a1, a2, a3, a4, _, a5, a6, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, _, a5, a6, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta6(implicit M: Monoid[Prod]): Inj[Prod, F[A7]] = {
      val (a0, a1, a2, a3, a4, a5, _, a6, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, _, a6, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta7(implicit M: Monoid[Prod]): Inj[Prod, F[A8]] = {
      val (a0, a1, a2, a3, a4, a5, a6, _, a7, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, _, a7, a8, a9, a10, a11, a12))
    }

    implicit def lifta8(implicit M: Monoid[Prod]): Inj[Prod, F[A9]] = {
      val (a0, a1, a2, a3, a4, a5, a6, a7, _, a8, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, _, a8, a9, a10, a11, a12))
    }

    implicit def lifta9(implicit M: Monoid[Prod]): Inj[Prod, F[A10]] = {
      val (a0, a1, a2, a3, a4, a5, a6, a7, a8, _, a9, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, a8, _, a9, a10, a11, a12))
    }

    implicit def lifta10(implicit M: Monoid[Prod]): Inj[Prod, F[A11]] = {
      val (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a10, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, _, a10, a11, a12))
    }

    implicit def lifta11(implicit M: Monoid[Prod]): Inj[Prod, F[A12]] = {
      val (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a11, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _, a11, a12))
    }

    implicit def lifta12(implicit M: Monoid[Prod]): Inj[Prod, F[A13]] = {
      val (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a12) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _, a12))
    }

    implicit def lifta13(implicit M: Monoid[Prod]): Inj[Prod, F[A14]] = {
      val (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _))
    }

  }

  import instances._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Prod => AndXorK14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Prod =
    (p: AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Prod) =>
      (nt(p._1), nt(p._2), nt(p._3), nt(p._4), nt(p._5), nt(p._6), nt(p._7), nt(p._8), nt(p._9), nt(p._10), nt(p._11), nt(p._12), nt(p._13), nt(p._14))

  def transformC[G[_]](nt: (F ~> G)): AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Cop => AndXorK14[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Cop =
    (p: AndXorK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Cop) =>
      p.bimap(
        nt(_),
        _.bimap(
          nt(_),
          _.bimap(
            nt(_),
            _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))))))))
          )
        )
      )

  // format: off
  def sequenceP(prod: Prod)(A: Apply[F]): F[AndXorK14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Prod] = {
    val (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = prod
    A.ap(a13)(
    A.ap(a12)(
    A.ap(a11)(
    A.ap(a10)(
    A.ap(a9)(
    A.ap(a8)(
    A.ap(a7)(
    A.ap(a6)(
    A.ap(a5)(
    A.ap(a4)(
    A.ap(a3)(
    A.ap(a2)(
    A.ap(a1)(
     A.map(a0)(((i0: A1, i1: A2, i2: A3, i3: A4, i4: A5, i5: A6, i6: A7, i7: A8, i8: A9, i9: A10, i10: A11, i11: A12, i12: A13, i13: A14) =>
    (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13)).curried))))))))))))))
  }
  // format: on

  def foldMap[C](p: AndXor[List]#Prod)(map: AndXor[Id]#Cop => C)(implicit O: Ordering[AndXorK14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]#Cop], M: Monoid[C]): C = {
    val TL = AndXorF[List]
    val TI = AndXorF[Id]
    import scala.collection.mutable.{PriorityQueue => PQ}
    import TI.instances._
    def uncons(p: TL.Prod): (List[TI.Cop], TL.Prod) =
      (
        List(
          p._1.headOption.map(TI.inj(_: A1)),
          p._2.headOption.map(TI.inj(_: A2)),
          p._3.headOption.map(TI.inj(_: A3)),
          p._4.headOption.map(TI.inj(_: A4)),
          p._5.headOption.map(TI.inj(_: A5)),
          p._6.headOption.map(TI.inj(_: A6)),
          p._7.headOption.map(TI.inj(_: A7)),
          p._8.headOption.map(TI.inj(_: A8)),
          p._9.headOption.map(TI.inj(_: A9)),
          p._10.headOption.map(TI.inj(_: A10)),
          p._11.headOption.map(TI.inj(_: A11)),
          p._12.headOption.map(TI.inj(_: A12)),
          p._13.headOption.map(TI.inj(_: A13)),
          p._14.headOption.map(TI.inj(_: A14))
        ).flatten,
        (
          p._1.headOption.map(_ => p._1.tail).getOrElse(p._1),
          p._2.headOption.map(_ => p._2.tail).getOrElse(p._2),
          p._3.headOption.map(_ => p._3.tail).getOrElse(p._3),
          p._4.headOption.map(_ => p._4.tail).getOrElse(p._4),
          p._5.headOption.map(_ => p._5.tail).getOrElse(p._5),
          p._6.headOption.map(_ => p._6.tail).getOrElse(p._6),
          p._7.headOption.map(_ => p._7.tail).getOrElse(p._7),
          p._8.headOption.map(_ => p._8.tail).getOrElse(p._8),
          p._9.headOption.map(_ => p._9.tail).getOrElse(p._9),
          p._10.headOption.map(_ => p._10.tail).getOrElse(p._10),
          p._11.headOption.map(_ => p._11.tail).getOrElse(p._11),
          p._12.headOption.map(_ => p._12.tail).getOrElse(p._12),
          p._13.headOption.map(_ => p._13.tail).getOrElse(p._13),
          p._14.headOption.map(_ => p._14.tail).getOrElse(p._14)
        )
      )
    @scala.annotation.tailrec
    def go(prod: TL.Prod, q: PQ[TI.Cop], out: C): C =
      prod match {
        case (Nil, Nil, Nil, Nil, Nil, Nil, Nil, Nil, Nil, Nil, Nil, Nil, Nil, Nil) =>
          q.foldLeft(out)((acc, el) => M.append(acc, map(el)))
        case (as0, as1, as2, as3, as4, as5, as6, as7, as8, as9, as10, as11, as12, as13) =>
          q.isEmpty match {
            case true => {
              val (hs, ts) = uncons(prod)
              q ++= hs
              go(ts, q, out)
            }
            case false =>
              q.dequeue match {
                case -\/(x) =>
                  go((as0.tail, as1, as2, as3, as4, as5, as6, as7, as8, as9, as10, as11, as12, as13), q ++= as0.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(-\/(x)) =>
                  go((as0, as1.tail, as2, as3, as4, as5, as6, as7, as8, as9, as10, as11, as12, as13), q ++= as1.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(\/-(-\/(x))) =>
                  go((as0, as1, as2.tail, as3, as4, as5, as6, as7, as8, as9, as10, as11, as12, as13), q ++= as2.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(\/-(\/-(-\/(x)))) =>
                  go((as0, as1, as2, as3.tail, as4, as5, as6, as7, as8, as9, as10, as11, as12, as13), q ++= as3.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(\/-(\/-(\/-(-\/(x))))) =>
                  go((as0, as1, as2, as3, as4.tail, as5, as6, as7, as8, as9, as10, as11, as12, as13), q ++= as4.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) =>
                  go((as0, as1, as2, as3, as4, as5.tail, as6, as7, as8, as9, as10, as11, as12, as13), q ++= as5.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) =>
                  go((as0, as1, as2, as3, as4, as5, as6.tail, as7, as8, as9, as10, as11, as12, as13), q ++= as6.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) =>
                  go((as0, as1, as2, as3, as4, as5, as6, as7.tail, as8, as9, as10, as11, as12, as13), q ++= as7.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) =>
                  go((as0, as1, as2, as3, as4, as5, as6, as7, as8.tail, as9, as10, as11, as12, as13), q ++= as8.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) =>
                  go((as0, as1, as2, as3, as4, as5, as6, as7, as8, as9.tail, as10, as11, as12, as13), q ++= as9.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) =>
                  go((as0, as1, as2, as3, as4, as5, as6, as7, as8, as9, as10.tail, as11, as12, as13), q ++= as10.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) =>
                  go((as0, as1, as2, as3, as4, as5, as6, as7, as8, as9, as10, as11.tail, as12, as13), q ++= as11.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) =>
                  go((as0, as1, as2, as3, as4, as5, as6, as7, as8, as9, as10, as11, as12.tail, as13), q ++= as12.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
                case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))) =>
                  go((as0, as1, as2, as3, as4, as5, as6, as7, as8, as9, as10, as11, as12, as13.tail), q ++= as13.headOption.map(TI.inj(_)), M.append(out, map(TI.inj(x))))

              }
          }
      }
    val Q = new scala.collection.mutable.PriorityQueue[TI.Cop]()
    val (hs, ts) = uncons(p)
    Q ++= hs
    go(ts, Q, M.zero)
  }
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
