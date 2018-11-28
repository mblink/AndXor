package andxor
import andxor.MapN.syntax._
import scala.language.higherKinds
import scalaz.{Apply, Foldable, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.Isomorphism.{<=>, IsoSet}
import scalaz.std.list._
import scalaz.syntax.either._

trait AndXorK21[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] extends AndXor {
  type Prod = (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20], F[A21])
  type Cop = (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[
    A19
  ] \/ (F[A20] \/ F[A21]))))))))))))))))))))
  val AndXorF = AndXorF21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
  type AndXor[G[_]] = AndXorF21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]#Repr[G]
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
      a13: G[F[A14]],
      a14: G[F[A15]],
      a15: G[F[A16]],
      a16: G[F[A17]],
      a17: G[F[A18]],
      a18: G[F[A19]],
      a19: G[F[A20]],
      a20: G[F[A21]]
  ): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose21(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly21(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide21(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply21(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(
          (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20) =>
            f((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20))
        )
    }

  object instances {

    implicit val inja0: Inj[Cop, F[A1]] =
      Inj.instance(
        _.left[(F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[
          A19
        ] \/ (F[A20] \/ F[A21])))))))))))))))))))]
      )

    implicit val inja0Inverse: Inj[Option[F[A1]], Cop] =
      Inj.instance(_ match {
        case -\/(x) => Some(x)
        case _      => None
      })

    implicit val inja1: Inj[Cop, F[A2]] =
      Inj.instance(
        _.left[(F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[
          A20
        ] \/ F[A21]))))))))))))))))))].right[F[A1]]
      )

    implicit val inja1Inverse: Inj[Option[F[A2]], Cop] =
      Inj.instance(_ match {
        case \/-(-\/(x)) => Some(x)
        case _           => None
      })

    implicit val inja2: Inj[Cop, F[A3]] =
      Inj.instance(
        _.left[(F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ F[
          A21
        ])))))))))))))))))].right[F[A2]].right[F[A1]]
      )

    implicit val inja2Inverse: Inj[Option[F[A3]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _                => None
      })

    implicit val inja3: Inj[Cop, F[A4]] =
      Inj.instance(
        _.left[(F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ F[
          A21
        ]))))))))))))))))].right[F[A3]].right[F[A2]].right[F[A1]]
      )

    implicit val inja3Inverse: Inj[Option[F[A4]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _                     => None
      })

    implicit val inja4: Inj[Cop, F[A5]] =
      Inj.instance(
        _.left[(F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ F[A21])))))))))))))))]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja4Inverse: Inj[Option[F[A5]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _                          => None
      })

    implicit val inja5: Inj[Cop, F[A6]] =
      Inj.instance(
        _.left[(F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ F[A21]))))))))))))))]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja5Inverse: Inj[Option[F[A6]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _                               => None
      })

    implicit val inja6: Inj[Cop, F[A7]] =
      Inj.instance(
        _.left[(F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ F[A21])))))))))))))]
          .right[F[A6]]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja6Inverse: Inj[Option[F[A7]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _                                    => None
      })

    implicit val inja7: Inj[Cop, F[A8]] =
      Inj.instance(
        _.left[(F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ F[A21]))))))))))))]
          .right[F[A7]]
          .right[F[A6]]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja7Inverse: Inj[Option[F[A8]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _                                         => None
      })

    implicit val inja8: Inj[Cop, F[A9]] =
      Inj.instance(
        _.left[(F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ F[A21])))))))))))]
          .right[F[A8]]
          .right[F[A7]]
          .right[F[A6]]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja8Inverse: Inj[Option[F[A9]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _                                              => None
      })

    implicit val inja9: Inj[Cop, F[A10]] =
      Inj.instance(
        _.left[(F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ F[A21]))))))))))]
          .right[F[A9]]
          .right[F[A8]]
          .right[F[A7]]
          .right[F[A6]]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja9Inverse: Inj[Option[F[A10]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
        case _                                                   => None
      })

    implicit val inja10: Inj[Cop, F[A11]] =
      Inj.instance(
        _.left[(F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ F[A21])))))))))]
          .right[F[A10]]
          .right[F[A9]]
          .right[F[A8]]
          .right[F[A7]]
          .right[F[A6]]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja10Inverse: Inj[Option[F[A11]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
        case _                                                        => None
      })

    implicit val inja11: Inj[Cop, F[A12]] =
      Inj.instance(
        _.left[(F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ F[A21]))))))))]
          .right[F[A11]]
          .right[F[A10]]
          .right[F[A9]]
          .right[F[A8]]
          .right[F[A7]]
          .right[F[A6]]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja11Inverse: Inj[Option[F[A12]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
        case _                                                             => None
      })

    implicit val inja12: Inj[Cop, F[A13]] =
      Inj.instance(
        _.left[(F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ F[A21])))))))]
          .right[F[A12]]
          .right[F[A11]]
          .right[F[A10]]
          .right[F[A9]]
          .right[F[A8]]
          .right[F[A7]]
          .right[F[A6]]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja12Inverse: Inj[Option[F[A13]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
        case _                                                                  => None
      })

    implicit val inja13: Inj[Cop, F[A14]] =
      Inj.instance(
        _.left[(F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ F[A21]))))))]
          .right[F[A13]]
          .right[F[A12]]
          .right[F[A11]]
          .right[F[A10]]
          .right[F[A9]]
          .right[F[A8]]
          .right[F[A7]]
          .right[F[A6]]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja13Inverse: Inj[Option[F[A14]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
        case _                                                                       => None
      })

    implicit val inja14: Inj[Cop, F[A15]] =
      Inj.instance(
        _.left[(F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ F[A21])))))]
          .right[F[A14]]
          .right[F[A13]]
          .right[F[A12]]
          .right[F[A11]]
          .right[F[A10]]
          .right[F[A9]]
          .right[F[A8]]
          .right[F[A7]]
          .right[F[A6]]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja14Inverse: Inj[Option[F[A15]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
        case _                                                                            => None
      })

    implicit val inja15: Inj[Cop, F[A16]] =
      Inj.instance(
        _.left[(F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ F[A21]))))]
          .right[F[A15]]
          .right[F[A14]]
          .right[F[A13]]
          .right[F[A12]]
          .right[F[A11]]
          .right[F[A10]]
          .right[F[A9]]
          .right[F[A8]]
          .right[F[A7]]
          .right[F[A6]]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja15Inverse: Inj[Option[F[A16]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Some(x)
        case _                                                                                 => None
      })

    implicit val inja16: Inj[Cop, F[A17]] =
      Inj.instance(
        _.left[(F[A18] \/ (F[A19] \/ (F[A20] \/ F[A21])))]
          .right[F[A16]]
          .right[F[A15]]
          .right[F[A14]]
          .right[F[A13]]
          .right[F[A12]]
          .right[F[A11]]
          .right[F[A10]]
          .right[F[A9]]
          .right[F[A8]]
          .right[F[A7]]
          .right[F[A6]]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja16Inverse: Inj[Option[F[A17]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => Some(x)
        case _                                                                                      => None
      })

    implicit val inja17: Inj[Cop, F[A18]] =
      Inj.instance(
        _.left[(F[A19] \/ (F[A20] \/ F[A21]))]
          .right[F[A17]]
          .right[F[A16]]
          .right[F[A15]]
          .right[F[A14]]
          .right[F[A13]]
          .right[F[A12]]
          .right[F[A11]]
          .right[F[A10]]
          .right[F[A9]]
          .right[F[A8]]
          .right[F[A7]]
          .right[F[A6]]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja17Inverse: Inj[Option[F[A18]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) => Some(x)
        case _                                                                                           => None
      })

    implicit val inja18: Inj[Cop, F[A19]] =
      Inj.instance(
        _.left[(F[A20] \/ F[A21])]
          .right[F[A18]]
          .right[F[A17]]
          .right[F[A16]]
          .right[F[A15]]
          .right[F[A14]]
          .right[F[A13]]
          .right[F[A12]]
          .right[F[A11]]
          .right[F[A10]]
          .right[F[A9]]
          .right[F[A8]]
          .right[F[A7]]
          .right[F[A6]]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja18Inverse: Inj[Option[F[A19]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))) => Some(x)
        case _                                                                                                => None
      })

    implicit val inja19: Inj[Cop, F[A20]] =
      Inj.instance(
        _.left[F[A21]]
          .right[F[A19]]
          .right[F[A18]]
          .right[F[A17]]
          .right[F[A16]]
          .right[F[A15]]
          .right[F[A14]]
          .right[F[A13]]
          .right[F[A12]]
          .right[F[A11]]
          .right[F[A10]]
          .right[F[A9]]
          .right[F[A8]]
          .right[F[A7]]
          .right[F[A6]]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja19Inverse: Inj[Option[F[A20]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))) => Some(x)
        case _                                                                                                     => None
      })

    implicit val inja20: Inj[Cop, F[A21]] =
      Inj.instance(
        _.right[F[A20]]
          .right[F[A19]]
          .right[F[A18]]
          .right[F[A17]]
          .right[F[A16]]
          .right[F[A15]]
          .right[F[A14]]
          .right[F[A13]]
          .right[F[A12]]
          .right[F[A11]]
          .right[F[A10]]
          .right[F[A9]]
          .right[F[A8]]
          .right[F[A7]]
          .right[F[A6]]
          .right[F[A5]]
          .right[F[A4]]
          .right[F[A3]]
          .right[F[A2]]
          .right[F[A1]]
      )

    implicit val inja20Inverse: Inj[Option[F[A21]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))))) => Some(x)
        case _                                                                                                     => None
      })

    implicit def liftisoa0(implicit M: Monoid[Prod]): Prod <=> F[A1] =
      IsoSet(_._1, x => M.zero.map1(_ => x))

    implicit def liftisoa1(implicit M: Monoid[Prod]): Prod <=> F[A2] =
      IsoSet(_._2, x => M.zero.map2(_ => x))

    implicit def liftisoa2(implicit M: Monoid[Prod]): Prod <=> F[A3] =
      IsoSet(_._3, x => M.zero.map3(_ => x))

    implicit def liftisoa3(implicit M: Monoid[Prod]): Prod <=> F[A4] =
      IsoSet(_._4, x => M.zero.map4(_ => x))

    implicit def liftisoa4(implicit M: Monoid[Prod]): Prod <=> F[A5] =
      IsoSet(_._5, x => M.zero.map5(_ => x))

    implicit def liftisoa5(implicit M: Monoid[Prod]): Prod <=> F[A6] =
      IsoSet(_._6, x => M.zero.map6(_ => x))

    implicit def liftisoa6(implicit M: Monoid[Prod]): Prod <=> F[A7] =
      IsoSet(_._7, x => M.zero.map7(_ => x))

    implicit def liftisoa7(implicit M: Monoid[Prod]): Prod <=> F[A8] =
      IsoSet(_._8, x => M.zero.map8(_ => x))

    implicit def liftisoa8(implicit M: Monoid[Prod]): Prod <=> F[A9] =
      IsoSet(_._9, x => M.zero.map9(_ => x))

    implicit def liftisoa9(implicit M: Monoid[Prod]): Prod <=> F[A10] =
      IsoSet(_._10, x => M.zero.map10(_ => x))

    implicit def liftisoa10(implicit M: Monoid[Prod]): Prod <=> F[A11] =
      IsoSet(_._11, x => M.zero.map11(_ => x))

    implicit def liftisoa11(implicit M: Monoid[Prod]): Prod <=> F[A12] =
      IsoSet(_._12, x => M.zero.map12(_ => x))

    implicit def liftisoa12(implicit M: Monoid[Prod]): Prod <=> F[A13] =
      IsoSet(_._13, x => M.zero.map13(_ => x))

    implicit def liftisoa13(implicit M: Monoid[Prod]): Prod <=> F[A14] =
      IsoSet(_._14, x => M.zero.map14(_ => x))

    implicit def liftisoa14(implicit M: Monoid[Prod]): Prod <=> F[A15] =
      IsoSet(_._15, x => M.zero.map15(_ => x))

    implicit def liftisoa15(implicit M: Monoid[Prod]): Prod <=> F[A16] =
      IsoSet(_._16, x => M.zero.map16(_ => x))

    implicit def liftisoa16(implicit M: Monoid[Prod]): Prod <=> F[A17] =
      IsoSet(_._17, x => M.zero.map17(_ => x))

    implicit def liftisoa17(implicit M: Monoid[Prod]): Prod <=> F[A18] =
      IsoSet(_._18, x => M.zero.map18(_ => x))

    implicit def liftisoa18(implicit M: Monoid[Prod]): Prod <=> F[A19] =
      IsoSet(_._19, x => M.zero.map19(_ => x))

    implicit def liftisoa19(implicit M: Monoid[Prod]): Prod <=> F[A20] =
      IsoSet(_._20, x => M.zero.map20(_ => x))

    implicit def liftisoa20(implicit M: Monoid[Prod]): Prod <=> F[A21] =
      IsoSet(_._21, x => M.zero.map21(_ => x))

  }

  import instances._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]#Prod => AndXorK21[
    G,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21
  ]#Prod =
    (p: AndXorK21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]#Prod) =>
      (
        nt(p._1),
        nt(p._2),
        nt(p._3),
        nt(p._4),
        nt(p._5),
        nt(p._6),
        nt(p._7),
        nt(p._8),
        nt(p._9),
        nt(p._10),
        nt(p._11),
        nt(p._12),
        nt(p._13),
        nt(p._14),
        nt(p._15),
        nt(p._16),
        nt(p._17),
        nt(p._18),
        nt(p._19),
        nt(p._20),
        nt(p._21)
      )

  def transformC[G[_]](nt: (F ~> G)): AndXorK21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]#Cop => AndXorK21[
    G,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21
  ]#Cop =
    (p: AndXorK21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]#Cop) =>
      p.bimap(
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

  // format: off
  def sequenceP(prod: Prod)(implicit A: Apply[F]): F[AndXorK21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]#Prod] = {
    val (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = prod
    A.ap(a20)(
    A.ap(a19)(
    A.ap(a18)(
    A.ap(a17)(
    A.ap(a16)(
    A.ap(a15)(
    A.ap(a14)(
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
     A.map(a0)(((i0: A1, i1: A2, i2: A3, i3: A4, i4: A5, i5: A6, i6: A7, i7: A8, i8: A9, i9: A10, i10: A11, i11: A12, i12: A13, i13: A14, i14: A15, i15: A16, i16: A17, i17: A18, i18: A19, i19: A20, i20: A21) =>
    (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20)).curried)))))))))))))))))))))
  }
  

  def extractC[B](c: Cop)(implicit inj: Inj[Option[B], Cop]): Option[B] = inj(c)

  def extractP[B](p: Prod)(implicit inj: Inj[B, Prod]): B = inj(p)

  def toListP(p: Prod): List[Cop] = combine[Inj.Aux[List[Cop]]#Out].divide.apply(p)

  def foldMap[G[_], C](p: AndXor[G]#Prod)(
    map: AndXor[Id]#Cop => C)(
    implicit O: Ordering[AndXorK21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]#Cop], M: Monoid[C],
    PE: PlusEmpty[G], U: Uncons[G]): C = {
    val TG = AndXorF[G]
    val TI = AndXorF[Id]
    import scala.collection.mutable.{PriorityQueue => PQ}
    import TI.instances._
    def uncons(p: TG.Prod): (List[TI.Cop], TG.Prod) = {
     val hts = (U(p._1), U(p._2), U(p._3), U(p._4), U(p._5), U(p._6), U(p._7), U(p._8), U(p._9), U(p._10), U(p._11), U(p._12), U(p._13), U(p._14), U(p._15), U(p._16), U(p._17), U(p._18), U(p._19), U(p._20), U(p._21))
     (List(hts._1._1.map(TI.inj(_: A1)), hts._2._1.map(TI.inj(_: A2)), hts._3._1.map(TI.inj(_: A3)), hts._4._1.map(TI.inj(_: A4)), hts._5._1.map(TI.inj(_: A5)), hts._6._1.map(TI.inj(_: A6)), hts._7._1.map(TI.inj(_: A7)), hts._8._1.map(TI.inj(_: A8)), hts._9._1.map(TI.inj(_: A9)), hts._10._1.map(TI.inj(_: A10)), hts._11._1.map(TI.inj(_: A11)), hts._12._1.map(TI.inj(_: A12)), hts._13._1.map(TI.inj(_: A13)), hts._14._1.map(TI.inj(_: A14)), hts._15._1.map(TI.inj(_: A15)), hts._16._1.map(TI.inj(_: A16)), hts._17._1.map(TI.inj(_: A17)), hts._18._1.map(TI.inj(_: A18)), hts._19._1.map(TI.inj(_: A19)), hts._20._1.map(TI.inj(_: A20)), hts._21._1.map(TI.inj(_: A21))).flatten,
      (hts._1._2, hts._2._2, hts._3._2, hts._4._2, hts._5._2, hts._6._2, hts._7._2, hts._8._2, hts._9._2, hts._10._2, hts._11._2, hts._12._2, hts._13._2, hts._14._2, hts._15._2, hts._16._2, hts._17._2, hts._18._2, hts._19._2, hts._20._2, hts._21._2))
    }
    @scala.annotation.tailrec
    def go(prod: TG.Prod, q: PQ[TI.Cop], out: C): C =
     (prod.==((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8], PE.empty[A9], PE.empty[A10], PE.empty[A11], PE.empty[A12], PE.empty[A13], PE.empty[A14], PE.empty[A15], PE.empty[A16], PE.empty[A17], PE.empty[A18], PE.empty[A19], PE.empty[A20], PE.empty[A21]))) match {
       case true =>
         q.foldLeft(out)((acc, el) => M.append(acc, map(el)))
       case false => q.isEmpty match {
         case true => {
           val (hs, ts) = uncons(prod)
           q ++= hs
           go(ts, q, out)
         }
         case false => q.dequeue match {
                        case -\/(x) => {
               val (h, t) = U(prod._1)
               go((t, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13, prod._14, prod._15, prod._16, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(-\/(x)) => {
               val (h, t) = U(prod._2)
               go((prod._1, t, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13, prod._14, prod._15, prod._16, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(-\/(x))) => {
               val (h, t) = U(prod._3)
               go((prod._1, prod._2, t, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13, prod._14, prod._15, prod._16, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(-\/(x)))) => {
               val (h, t) = U(prod._4)
               go((prod._1, prod._2, prod._3, t, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13, prod._14, prod._15, prod._16, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(-\/(x))))) => {
               val (h, t) = U(prod._5)
               go((prod._1, prod._2, prod._3, prod._4, t, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13, prod._14, prod._15, prod._16, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => {
               val (h, t) = U(prod._6)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, t, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13, prod._14, prod._15, prod._16, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => {
               val (h, t) = U(prod._7)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, t, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13, prod._14, prod._15, prod._16, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => {
               val (h, t) = U(prod._8)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, t, prod._9, prod._10, prod._11, prod._12, prod._13, prod._14, prod._15, prod._16, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => {
               val (h, t) = U(prod._9)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, t, prod._10, prod._11, prod._12, prod._13, prod._14, prod._15, prod._16, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => {
               val (h, t) = U(prod._10)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, t, prod._11, prod._12, prod._13, prod._14, prod._15, prod._16, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => {
               val (h, t) = U(prod._11)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, t, prod._12, prod._13, prod._14, prod._15, prod._16, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => {
               val (h, t) = U(prod._12)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, t, prod._13, prod._14, prod._15, prod._16, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => {
               val (h, t) = U(prod._13)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, t, prod._14, prod._15, prod._16, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => {
               val (h, t) = U(prod._14)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13, t, prod._15, prod._16, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => {
               val (h, t) = U(prod._15)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13, prod._14, t, prod._16, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => {
               val (h, t) = U(prod._16)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13, prod._14, prod._15, t, prod._17, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => {
               val (h, t) = U(prod._17)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13, prod._14, prod._15, prod._16, t, prod._18, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) => {
               val (h, t) = U(prod._18)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13, prod._14, prod._15, prod._16, prod._17, t, prod._19, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))) => {
               val (h, t) = U(prod._19)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13, prod._14, prod._15, prod._16, prod._17, prod._18, t, prod._20, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))) => {
               val (h, t) = U(prod._20)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13, prod._14, prod._15, prod._16, prod._17, prod._18, prod._19, t, prod._21),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))))) => {
               val (h, t) = U(prod._21)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13, prod._14, prod._15, prod._16, prod._17, prod._18, prod._19, prod._20, t),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }

         }
       }
     }
    val Q = new scala.collection.mutable.PriorityQueue[TI.Cop]()
    val (hs, ts) = uncons(p)
    Q ++= hs
    go(ts, Q, M.zero)
  }
  // format: on
}

object AndXorK21 {

  def apply[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
    : AndXorK21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
    new AndXorK21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}
}

trait AndXorF21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {
  type Repr[F[_]] = AndXorK21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
  def apply[F[_]]: Repr[F] =
    new AndXorK21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}
}

object AndXorF21 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
    : AndXorF21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
    new AndXorF21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}
}

trait AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
    extends AndXorK21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

object AndXor21 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
    : AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
    new AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}

}
