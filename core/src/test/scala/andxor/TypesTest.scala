package andxor

import andxor.AndXorProperties.arbitrary._
import andxor.types._
import org.scalacheck.{Arbitrary, Properties}
import scalaz.Equal
import scalaz.scalacheck.ScalazProperties.lens
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.std.string._

object arbitrary {

  implicit def prod2Arb[F[_], A1[_[_]], A2[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]]): Arbitrary[Prod2[F, A1, A2]] =
    AndXor.buildNested[A1, A2].deriving[Arbitrary, F].apply

  implicit def cop2Arb[F[_], A1[_[_]], A2[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]]): Arbitrary[Cop2[F, A1, A2]] =
    AndXor.buildNested[A1, A2].deriving[Arbitrary, F].alt

  implicit def prod2Eq[F[_], A1[_[_]], A2[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]]): Equal[Prod2[F, A1, A2]] =
    AndXor.buildNested[A1, A2].deriving[Equal, F].divide

  implicit def cop2Eq[F[_], A1[_[_]], A2[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]]): Equal[Cop2[F, A1, A2]] =
    AndXor.buildNested[A1, A2].deriving[Equal, F].choose

  implicit def prod3Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]]): Arbitrary[Prod3[F, A1, A2, A3]] =
    AndXor.buildNested[A1, A2, A3].deriving[Arbitrary, F].apply

  implicit def cop3Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]]): Arbitrary[Cop3[F, A1, A2, A3]] =
    AndXor.buildNested[A1, A2, A3].deriving[Arbitrary, F].alt

  implicit def prod3Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]]): Equal[Prod3[F, A1, A2, A3]] =
    AndXor.buildNested[A1, A2, A3].deriving[Equal, F].divide

  implicit def cop3Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]]): Equal[Cop3[F, A1, A2, A3]] =
    AndXor.buildNested[A1, A2, A3].deriving[Equal, F].choose

  implicit def prod4Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]]): Arbitrary[Prod4[F, A1, A2, A3, A4]] =
    AndXor.buildNested[A1, A2, A3, A4].deriving[Arbitrary, F].apply

  implicit def cop4Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]]): Arbitrary[Cop4[F, A1, A2, A3, A4]] =
    AndXor.buildNested[A1, A2, A3, A4].deriving[Arbitrary, F].alt

  implicit def prod4Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]]): Equal[Prod4[F, A1, A2, A3, A4]] =
    AndXor.buildNested[A1, A2, A3, A4].deriving[Equal, F].divide

  implicit def cop4Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]]): Equal[Cop4[F, A1, A2, A3, A4]] =
    AndXor.buildNested[A1, A2, A3, A4].deriving[Equal, F].choose

  implicit def prod5Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]]): Arbitrary[Prod5[F, A1, A2, A3, A4, A5]] =
    AndXor.buildNested[A1, A2, A3, A4, A5].deriving[Arbitrary, F].apply

  implicit def cop5Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]]): Arbitrary[Cop5[F, A1, A2, A3, A4, A5]] =
    AndXor.buildNested[A1, A2, A3, A4, A5].deriving[Arbitrary, F].alt

  implicit def prod5Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]]): Equal[Prod5[F, A1, A2, A3, A4, A5]] =
    AndXor.buildNested[A1, A2, A3, A4, A5].deriving[Equal, F].divide

  implicit def cop5Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]]): Equal[Cop5[F, A1, A2, A3, A4, A5]] =
    AndXor.buildNested[A1, A2, A3, A4, A5].deriving[Equal, F].choose

  implicit def prod6Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]]): Arbitrary[Prod6[F, A1, A2, A3, A4, A5, A6]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6].deriving[Arbitrary, F].apply

  implicit def cop6Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]]): Arbitrary[Cop6[F, A1, A2, A3, A4, A5, A6]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6].deriving[Arbitrary, F].alt

  implicit def prod6Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]]): Equal[Prod6[F, A1, A2, A3, A4, A5, A6]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6].deriving[Equal, F].divide

  implicit def cop6Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]]): Equal[Cop6[F, A1, A2, A3, A4, A5, A6]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6].deriving[Equal, F].choose

  implicit def prod7Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]]): Arbitrary[Prod7[F, A1, A2, A3, A4, A5, A6, A7]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7].deriving[Arbitrary, F].apply

  implicit def cop7Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]]): Arbitrary[Cop7[F, A1, A2, A3, A4, A5, A6, A7]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7].deriving[Arbitrary, F].alt

  implicit def prod7Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]]): Equal[Prod7[F, A1, A2, A3, A4, A5, A6, A7]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7].deriving[Equal, F].divide

  implicit def cop7Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]]): Equal[Cop7[F, A1, A2, A3, A4, A5, A6, A7]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7].deriving[Equal, F].choose

  implicit def prod8Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]]): Arbitrary[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8].deriving[Arbitrary, F].apply

  implicit def cop8Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]]): Arbitrary[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8].deriving[Arbitrary, F].alt

  implicit def prod8Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]]): Equal[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8].deriving[Equal, F].divide

  implicit def cop8Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]]): Equal[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8].deriving[Equal, F].choose

  implicit def prod9Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]]): Arbitrary[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9].deriving[Arbitrary, F].apply

  implicit def cop9Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]]): Arbitrary[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9].deriving[Arbitrary, F].alt

  implicit def prod9Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]]): Equal[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9].deriving[Equal, F].divide

  implicit def cop9Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]]): Equal[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9].deriving[Equal, F].choose

  implicit def prod10Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]]): Arbitrary[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10].deriving[Arbitrary, F].apply

  implicit def cop10Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]]): Arbitrary[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10].deriving[Arbitrary, F].alt

  implicit def prod10Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]]): Equal[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10].deriving[Equal, F].divide

  implicit def cop10Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]]): Equal[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10].deriving[Equal, F].choose

  implicit def prod11Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]]): Arbitrary[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11].deriving[Arbitrary, F].apply

  implicit def cop11Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]]): Arbitrary[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11].deriving[Arbitrary, F].alt

  implicit def prod11Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]]): Equal[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11].deriving[Equal, F].divide

  implicit def cop11Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]]): Equal[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11].deriving[Equal, F].choose

  implicit def prod12Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]]): Arbitrary[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].deriving[Arbitrary, F].apply

  implicit def cop12Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]]): Arbitrary[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].deriving[Arbitrary, F].alt

  implicit def prod12Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]]): Equal[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].deriving[Equal, F].divide

  implicit def cop12Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]]): Equal[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].deriving[Equal, F].choose

  implicit def prod13Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]]): Arbitrary[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13].deriving[Arbitrary, F].apply

  implicit def cop13Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]]): Arbitrary[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13].deriving[Arbitrary, F].alt

  implicit def prod13Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]]): Equal[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13].deriving[Equal, F].divide

  implicit def cop13Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]]): Equal[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13].deriving[Equal, F].choose

  implicit def prod14Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]]): Arbitrary[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14].deriving[Arbitrary, F].apply

  implicit def cop14Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]]): Arbitrary[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14].deriving[Arbitrary, F].alt

  implicit def prod14Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]]): Equal[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14].deriving[Equal, F].divide

  implicit def cop14Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]]): Equal[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14].deriving[Equal, F].choose

  implicit def prod15Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]]): Arbitrary[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15].deriving[Arbitrary, F].apply

  implicit def cop15Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]]): Arbitrary[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15].deriving[Arbitrary, F].alt

  implicit def prod15Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]]): Equal[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15].deriving[Equal, F].divide

  implicit def cop15Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]]): Equal[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15].deriving[Equal, F].choose

  implicit def prod16Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]], a15: Arbitrary[A16[F]]): Arbitrary[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16].deriving[Arbitrary, F].apply

  implicit def cop16Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]], a15: Arbitrary[A16[F]]): Arbitrary[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16].deriving[Arbitrary, F].alt

  implicit def prod16Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]], e15: Equal[A16[F]]): Equal[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16].deriving[Equal, F].divide

  implicit def cop16Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]], e15: Equal[A16[F]]): Equal[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16].deriving[Equal, F].choose

  implicit def prod17Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]], a15: Arbitrary[A16[F]], a16: Arbitrary[A17[F]]): Arbitrary[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17].deriving[Arbitrary, F].apply

  implicit def cop17Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]], a15: Arbitrary[A16[F]], a16: Arbitrary[A17[F]]): Arbitrary[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17].deriving[Arbitrary, F].alt

  implicit def prod17Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]], e15: Equal[A16[F]], e16: Equal[A17[F]]): Equal[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17].deriving[Equal, F].divide

  implicit def cop17Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]], e15: Equal[A16[F]], e16: Equal[A17[F]]): Equal[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17].deriving[Equal, F].choose

  implicit def prod18Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]], a15: Arbitrary[A16[F]], a16: Arbitrary[A17[F]], a17: Arbitrary[A18[F]]): Arbitrary[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18].deriving[Arbitrary, F].apply

  implicit def cop18Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]], a15: Arbitrary[A16[F]], a16: Arbitrary[A17[F]], a17: Arbitrary[A18[F]]): Arbitrary[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18].deriving[Arbitrary, F].alt

  implicit def prod18Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]], e15: Equal[A16[F]], e16: Equal[A17[F]], e17: Equal[A18[F]]): Equal[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18].deriving[Equal, F].divide

  implicit def cop18Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]], e15: Equal[A16[F]], e16: Equal[A17[F]], e17: Equal[A18[F]]): Equal[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18].deriving[Equal, F].choose

  implicit def prod19Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]], a15: Arbitrary[A16[F]], a16: Arbitrary[A17[F]], a17: Arbitrary[A18[F]], a18: Arbitrary[A19[F]]): Arbitrary[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19].deriving[Arbitrary, F].apply

  implicit def cop19Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]], a15: Arbitrary[A16[F]], a16: Arbitrary[A17[F]], a17: Arbitrary[A18[F]], a18: Arbitrary[A19[F]]): Arbitrary[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19].deriving[Arbitrary, F].alt

  implicit def prod19Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]], e15: Equal[A16[F]], e16: Equal[A17[F]], e17: Equal[A18[F]], e18: Equal[A19[F]]): Equal[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19].deriving[Equal, F].divide

  implicit def cop19Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]], e15: Equal[A16[F]], e16: Equal[A17[F]], e17: Equal[A18[F]], e18: Equal[A19[F]]): Equal[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19].deriving[Equal, F].choose

  implicit def prod20Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]], a15: Arbitrary[A16[F]], a16: Arbitrary[A17[F]], a17: Arbitrary[A18[F]], a18: Arbitrary[A19[F]], a19: Arbitrary[A20[F]]): Arbitrary[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20].deriving[Arbitrary, F].apply

  implicit def cop20Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]], a15: Arbitrary[A16[F]], a16: Arbitrary[A17[F]], a17: Arbitrary[A18[F]], a18: Arbitrary[A19[F]], a19: Arbitrary[A20[F]]): Arbitrary[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20].deriving[Arbitrary, F].alt

  implicit def prod20Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]], e15: Equal[A16[F]], e16: Equal[A17[F]], e17: Equal[A18[F]], e18: Equal[A19[F]], e19: Equal[A20[F]]): Equal[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20].deriving[Equal, F].divide

  implicit def cop20Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]], e15: Equal[A16[F]], e16: Equal[A17[F]], e17: Equal[A18[F]], e18: Equal[A19[F]], e19: Equal[A20[F]]): Equal[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20].deriving[Equal, F].choose

  implicit def prod21Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]], a15: Arbitrary[A16[F]], a16: Arbitrary[A17[F]], a17: Arbitrary[A18[F]], a18: Arbitrary[A19[F]], a19: Arbitrary[A20[F]], a20: Arbitrary[A21[F]]): Arbitrary[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21].deriving[Arbitrary, F].apply

  implicit def cop21Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]], a15: Arbitrary[A16[F]], a16: Arbitrary[A17[F]], a17: Arbitrary[A18[F]], a18: Arbitrary[A19[F]], a19: Arbitrary[A20[F]], a20: Arbitrary[A21[F]]): Arbitrary[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21].deriving[Arbitrary, F].alt

  implicit def prod21Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]], e15: Equal[A16[F]], e16: Equal[A17[F]], e17: Equal[A18[F]], e18: Equal[A19[F]], e19: Equal[A20[F]], e20: Equal[A21[F]]): Equal[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21].deriving[Equal, F].divide

  implicit def cop21Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]], e15: Equal[A16[F]], e16: Equal[A17[F]], e17: Equal[A18[F]], e18: Equal[A19[F]], e19: Equal[A20[F]], e20: Equal[A21[F]]): Equal[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21].deriving[Equal, F].choose

  implicit def prod22Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]], A22[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]], a15: Arbitrary[A16[F]], a16: Arbitrary[A17[F]], a17: Arbitrary[A18[F]], a18: Arbitrary[A19[F]], a19: Arbitrary[A20[F]], a20: Arbitrary[A21[F]], a21: Arbitrary[A22[F]]): Arbitrary[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22].deriving[Arbitrary, F].apply

  implicit def cop22Arb[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]], A22[_[_]]](implicit a0: Arbitrary[A1[F]], a1: Arbitrary[A2[F]], a2: Arbitrary[A3[F]], a3: Arbitrary[A4[F]], a4: Arbitrary[A5[F]], a5: Arbitrary[A6[F]], a6: Arbitrary[A7[F]], a7: Arbitrary[A8[F]], a8: Arbitrary[A9[F]], a9: Arbitrary[A10[F]], a10: Arbitrary[A11[F]], a11: Arbitrary[A12[F]], a12: Arbitrary[A13[F]], a13: Arbitrary[A14[F]], a14: Arbitrary[A15[F]], a15: Arbitrary[A16[F]], a16: Arbitrary[A17[F]], a17: Arbitrary[A18[F]], a18: Arbitrary[A19[F]], a19: Arbitrary[A20[F]], a20: Arbitrary[A21[F]], a21: Arbitrary[A22[F]]): Arbitrary[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22].deriving[Arbitrary, F].alt

  implicit def prod22Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]], A22[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]], e15: Equal[A16[F]], e16: Equal[A17[F]], e17: Equal[A18[F]], e18: Equal[A19[F]], e19: Equal[A20[F]], e20: Equal[A21[F]], e21: Equal[A22[F]]): Equal[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22].deriving[Equal, F].divide

  implicit def cop22Eq[F[_], A1[_[_]], A2[_[_]], A3[_[_]], A4[_[_]], A5[_[_]], A6[_[_]], A7[_[_]], A8[_[_]], A9[_[_]], A10[_[_]], A11[_[_]], A12[_[_]], A13[_[_]], A14[_[_]], A15[_[_]], A16[_[_]], A17[_[_]], A18[_[_]], A19[_[_]], A20[_[_]], A21[_[_]], A22[_[_]]](implicit e0: Equal[A1[F]], e1: Equal[A2[F]], e2: Equal[A3[F]], e3: Equal[A4[F]], e4: Equal[A5[F]], e5: Equal[A6[F]], e6: Equal[A7[F]], e7: Equal[A8[F]], e8: Equal[A9[F]], e9: Equal[A10[F]], e10: Equal[A11[F]], e11: Equal[A12[F]], e12: Equal[A13[F]], e13: Equal[A14[F]], e14: Equal[A15[F]], e15: Equal[A16[F]], e16: Equal[A17[F]], e17: Equal[A18[F]], e18: Equal[A19[F]], e19: Equal[A20[F]], e20: Equal[A21[F]], e21: Equal[A22[F]]): Equal[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
    AndXor.buildNested[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22].deriving[Equal, F].choose

}

object Prod2Test extends Properties("Prod2") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod2[?[_], FConst[String]#T, FConst[Int]#T]])

  include(lens.laws[Prod2[Option, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod2.Prod2Lens0[Option, FConst[String]#T, FConst[Int]#T]), "0.")

  include(lens.laws[Prod2[Option, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod2.Prod2Lens1[Option, FConst[String]#T, FConst[Int]#T]), "1.")

}

object Cop2Test extends Properties("Cop2") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop2[?[_], FConst[String]#T, FConst[Int]#T]])
}

object Prod3Test extends Properties("Prod3") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod3[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T]])

  include(lens.laws[Prod3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod3.Prod3Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "0.")

  include(lens.laws[Prod3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod3.Prod3Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "1.")

  include(lens.laws[Prod3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod3.Prod3Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "2.")

}

object Cop3Test extends Properties("Cop3") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop3[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T]])
}

object Prod4Test extends Properties("Prod4") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod4[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])

  include(lens.laws[Prod4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod4.Prod4Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "0.")

  include(lens.laws[Prod4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod4.Prod4Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "1.")

  include(lens.laws[Prod4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod4.Prod4Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "2.")

  include(lens.laws[Prod4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod4.Prod4Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "3.")

}

object Cop4Test extends Properties("Cop4") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop4[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])
}

object Prod5Test extends Properties("Prod5") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod5[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])

  include(lens.laws[Prod5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod5.Prod5Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "0.")

  include(lens.laws[Prod5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod5.Prod5Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "1.")

  include(lens.laws[Prod5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod5.Prod5Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "2.")

  include(lens.laws[Prod5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod5.Prod5Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "3.")

  include(lens.laws[Prod5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod5.Prod5Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "4.")

}

object Cop5Test extends Properties("Cop5") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop5[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])
}

object Prod6Test extends Properties("Prod6") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod6[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])

  include(lens.laws[Prod6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod6.Prod6Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "0.")

  include(lens.laws[Prod6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod6.Prod6Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "1.")

  include(lens.laws[Prod6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod6.Prod6Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "2.")

  include(lens.laws[Prod6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod6.Prod6Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "3.")

  include(lens.laws[Prod6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod6.Prod6Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "4.")

  include(lens.laws[Prod6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod6.Prod6Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "5.")

}

object Cop6Test extends Properties("Cop6") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop6[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])
}

object Prod7Test extends Properties("Prod7") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod7[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])

  include(lens.laws[Prod7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod7.Prod7Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "0.")

  include(lens.laws[Prod7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod7.Prod7Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "1.")

  include(lens.laws[Prod7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod7.Prod7Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "2.")

  include(lens.laws[Prod7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod7.Prod7Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "3.")

  include(lens.laws[Prod7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod7.Prod7Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "4.")

  include(lens.laws[Prod7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod7.Prod7Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "5.")

  include(lens.laws[Prod7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod7.Prod7Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "6.")

}

object Cop7Test extends Properties("Cop7") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop7[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])
}

object Prod8Test extends Properties("Prod8") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod8[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])

  include(lens.laws[Prod8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod8.Prod8Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "0.")

  include(lens.laws[Prod8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod8.Prod8Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "1.")

  include(lens.laws[Prod8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod8.Prod8Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "2.")

  include(lens.laws[Prod8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod8.Prod8Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "3.")

  include(lens.laws[Prod8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod8.Prod8Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "4.")

  include(lens.laws[Prod8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod8.Prod8Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "5.")

  include(lens.laws[Prod8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod8.Prod8Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "6.")

  include(lens.laws[Prod8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod8.Prod8Lens7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "7.")

}

object Cop8Test extends Properties("Cop8") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop8[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])
}

object Prod9Test extends Properties("Prod9") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod9[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])

  include(lens.laws[Prod9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod9.Prod9Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "0.")

  include(lens.laws[Prod9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod9.Prod9Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "1.")

  include(lens.laws[Prod9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod9.Prod9Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "2.")

  include(lens.laws[Prod9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod9.Prod9Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "3.")

  include(lens.laws[Prod9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod9.Prod9Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "4.")

  include(lens.laws[Prod9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod9.Prod9Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "5.")

  include(lens.laws[Prod9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod9.Prod9Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "6.")

  include(lens.laws[Prod9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod9.Prod9Lens7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "7.")

  include(lens.laws[Prod9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod9.Prod9Lens8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "8.")

}

object Cop9Test extends Properties("Cop9") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop9[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])
}

object Prod10Test extends Properties("Prod10") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod10[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])

  include(lens.laws[Prod10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod10.Prod10Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "0.")

  include(lens.laws[Prod10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod10.Prod10Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "1.")

  include(lens.laws[Prod10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod10.Prod10Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "2.")

  include(lens.laws[Prod10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod10.Prod10Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "3.")

  include(lens.laws[Prod10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod10.Prod10Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "4.")

  include(lens.laws[Prod10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod10.Prod10Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "5.")

  include(lens.laws[Prod10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod10.Prod10Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "6.")

  include(lens.laws[Prod10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod10.Prod10Lens7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "7.")

  include(lens.laws[Prod10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod10.Prod10Lens8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "8.")

  include(lens.laws[Prod10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod10.Prod10Lens9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "9.")

}

object Cop10Test extends Properties("Cop10") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop10[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])
}

object Prod11Test extends Properties("Prod11") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod11[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])

  include(lens.laws[Prod11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod11.Prod11Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "0.")

  include(lens.laws[Prod11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod11.Prod11Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "1.")

  include(lens.laws[Prod11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod11.Prod11Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "2.")

  include(lens.laws[Prod11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod11.Prod11Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "3.")

  include(lens.laws[Prod11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod11.Prod11Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "4.")

  include(lens.laws[Prod11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod11.Prod11Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "5.")

  include(lens.laws[Prod11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod11.Prod11Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "6.")

  include(lens.laws[Prod11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod11.Prod11Lens7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "7.")

  include(lens.laws[Prod11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod11.Prod11Lens8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "8.")

  include(lens.laws[Prod11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod11.Prod11Lens9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "9.")

  include(lens.laws[Prod11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod11.Prod11Lens10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "10.")

}

object Cop11Test extends Properties("Cop11") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop11[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])
}

object Prod12Test extends Properties("Prod12") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod12[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])

  include(lens.laws[Prod12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod12.Prod12Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "0.")

  include(lens.laws[Prod12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod12.Prod12Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "1.")

  include(lens.laws[Prod12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod12.Prod12Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "2.")

  include(lens.laws[Prod12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod12.Prod12Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "3.")

  include(lens.laws[Prod12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod12.Prod12Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "4.")

  include(lens.laws[Prod12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod12.Prod12Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "5.")

  include(lens.laws[Prod12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod12.Prod12Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "6.")

  include(lens.laws[Prod12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod12.Prod12Lens7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "7.")

  include(lens.laws[Prod12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod12.Prod12Lens8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "8.")

  include(lens.laws[Prod12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod12.Prod12Lens9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "9.")

  include(lens.laws[Prod12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod12.Prod12Lens10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "10.")

  include(lens.laws[Prod12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod12.Prod12Lens11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "11.")

}

object Cop12Test extends Properties("Cop12") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop12[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])
}

object Prod13Test extends Properties("Prod13") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod13[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])

  include(lens.laws[Prod13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod13.Prod13Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "0.")

  include(lens.laws[Prod13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod13.Prod13Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "1.")

  include(lens.laws[Prod13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod13.Prod13Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "2.")

  include(lens.laws[Prod13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod13.Prod13Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "3.")

  include(lens.laws[Prod13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod13.Prod13Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "4.")

  include(lens.laws[Prod13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod13.Prod13Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "5.")

  include(lens.laws[Prod13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod13.Prod13Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "6.")

  include(lens.laws[Prod13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod13.Prod13Lens7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "7.")

  include(lens.laws[Prod13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod13.Prod13Lens8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "8.")

  include(lens.laws[Prod13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod13.Prod13Lens9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "9.")

  include(lens.laws[Prod13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod13.Prod13Lens10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "10.")

  include(lens.laws[Prod13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod13.Prod13Lens11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "11.")

  include(lens.laws[Prod13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod13.Prod13Lens12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "12.")

}

object Cop13Test extends Properties("Cop13") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop13[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])
}

object Prod14Test extends Properties("Prod14") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod14[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])

  include(lens.laws[Prod14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod14.Prod14Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "0.")

  include(lens.laws[Prod14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod14.Prod14Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "1.")

  include(lens.laws[Prod14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod14.Prod14Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "2.")

  include(lens.laws[Prod14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod14.Prod14Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "3.")

  include(lens.laws[Prod14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod14.Prod14Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "4.")

  include(lens.laws[Prod14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod14.Prod14Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "5.")

  include(lens.laws[Prod14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod14.Prod14Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "6.")

  include(lens.laws[Prod14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod14.Prod14Lens7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "7.")

  include(lens.laws[Prod14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod14.Prod14Lens8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "8.")

  include(lens.laws[Prod14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod14.Prod14Lens9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "9.")

  include(lens.laws[Prod14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod14.Prod14Lens10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "10.")

  include(lens.laws[Prod14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod14.Prod14Lens11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "11.")

  include(lens.laws[Prod14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod14.Prod14Lens12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "12.")

  include(lens.laws[Prod14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod14.Prod14Lens13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "13.")

}

object Cop14Test extends Properties("Cop14") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop14[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])
}

object Prod15Test extends Properties("Prod15") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod15[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])

  include(lens.laws[Prod15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod15.Prod15Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "0.")

  include(lens.laws[Prod15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod15.Prod15Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "1.")

  include(lens.laws[Prod15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod15.Prod15Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "2.")

  include(lens.laws[Prod15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod15.Prod15Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "3.")

  include(lens.laws[Prod15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod15.Prod15Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "4.")

  include(lens.laws[Prod15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod15.Prod15Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "5.")

  include(lens.laws[Prod15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod15.Prod15Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "6.")

  include(lens.laws[Prod15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod15.Prod15Lens7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "7.")

  include(lens.laws[Prod15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod15.Prod15Lens8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "8.")

  include(lens.laws[Prod15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod15.Prod15Lens9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "9.")

  include(lens.laws[Prod15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod15.Prod15Lens10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "10.")

  include(lens.laws[Prod15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod15.Prod15Lens11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "11.")

  include(lens.laws[Prod15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod15.Prod15Lens12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "12.")

  include(lens.laws[Prod15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod15.Prod15Lens13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "13.")

  include(lens.laws[Prod15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod15.Prod15Lens14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "14.")

}

object Cop15Test extends Properties("Cop15") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop15[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])
}

object Prod16Test extends Properties("Prod16") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod16[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod16.Prod16Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "0.")

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod16.Prod16Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "1.")

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod16.Prod16Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "2.")

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod16.Prod16Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "3.")

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod16.Prod16Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "4.")

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod16.Prod16Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "5.")

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod16.Prod16Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "6.")

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod16.Prod16Lens7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "7.")

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod16.Prod16Lens8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "8.")

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod16.Prod16Lens9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "9.")

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod16.Prod16Lens10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "10.")

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod16.Prod16Lens11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "11.")

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod16.Prod16Lens12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "12.")

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod16.Prod16Lens13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "13.")

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod16.Prod16Lens14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "14.")

  include(lens.laws[Prod16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod16.Prod16Lens15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "15.")

}

object Cop16Test extends Properties("Cop16") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop16[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])
}

object Prod17Test extends Properties("Prod17") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod17[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod17.Prod17Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "0.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod17.Prod17Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "1.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod17.Prod17Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "2.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod17.Prod17Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "3.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod17.Prod17Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "4.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod17.Prod17Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "5.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod17.Prod17Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "6.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod17.Prod17Lens7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "7.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod17.Prod17Lens8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "8.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod17.Prod17Lens9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "9.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod17.Prod17Lens10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "10.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod17.Prod17Lens11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "11.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod17.Prod17Lens12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "12.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod17.Prod17Lens13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "13.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod17.Prod17Lens14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "14.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod17.Prod17Lens15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "15.")

  include(lens.laws[Prod17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod17.Prod17Lens16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "16.")

}

object Cop17Test extends Properties("Cop17") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop17[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])
}

object Prod18Test extends Properties("Prod18") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod18[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod18.Prod18Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "0.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod18.Prod18Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "1.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod18.Prod18Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "2.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod18.Prod18Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "3.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod18.Prod18Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "4.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod18.Prod18Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "5.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod18.Prod18Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "6.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod18.Prod18Lens7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "7.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod18.Prod18Lens8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "8.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod18.Prod18Lens9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "9.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod18.Prod18Lens10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "10.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod18.Prod18Lens11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "11.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod18.Prod18Lens12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "12.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod18.Prod18Lens13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "13.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod18.Prod18Lens14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "14.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod18.Prod18Lens15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "15.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod18.Prod18Lens16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "16.")

  include(lens.laws[Prod18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod18.Prod18Lens17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "17.")

}

object Cop18Test extends Properties("Cop18") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop18[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])
}

object Prod19Test extends Properties("Prod19") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod19[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod19.Prod19Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "0.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod19.Prod19Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "1.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod19.Prod19Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "2.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod19.Prod19Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "3.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod19.Prod19Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "4.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod19.Prod19Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "5.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod19.Prod19Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "6.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod19.Prod19Lens7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "7.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod19.Prod19Lens8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "8.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod19.Prod19Lens9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "9.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod19.Prod19Lens10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "10.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod19.Prod19Lens11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "11.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod19.Prod19Lens12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "12.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod19.Prod19Lens13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "13.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod19.Prod19Lens14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "14.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod19.Prod19Lens15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "15.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod19.Prod19Lens16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "16.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod19.Prod19Lens17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "17.")

  include(lens.laws[Prod19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod19.Prod19Lens18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "18.")

}

object Cop19Test extends Properties("Cop19") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop19[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])
}

object Prod20Test extends Properties("Prod20") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod20[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod20.Prod20Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "0.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod20.Prod20Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "1.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod20.Prod20Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "2.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod20.Prod20Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "3.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod20.Prod20Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "4.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod20.Prod20Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "5.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod20.Prod20Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "6.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod20.Prod20Lens7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "7.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod20.Prod20Lens8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "8.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod20.Prod20Lens9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "9.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod20.Prod20Lens10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "10.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod20.Prod20Lens11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "11.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod20.Prod20Lens12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "12.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod20.Prod20Lens13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "13.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod20.Prod20Lens14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "14.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod20.Prod20Lens15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "15.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod20.Prod20Lens16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "16.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod20.Prod20Lens17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "17.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod20.Prod20Lens18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "18.")

  include(lens.laws[Prod20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod20.Prod20Lens19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "19.")

}

object Cop20Test extends Properties("Cop20") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop20[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])
}

object Prod21Test extends Properties("Prod21") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod21[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod21.Prod21Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "0.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod21.Prod21Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "1.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod21.Prod21Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "2.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod21.Prod21Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "3.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod21.Prod21Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "4.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod21.Prod21Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "5.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod21.Prod21Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "6.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod21.Prod21Lens7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "7.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod21.Prod21Lens8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "8.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod21.Prod21Lens9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "9.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod21.Prod21Lens10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "10.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod21.Prod21Lens11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "11.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod21.Prod21Lens12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "12.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod21.Prod21Lens13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "13.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod21.Prod21Lens14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "14.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod21.Prod21Lens15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "15.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod21.Prod21Lens16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "16.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod21.Prod21Lens17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "17.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod21.Prod21Lens18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "18.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[Int]](
    Prod21.Prod21Lens19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "19.")

  include(lens.laws[Prod21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T], Option[String]](
    Prod21.Prod21Lens20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]), "20.")

}

object Cop21Test extends Properties("Cop21") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop21[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T]])
}

object Prod22Test extends Properties("Prod22") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod22[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod22.Prod22Lens0[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "0.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod22.Prod22Lens1[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "1.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod22.Prod22Lens2[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "2.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod22.Prod22Lens3[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "3.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod22.Prod22Lens4[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "4.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod22.Prod22Lens5[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "5.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod22.Prod22Lens6[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "6.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod22.Prod22Lens7[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "7.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod22.Prod22Lens8[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "8.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod22.Prod22Lens9[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "9.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod22.Prod22Lens10[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "10.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod22.Prod22Lens11[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "11.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod22.Prod22Lens12[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "12.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod22.Prod22Lens13[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "13.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod22.Prod22Lens14[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "14.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod22.Prod22Lens15[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "15.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod22.Prod22Lens16[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "16.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod22.Prod22Lens17[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "17.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod22.Prod22Lens18[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "18.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod22.Prod22Lens19[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "19.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[String]](
    Prod22.Prod22Lens20[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "20.")

  include(lens.laws[Prod22[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T], Option[Int]](
    Prod22.Prod22Lens21[Option, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]), "21.")

}

object Cop22Test extends Properties("Cop22") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop22[?[_], FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T, FConst[String]#T, FConst[Int]#T]])
}

