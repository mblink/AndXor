package andxor

import andxor.AndXorProperties.arbitrary._
import andxor.types._
import org.scalacheck.{Arbitrary, Properties}
import cats.{Applicative, Eq, Functor}
import cats.instances.int._
import cats.instances.list._
import cats.instances.option._
import cats.instances.string._
import cats.instances.tuple._
import cats.instances.vector._
import cats.kernel.laws.discipline.MonoidTests
import monocle.law.discipline.LensTests

object arbitrary {

  implicit def prod2Arb[F[_], A1, A2](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]]): Arbitrary[Prod2[F, A1, A2]] =
    AndXor[A1, A2].deriving[Arbitrary, F].apply

  implicit def cop2Arb[F[_], A1, A2](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]]): Arbitrary[Cop2[F, A1, A2]] =
    AndXor[A1, A2].deriving[Arbitrary, F].alt

  implicit def prod2Eq[F[_], A1, A2](implicit e0: Eq[F[A1]], e1: Eq[F[A2]]): Eq[Prod2[F, A1, A2]] =
    AndXor[A1, A2].deriving[Eq, F].divide

  implicit def cop2Eq[F[_], A1, A2](implicit e0: Eq[F[A1]], e1: Eq[F[A2]]): Eq[Cop2[F, A1, A2]] =
    AndXor[A1, A2].deriving[Eq, F].choose

  implicit def prod3Arb[F[_], A1, A2, A3](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]]): Arbitrary[Prod3[F, A1, A2, A3]] =
    AndXor[A1, A2, A3].deriving[Arbitrary, F].apply

  implicit def cop3Arb[F[_], A1, A2, A3](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]]): Arbitrary[Cop3[F, A1, A2, A3]] =
    AndXor[A1, A2, A3].deriving[Arbitrary, F].alt

  implicit def prod3Eq[F[_], A1, A2, A3](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]]): Eq[Prod3[F, A1, A2, A3]] =
    AndXor[A1, A2, A3].deriving[Eq, F].divide

  implicit def cop3Eq[F[_], A1, A2, A3](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]]): Eq[Cop3[F, A1, A2, A3]] =
    AndXor[A1, A2, A3].deriving[Eq, F].choose

  implicit def prod4Arb[F[_], A1, A2, A3, A4](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]]): Arbitrary[Prod4[F, A1, A2, A3, A4]] =
    AndXor[A1, A2, A3, A4].deriving[Arbitrary, F].apply

  implicit def cop4Arb[F[_], A1, A2, A3, A4](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]]): Arbitrary[Cop4[F, A1, A2, A3, A4]] =
    AndXor[A1, A2, A3, A4].deriving[Arbitrary, F].alt

  implicit def prod4Eq[F[_], A1, A2, A3, A4](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]]): Eq[Prod4[F, A1, A2, A3, A4]] =
    AndXor[A1, A2, A3, A4].deriving[Eq, F].divide

  implicit def cop4Eq[F[_], A1, A2, A3, A4](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]]): Eq[Cop4[F, A1, A2, A3, A4]] =
    AndXor[A1, A2, A3, A4].deriving[Eq, F].choose

  implicit def prod5Arb[F[_], A1, A2, A3, A4, A5](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]]): Arbitrary[Prod5[F, A1, A2, A3, A4, A5]] =
    AndXor[A1, A2, A3, A4, A5].deriving[Arbitrary, F].apply

  implicit def cop5Arb[F[_], A1, A2, A3, A4, A5](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]]): Arbitrary[Cop5[F, A1, A2, A3, A4, A5]] =
    AndXor[A1, A2, A3, A4, A5].deriving[Arbitrary, F].alt

  implicit def prod5Eq[F[_], A1, A2, A3, A4, A5](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]]): Eq[Prod5[F, A1, A2, A3, A4, A5]] =
    AndXor[A1, A2, A3, A4, A5].deriving[Eq, F].divide

  implicit def cop5Eq[F[_], A1, A2, A3, A4, A5](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]]): Eq[Cop5[F, A1, A2, A3, A4, A5]] =
    AndXor[A1, A2, A3, A4, A5].deriving[Eq, F].choose

  implicit def prod6Arb[F[_], A1, A2, A3, A4, A5, A6](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]]): Arbitrary[Prod6[F, A1, A2, A3, A4, A5, A6]] =
    AndXor[A1, A2, A3, A4, A5, A6].deriving[Arbitrary, F].apply

  implicit def cop6Arb[F[_], A1, A2, A3, A4, A5, A6](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]]): Arbitrary[Cop6[F, A1, A2, A3, A4, A5, A6]] =
    AndXor[A1, A2, A3, A4, A5, A6].deriving[Arbitrary, F].alt

  implicit def prod6Eq[F[_], A1, A2, A3, A4, A5, A6](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]]): Eq[Prod6[F, A1, A2, A3, A4, A5, A6]] =
    AndXor[A1, A2, A3, A4, A5, A6].deriving[Eq, F].divide

  implicit def cop6Eq[F[_], A1, A2, A3, A4, A5, A6](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]]): Eq[Cop6[F, A1, A2, A3, A4, A5, A6]] =
    AndXor[A1, A2, A3, A4, A5, A6].deriving[Eq, F].choose

  implicit def prod7Arb[F[_], A1, A2, A3, A4, A5, A6, A7](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]]): Arbitrary[Prod7[F, A1, A2, A3, A4, A5, A6, A7]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7].deriving[Arbitrary, F].apply

  implicit def cop7Arb[F[_], A1, A2, A3, A4, A5, A6, A7](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]]): Arbitrary[Cop7[F, A1, A2, A3, A4, A5, A6, A7]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7].deriving[Arbitrary, F].alt

  implicit def prod7Eq[F[_], A1, A2, A3, A4, A5, A6, A7](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]]): Eq[Prod7[F, A1, A2, A3, A4, A5, A6, A7]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7].deriving[Eq, F].divide

  implicit def cop7Eq[F[_], A1, A2, A3, A4, A5, A6, A7](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]]): Eq[Cop7[F, A1, A2, A3, A4, A5, A6, A7]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7].deriving[Eq, F].choose

  implicit def prod8Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]]): Arbitrary[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8].deriving[Arbitrary, F].apply

  implicit def cop8Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]]): Arbitrary[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8].deriving[Arbitrary, F].alt

  implicit def prod8Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]]): Eq[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8].deriving[Eq, F].divide

  implicit def cop8Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]]): Eq[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8].deriving[Eq, F].choose

  implicit def prod9Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]]): Arbitrary[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9].deriving[Arbitrary, F].apply

  implicit def cop9Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]]): Arbitrary[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9].deriving[Arbitrary, F].alt

  implicit def prod9Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]]): Eq[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9].deriving[Eq, F].divide

  implicit def cop9Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]]): Eq[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9].deriving[Eq, F].choose

  implicit def prod10Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]]): Arbitrary[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10].deriving[Arbitrary, F].apply

  implicit def cop10Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]]): Arbitrary[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10].deriving[Arbitrary, F].alt

  implicit def prod10Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]]): Eq[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10].deriving[Eq, F].divide

  implicit def cop10Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]]): Eq[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10].deriving[Eq, F].choose

  implicit def prod11Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]]): Arbitrary[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11].deriving[Arbitrary, F].apply

  implicit def cop11Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]]): Arbitrary[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11].deriving[Arbitrary, F].alt

  implicit def prod11Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]]): Eq[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11].deriving[Eq, F].divide

  implicit def cop11Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]]): Eq[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11].deriving[Eq, F].choose

  implicit def prod12Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]]): Arbitrary[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].deriving[Arbitrary, F].apply

  implicit def cop12Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]]): Arbitrary[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].deriving[Arbitrary, F].alt

  implicit def prod12Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]]): Eq[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].deriving[Eq, F].divide

  implicit def cop12Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]]): Eq[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].deriving[Eq, F].choose

  implicit def prod13Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]]): Arbitrary[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13].deriving[Arbitrary, F].apply

  implicit def cop13Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]]): Arbitrary[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13].deriving[Arbitrary, F].alt

  implicit def prod13Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]]): Eq[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13].deriving[Eq, F].divide

  implicit def cop13Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]]): Eq[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13].deriving[Eq, F].choose

  implicit def prod14Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]]): Arbitrary[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14].deriving[Arbitrary, F].apply

  implicit def cop14Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]]): Arbitrary[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14].deriving[Arbitrary, F].alt

  implicit def prod14Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]]): Eq[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14].deriving[Eq, F].divide

  implicit def cop14Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]]): Eq[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14].deriving[Eq, F].choose

  implicit def prod15Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]]): Arbitrary[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15].deriving[Arbitrary, F].apply

  implicit def cop15Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]]): Arbitrary[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15].deriving[Arbitrary, F].alt

  implicit def prod15Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]]): Eq[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15].deriving[Eq, F].divide

  implicit def cop15Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]]): Eq[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15].deriving[Eq, F].choose

  implicit def prod16Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]], a15: Arbitrary[F[A16]]): Arbitrary[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16].deriving[Arbitrary, F].apply

  implicit def cop16Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]], a15: Arbitrary[F[A16]]): Arbitrary[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16].deriving[Arbitrary, F].alt

  implicit def prod16Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]], e15: Eq[F[A16]]): Eq[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16].deriving[Eq, F].divide

  implicit def cop16Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]], e15: Eq[F[A16]]): Eq[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16].deriving[Eq, F].choose

  implicit def prod17Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]], a15: Arbitrary[F[A16]], a16: Arbitrary[F[A17]]): Arbitrary[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17].deriving[Arbitrary, F].apply

  implicit def cop17Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]], a15: Arbitrary[F[A16]], a16: Arbitrary[F[A17]]): Arbitrary[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17].deriving[Arbitrary, F].alt

  implicit def prod17Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]], e15: Eq[F[A16]], e16: Eq[F[A17]]): Eq[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17].deriving[Eq, F].divide

  implicit def cop17Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]], e15: Eq[F[A16]], e16: Eq[F[A17]]): Eq[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17].deriving[Eq, F].choose

  implicit def prod18Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]], a15: Arbitrary[F[A16]], a16: Arbitrary[F[A17]], a17: Arbitrary[F[A18]]): Arbitrary[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18].deriving[Arbitrary, F].apply

  implicit def cop18Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]], a15: Arbitrary[F[A16]], a16: Arbitrary[F[A17]], a17: Arbitrary[F[A18]]): Arbitrary[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18].deriving[Arbitrary, F].alt

  implicit def prod18Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]], e15: Eq[F[A16]], e16: Eq[F[A17]], e17: Eq[F[A18]]): Eq[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18].deriving[Eq, F].divide

  implicit def cop18Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]], e15: Eq[F[A16]], e16: Eq[F[A17]], e17: Eq[F[A18]]): Eq[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18].deriving[Eq, F].choose

  implicit def prod19Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]], a15: Arbitrary[F[A16]], a16: Arbitrary[F[A17]], a17: Arbitrary[F[A18]], a18: Arbitrary[F[A19]]): Arbitrary[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19].deriving[Arbitrary, F].apply

  implicit def cop19Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]], a15: Arbitrary[F[A16]], a16: Arbitrary[F[A17]], a17: Arbitrary[F[A18]], a18: Arbitrary[F[A19]]): Arbitrary[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19].deriving[Arbitrary, F].alt

  implicit def prod19Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]], e15: Eq[F[A16]], e16: Eq[F[A17]], e17: Eq[F[A18]], e18: Eq[F[A19]]): Eq[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19].deriving[Eq, F].divide

  implicit def cop19Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]], e15: Eq[F[A16]], e16: Eq[F[A17]], e17: Eq[F[A18]], e18: Eq[F[A19]]): Eq[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19].deriving[Eq, F].choose

  implicit def prod20Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]], a15: Arbitrary[F[A16]], a16: Arbitrary[F[A17]], a17: Arbitrary[F[A18]], a18: Arbitrary[F[A19]], a19: Arbitrary[F[A20]]): Arbitrary[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20].deriving[Arbitrary, F].apply

  implicit def cop20Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]], a15: Arbitrary[F[A16]], a16: Arbitrary[F[A17]], a17: Arbitrary[F[A18]], a18: Arbitrary[F[A19]], a19: Arbitrary[F[A20]]): Arbitrary[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20].deriving[Arbitrary, F].alt

  implicit def prod20Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]], e15: Eq[F[A16]], e16: Eq[F[A17]], e17: Eq[F[A18]], e18: Eq[F[A19]], e19: Eq[F[A20]]): Eq[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20].deriving[Eq, F].divide

  implicit def cop20Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]], e15: Eq[F[A16]], e16: Eq[F[A17]], e17: Eq[F[A18]], e18: Eq[F[A19]], e19: Eq[F[A20]]): Eq[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20].deriving[Eq, F].choose

  implicit def prod21Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]], a15: Arbitrary[F[A16]], a16: Arbitrary[F[A17]], a17: Arbitrary[F[A18]], a18: Arbitrary[F[A19]], a19: Arbitrary[F[A20]], a20: Arbitrary[F[A21]]): Arbitrary[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21].deriving[Arbitrary, F].apply

  implicit def cop21Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]], a15: Arbitrary[F[A16]], a16: Arbitrary[F[A17]], a17: Arbitrary[F[A18]], a18: Arbitrary[F[A19]], a19: Arbitrary[F[A20]], a20: Arbitrary[F[A21]]): Arbitrary[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21].deriving[Arbitrary, F].alt

  implicit def prod21Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]], e15: Eq[F[A16]], e16: Eq[F[A17]], e17: Eq[F[A18]], e18: Eq[F[A19]], e19: Eq[F[A20]], e20: Eq[F[A21]]): Eq[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21].deriving[Eq, F].divide

  implicit def cop21Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]], e15: Eq[F[A16]], e16: Eq[F[A17]], e17: Eq[F[A18]], e18: Eq[F[A19]], e19: Eq[F[A20]], e20: Eq[F[A21]]): Eq[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21].deriving[Eq, F].choose

  implicit def prod22Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]], a15: Arbitrary[F[A16]], a16: Arbitrary[F[A17]], a17: Arbitrary[F[A18]], a18: Arbitrary[F[A19]], a19: Arbitrary[F[A20]], a20: Arbitrary[F[A21]], a21: Arbitrary[F[A22]]): Arbitrary[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22].deriving[Arbitrary, F].apply

  implicit def cop22Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]], a6: Arbitrary[F[A7]], a7: Arbitrary[F[A8]], a8: Arbitrary[F[A9]], a9: Arbitrary[F[A10]], a10: Arbitrary[F[A11]], a11: Arbitrary[F[A12]], a12: Arbitrary[F[A13]], a13: Arbitrary[F[A14]], a14: Arbitrary[F[A15]], a15: Arbitrary[F[A16]], a16: Arbitrary[F[A17]], a17: Arbitrary[F[A18]], a18: Arbitrary[F[A19]], a19: Arbitrary[F[A20]], a20: Arbitrary[F[A21]], a21: Arbitrary[F[A22]]): Arbitrary[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22].deriving[Arbitrary, F].alt

  implicit def prod22Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]], e15: Eq[F[A16]], e16: Eq[F[A17]], e17: Eq[F[A18]], e18: Eq[F[A19]], e19: Eq[F[A20]], e20: Eq[F[A21]], e21: Eq[F[A22]]): Eq[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22].deriving[Eq, F].divide

  implicit def cop22Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit e0: Eq[F[A1]], e1: Eq[F[A2]], e2: Eq[F[A3]], e3: Eq[F[A4]], e4: Eq[F[A5]], e5: Eq[F[A6]], e6: Eq[F[A7]], e7: Eq[F[A8]], e8: Eq[F[A9]], e9: Eq[F[A10]], e10: Eq[F[A11]], e11: Eq[F[A12]], e12: Eq[F[A13]], e13: Eq[F[A14]], e14: Eq[F[A15]], e15: Eq[F[A16]], e16: Eq[F[A17]], e17: Eq[F[A18]], e18: Eq[F[A19]], e19: Eq[F[A20]], e20: Eq[F[A21]], e21: Eq[F[A22]]): Eq[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
    AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22].deriving[Eq, F].choose

}

object Prod2Test extends Properties("Prod2") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod2[?[_], String, Int], Applicative])

  include(MonoidTests[Prod2[Option, String, Int]].monoid.all)

  include(LensTests[Prod2[Option, String, Int], Option[String]](
    Prod2.Prod2Lens0[Option, String, Int]).all, "0.")

  include(LensTests[Prod2[Option, String, Int], Option[Int]](
    Prod2.Prod2Lens1[Option, String, Int]).all, "1.")

}

object Cop2Test extends Properties("Cop2") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop2[?[_], String, Int], Functor])
}

object Prod3Test extends Properties("Prod3") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod3[?[_], String, Int, String], Applicative])

  include(MonoidTests[Prod3[Option, String, Int, String]].monoid.all)

  include(LensTests[Prod3[Option, String, Int, String], Option[String]](
    Prod3.Prod3Lens0[Option, String, Int, String]).all, "0.")

  include(LensTests[Prod3[Option, String, Int, String], Option[Int]](
    Prod3.Prod3Lens1[Option, String, Int, String]).all, "1.")

  include(LensTests[Prod3[Option, String, Int, String], Option[String]](
    Prod3.Prod3Lens2[Option, String, Int, String]).all, "2.")

}

object Cop3Test extends Properties("Cop3") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop3[?[_], String, Int, String], Functor])
}

object Prod4Test extends Properties("Prod4") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod4[?[_], String, Int, String, Int], Applicative])

  include(MonoidTests[Prod4[Option, String, Int, String, Int]].monoid.all)

  include(LensTests[Prod4[Option, String, Int, String, Int], Option[String]](
    Prod4.Prod4Lens0[Option, String, Int, String, Int]).all, "0.")

  include(LensTests[Prod4[Option, String, Int, String, Int], Option[Int]](
    Prod4.Prod4Lens1[Option, String, Int, String, Int]).all, "1.")

  include(LensTests[Prod4[Option, String, Int, String, Int], Option[String]](
    Prod4.Prod4Lens2[Option, String, Int, String, Int]).all, "2.")

  include(LensTests[Prod4[Option, String, Int, String, Int], Option[Int]](
    Prod4.Prod4Lens3[Option, String, Int, String, Int]).all, "3.")

}

object Cop4Test extends Properties("Cop4") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop4[?[_], String, Int, String, Int], Functor])
}

object Prod5Test extends Properties("Prod5") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod5[?[_], String, Int, String, Int, String], Applicative])

  include(MonoidTests[Prod5[Option, String, Int, String, Int, String]].monoid.all)

  include(LensTests[Prod5[Option, String, Int, String, Int, String], Option[String]](
    Prod5.Prod5Lens0[Option, String, Int, String, Int, String]).all, "0.")

  include(LensTests[Prod5[Option, String, Int, String, Int, String], Option[Int]](
    Prod5.Prod5Lens1[Option, String, Int, String, Int, String]).all, "1.")

  include(LensTests[Prod5[Option, String, Int, String, Int, String], Option[String]](
    Prod5.Prod5Lens2[Option, String, Int, String, Int, String]).all, "2.")

  include(LensTests[Prod5[Option, String, Int, String, Int, String], Option[Int]](
    Prod5.Prod5Lens3[Option, String, Int, String, Int, String]).all, "3.")

  include(LensTests[Prod5[Option, String, Int, String, Int, String], Option[String]](
    Prod5.Prod5Lens4[Option, String, Int, String, Int, String]).all, "4.")

}

object Cop5Test extends Properties("Cop5") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop5[?[_], String, Int, String, Int, String], Functor])
}

object Prod6Test extends Properties("Prod6") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod6[?[_], String, Int, String, Int, String, Int], Applicative])

  include(MonoidTests[Prod6[Option, String, Int, String, Int, String, Int]].monoid.all)

  include(LensTests[Prod6[Option, String, Int, String, Int, String, Int], Option[String]](
    Prod6.Prod6Lens0[Option, String, Int, String, Int, String, Int]).all, "0.")

  include(LensTests[Prod6[Option, String, Int, String, Int, String, Int], Option[Int]](
    Prod6.Prod6Lens1[Option, String, Int, String, Int, String, Int]).all, "1.")

  include(LensTests[Prod6[Option, String, Int, String, Int, String, Int], Option[String]](
    Prod6.Prod6Lens2[Option, String, Int, String, Int, String, Int]).all, "2.")

  include(LensTests[Prod6[Option, String, Int, String, Int, String, Int], Option[Int]](
    Prod6.Prod6Lens3[Option, String, Int, String, Int, String, Int]).all, "3.")

  include(LensTests[Prod6[Option, String, Int, String, Int, String, Int], Option[String]](
    Prod6.Prod6Lens4[Option, String, Int, String, Int, String, Int]).all, "4.")

  include(LensTests[Prod6[Option, String, Int, String, Int, String, Int], Option[Int]](
    Prod6.Prod6Lens5[Option, String, Int, String, Int, String, Int]).all, "5.")

}

object Cop6Test extends Properties("Cop6") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop6[?[_], String, Int, String, Int, String, Int], Functor])
}

object Prod7Test extends Properties("Prod7") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod7[?[_], String, Int, String, Int, String, Int, String], Applicative])

  include(MonoidTests[Prod7[Option, String, Int, String, Int, String, Int, String]].monoid.all)

  include(LensTests[Prod7[Option, String, Int, String, Int, String, Int, String], Option[String]](
    Prod7.Prod7Lens0[Option, String, Int, String, Int, String, Int, String]).all, "0.")

  include(LensTests[Prod7[Option, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod7.Prod7Lens1[Option, String, Int, String, Int, String, Int, String]).all, "1.")

  include(LensTests[Prod7[Option, String, Int, String, Int, String, Int, String], Option[String]](
    Prod7.Prod7Lens2[Option, String, Int, String, Int, String, Int, String]).all, "2.")

  include(LensTests[Prod7[Option, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod7.Prod7Lens3[Option, String, Int, String, Int, String, Int, String]).all, "3.")

  include(LensTests[Prod7[Option, String, Int, String, Int, String, Int, String], Option[String]](
    Prod7.Prod7Lens4[Option, String, Int, String, Int, String, Int, String]).all, "4.")

  include(LensTests[Prod7[Option, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod7.Prod7Lens5[Option, String, Int, String, Int, String, Int, String]).all, "5.")

  include(LensTests[Prod7[Option, String, Int, String, Int, String, Int, String], Option[String]](
    Prod7.Prod7Lens6[Option, String, Int, String, Int, String, Int, String]).all, "6.")

}

object Cop7Test extends Properties("Cop7") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop7[?[_], String, Int, String, Int, String, Int, String], Functor])
}

object Prod8Test extends Properties("Prod8") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod8[?[_], String, Int, String, Int, String, Int, String, Int], Applicative])

  include(MonoidTests[Prod8[Option, String, Int, String, Int, String, Int, String, Int]].monoid.all)

  include(LensTests[Prod8[Option, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod8.Prod8Lens0[Option, String, Int, String, Int, String, Int, String, Int]).all, "0.")

  include(LensTests[Prod8[Option, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod8.Prod8Lens1[Option, String, Int, String, Int, String, Int, String, Int]).all, "1.")

  include(LensTests[Prod8[Option, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod8.Prod8Lens2[Option, String, Int, String, Int, String, Int, String, Int]).all, "2.")

  include(LensTests[Prod8[Option, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod8.Prod8Lens3[Option, String, Int, String, Int, String, Int, String, Int]).all, "3.")

  include(LensTests[Prod8[Option, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod8.Prod8Lens4[Option, String, Int, String, Int, String, Int, String, Int]).all, "4.")

  include(LensTests[Prod8[Option, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod8.Prod8Lens5[Option, String, Int, String, Int, String, Int, String, Int]).all, "5.")

  include(LensTests[Prod8[Option, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod8.Prod8Lens6[Option, String, Int, String, Int, String, Int, String, Int]).all, "6.")

  include(LensTests[Prod8[Option, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod8.Prod8Lens7[Option, String, Int, String, Int, String, Int, String, Int]).all, "7.")

}

object Cop8Test extends Properties("Cop8") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop8[?[_], String, Int, String, Int, String, Int, String, Int], Functor])
}

object Prod9Test extends Properties("Prod9") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod9[?[_], String, Int, String, Int, String, Int, String, Int, String], Applicative])

  include(MonoidTests[Prod9[Option, String, Int, String, Int, String, Int, String, Int, String]].monoid.all)

  include(LensTests[Prod9[Option, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod9.Prod9Lens0[Option, String, Int, String, Int, String, Int, String, Int, String]).all, "0.")

  include(LensTests[Prod9[Option, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod9.Prod9Lens1[Option, String, Int, String, Int, String, Int, String, Int, String]).all, "1.")

  include(LensTests[Prod9[Option, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod9.Prod9Lens2[Option, String, Int, String, Int, String, Int, String, Int, String]).all, "2.")

  include(LensTests[Prod9[Option, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod9.Prod9Lens3[Option, String, Int, String, Int, String, Int, String, Int, String]).all, "3.")

  include(LensTests[Prod9[Option, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod9.Prod9Lens4[Option, String, Int, String, Int, String, Int, String, Int, String]).all, "4.")

  include(LensTests[Prod9[Option, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod9.Prod9Lens5[Option, String, Int, String, Int, String, Int, String, Int, String]).all, "5.")

  include(LensTests[Prod9[Option, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod9.Prod9Lens6[Option, String, Int, String, Int, String, Int, String, Int, String]).all, "6.")

  include(LensTests[Prod9[Option, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod9.Prod9Lens7[Option, String, Int, String, Int, String, Int, String, Int, String]).all, "7.")

  include(LensTests[Prod9[Option, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod9.Prod9Lens8[Option, String, Int, String, Int, String, Int, String, Int, String]).all, "8.")

}

object Cop9Test extends Properties("Cop9") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop9[?[_], String, Int, String, Int, String, Int, String, Int, String], Functor])
}

object Prod10Test extends Properties("Prod10") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod10[?[_], String, Int, String, Int, String, Int, String, Int, String, Int], Applicative])

  include(MonoidTests[Prod10[Option, String, Int, String, Int, String, Int, String, Int, String, Int]].monoid.all)

  include(LensTests[Prod10[Option, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod10.Prod10Lens0[Option, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "0.")

  include(LensTests[Prod10[Option, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod10.Prod10Lens1[Option, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "1.")

  include(LensTests[Prod10[Option, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod10.Prod10Lens2[Option, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "2.")

  include(LensTests[Prod10[Option, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod10.Prod10Lens3[Option, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "3.")

  include(LensTests[Prod10[Option, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod10.Prod10Lens4[Option, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "4.")

  include(LensTests[Prod10[Option, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod10.Prod10Lens5[Option, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "5.")

  include(LensTests[Prod10[Option, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod10.Prod10Lens6[Option, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "6.")

  include(LensTests[Prod10[Option, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod10.Prod10Lens7[Option, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "7.")

  include(LensTests[Prod10[Option, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod10.Prod10Lens8[Option, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "8.")

  include(LensTests[Prod10[Option, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod10.Prod10Lens9[Option, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "9.")

}

object Cop10Test extends Properties("Cop10") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop10[?[_], String, Int, String, Int, String, Int, String, Int, String, Int], Functor])
}

object Prod11Test extends Properties("Prod11") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod11[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String], Applicative])

  include(MonoidTests[Prod11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String]].monoid.all)

  include(LensTests[Prod11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod11.Prod11Lens0[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "0.")

  include(LensTests[Prod11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod11.Prod11Lens1[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "1.")

  include(LensTests[Prod11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod11.Prod11Lens2[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "2.")

  include(LensTests[Prod11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod11.Prod11Lens3[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "3.")

  include(LensTests[Prod11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod11.Prod11Lens4[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "4.")

  include(LensTests[Prod11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod11.Prod11Lens5[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "5.")

  include(LensTests[Prod11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod11.Prod11Lens6[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "6.")

  include(LensTests[Prod11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod11.Prod11Lens7[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "7.")

  include(LensTests[Prod11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod11.Prod11Lens8[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "8.")

  include(LensTests[Prod11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod11.Prod11Lens9[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "9.")

  include(LensTests[Prod11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod11.Prod11Lens10[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "10.")

}

object Cop11Test extends Properties("Cop11") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop11[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String], Functor])
}

object Prod12Test extends Properties("Prod12") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod12[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Applicative])

  include(MonoidTests[Prod12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]].monoid.all)

  include(LensTests[Prod12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod12.Prod12Lens0[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "0.")

  include(LensTests[Prod12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod12.Prod12Lens1[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "1.")

  include(LensTests[Prod12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod12.Prod12Lens2[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "2.")

  include(LensTests[Prod12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod12.Prod12Lens3[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "3.")

  include(LensTests[Prod12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod12.Prod12Lens4[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "4.")

  include(LensTests[Prod12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod12.Prod12Lens5[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "5.")

  include(LensTests[Prod12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod12.Prod12Lens6[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "6.")

  include(LensTests[Prod12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod12.Prod12Lens7[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "7.")

  include(LensTests[Prod12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod12.Prod12Lens8[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "8.")

  include(LensTests[Prod12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod12.Prod12Lens9[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "9.")

  include(LensTests[Prod12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod12.Prod12Lens10[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "10.")

  include(LensTests[Prod12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod12.Prod12Lens11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "11.")

}

object Cop12Test extends Properties("Cop12") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop12[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Functor])
}

object Prod13Test extends Properties("Prod13") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod13[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Applicative])

  include(MonoidTests[Prod13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]].monoid.all)

  include(LensTests[Prod13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod13.Prod13Lens0[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "0.")

  include(LensTests[Prod13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod13.Prod13Lens1[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "1.")

  include(LensTests[Prod13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod13.Prod13Lens2[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "2.")

  include(LensTests[Prod13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod13.Prod13Lens3[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "3.")

  include(LensTests[Prod13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod13.Prod13Lens4[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "4.")

  include(LensTests[Prod13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod13.Prod13Lens5[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "5.")

  include(LensTests[Prod13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod13.Prod13Lens6[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "6.")

  include(LensTests[Prod13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod13.Prod13Lens7[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "7.")

  include(LensTests[Prod13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod13.Prod13Lens8[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "8.")

  include(LensTests[Prod13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod13.Prod13Lens9[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "9.")

  include(LensTests[Prod13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod13.Prod13Lens10[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "10.")

  include(LensTests[Prod13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod13.Prod13Lens11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "11.")

  include(LensTests[Prod13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod13.Prod13Lens12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "12.")

}

object Cop13Test extends Properties("Cop13") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop13[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Functor])
}

object Prod14Test extends Properties("Prod14") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod14[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Applicative])

  include(MonoidTests[Prod14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]].monoid.all)

  include(LensTests[Prod14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod14.Prod14Lens0[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "0.")

  include(LensTests[Prod14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod14.Prod14Lens1[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "1.")

  include(LensTests[Prod14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod14.Prod14Lens2[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "2.")

  include(LensTests[Prod14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod14.Prod14Lens3[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "3.")

  include(LensTests[Prod14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod14.Prod14Lens4[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "4.")

  include(LensTests[Prod14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod14.Prod14Lens5[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "5.")

  include(LensTests[Prod14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod14.Prod14Lens6[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "6.")

  include(LensTests[Prod14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod14.Prod14Lens7[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "7.")

  include(LensTests[Prod14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod14.Prod14Lens8[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "8.")

  include(LensTests[Prod14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod14.Prod14Lens9[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "9.")

  include(LensTests[Prod14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod14.Prod14Lens10[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "10.")

  include(LensTests[Prod14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod14.Prod14Lens11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "11.")

  include(LensTests[Prod14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod14.Prod14Lens12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "12.")

  include(LensTests[Prod14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod14.Prod14Lens13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "13.")

}

object Cop14Test extends Properties("Cop14") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop14[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Functor])
}

object Prod15Test extends Properties("Prod15") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod15[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Applicative])

  include(MonoidTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]].monoid.all)

  include(LensTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod15.Prod15Lens0[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "0.")

  include(LensTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod15.Prod15Lens1[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "1.")

  include(LensTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod15.Prod15Lens2[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "2.")

  include(LensTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod15.Prod15Lens3[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "3.")

  include(LensTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod15.Prod15Lens4[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "4.")

  include(LensTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod15.Prod15Lens5[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "5.")

  include(LensTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod15.Prod15Lens6[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "6.")

  include(LensTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod15.Prod15Lens7[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "7.")

  include(LensTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod15.Prod15Lens8[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "8.")

  include(LensTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod15.Prod15Lens9[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "9.")

  include(LensTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod15.Prod15Lens10[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "10.")

  include(LensTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod15.Prod15Lens11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "11.")

  include(LensTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod15.Prod15Lens12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "12.")

  include(LensTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod15.Prod15Lens13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "13.")

  include(LensTests[Prod15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod15.Prod15Lens14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "14.")

}

object Cop15Test extends Properties("Cop15") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop15[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Functor])
}

object Prod16Test extends Properties("Prod16") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod16[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Applicative])

  include(MonoidTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]].monoid.all)

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod16.Prod16Lens0[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "0.")

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod16.Prod16Lens1[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "1.")

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod16.Prod16Lens2[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "2.")

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod16.Prod16Lens3[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "3.")

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod16.Prod16Lens4[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "4.")

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod16.Prod16Lens5[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "5.")

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod16.Prod16Lens6[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "6.")

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod16.Prod16Lens7[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "7.")

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod16.Prod16Lens8[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "8.")

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod16.Prod16Lens9[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "9.")

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod16.Prod16Lens10[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "10.")

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod16.Prod16Lens11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "11.")

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod16.Prod16Lens12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "12.")

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod16.Prod16Lens13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "13.")

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod16.Prod16Lens14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "14.")

  include(LensTests[Prod16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod16.Prod16Lens15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "15.")

}

object Cop16Test extends Properties("Cop16") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop16[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Functor])
}

object Prod17Test extends Properties("Prod17") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod17[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Applicative])

  include(MonoidTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]].monoid.all)

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod17.Prod17Lens0[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "0.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod17.Prod17Lens1[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "1.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod17.Prod17Lens2[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "2.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod17.Prod17Lens3[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "3.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod17.Prod17Lens4[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "4.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod17.Prod17Lens5[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "5.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod17.Prod17Lens6[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "6.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod17.Prod17Lens7[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "7.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod17.Prod17Lens8[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "8.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod17.Prod17Lens9[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "9.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod17.Prod17Lens10[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "10.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod17.Prod17Lens11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "11.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod17.Prod17Lens12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "12.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod17.Prod17Lens13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "13.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod17.Prod17Lens14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "14.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod17.Prod17Lens15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "15.")

  include(LensTests[Prod17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod17.Prod17Lens16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "16.")

}

object Cop17Test extends Properties("Cop17") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop17[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Functor])
}

object Prod18Test extends Properties("Prod18") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod18[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Applicative])

  include(MonoidTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]].monoid.all)

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod18.Prod18Lens0[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "0.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod18.Prod18Lens1[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "1.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod18.Prod18Lens2[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "2.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod18.Prod18Lens3[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "3.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod18.Prod18Lens4[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "4.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod18.Prod18Lens5[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "5.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod18.Prod18Lens6[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "6.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod18.Prod18Lens7[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "7.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod18.Prod18Lens8[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "8.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod18.Prod18Lens9[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "9.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod18.Prod18Lens10[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "10.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod18.Prod18Lens11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "11.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod18.Prod18Lens12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "12.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod18.Prod18Lens13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "13.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod18.Prod18Lens14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "14.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod18.Prod18Lens15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "15.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod18.Prod18Lens16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "16.")

  include(LensTests[Prod18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod18.Prod18Lens17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "17.")

}

object Cop18Test extends Properties("Cop18") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop18[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Functor])
}

object Prod19Test extends Properties("Prod19") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod19[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Applicative])

  include(MonoidTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]].monoid.all)

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod19.Prod19Lens0[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "0.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod19.Prod19Lens1[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "1.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod19.Prod19Lens2[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "2.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod19.Prod19Lens3[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "3.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod19.Prod19Lens4[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "4.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod19.Prod19Lens5[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "5.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod19.Prod19Lens6[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "6.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod19.Prod19Lens7[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "7.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod19.Prod19Lens8[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "8.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod19.Prod19Lens9[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "9.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod19.Prod19Lens10[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "10.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod19.Prod19Lens11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "11.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod19.Prod19Lens12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "12.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod19.Prod19Lens13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "13.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod19.Prod19Lens14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "14.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod19.Prod19Lens15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "15.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod19.Prod19Lens16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "16.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod19.Prod19Lens17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "17.")

  include(LensTests[Prod19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod19.Prod19Lens18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "18.")

}

object Cop19Test extends Properties("Cop19") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop19[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Functor])
}

object Prod20Test extends Properties("Prod20") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod20[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Applicative])

  include(MonoidTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]].monoid.all)

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod20.Prod20Lens0[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "0.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod20.Prod20Lens1[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "1.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod20.Prod20Lens2[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "2.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod20.Prod20Lens3[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "3.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod20.Prod20Lens4[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "4.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod20.Prod20Lens5[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "5.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod20.Prod20Lens6[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "6.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod20.Prod20Lens7[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "7.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod20.Prod20Lens8[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "8.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod20.Prod20Lens9[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "9.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod20.Prod20Lens10[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "10.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod20.Prod20Lens11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "11.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod20.Prod20Lens12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "12.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod20.Prod20Lens13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "13.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod20.Prod20Lens14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "14.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod20.Prod20Lens15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "15.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod20.Prod20Lens16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "16.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod20.Prod20Lens17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "17.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod20.Prod20Lens18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "18.")

  include(LensTests[Prod20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod20.Prod20Lens19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "19.")

}

object Cop20Test extends Properties("Cop20") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop20[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Functor])
}

object Prod21Test extends Properties("Prod21") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod21[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Applicative])

  include(MonoidTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]].monoid.all)

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod21.Prod21Lens0[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "0.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod21.Prod21Lens1[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "1.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod21.Prod21Lens2[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "2.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod21.Prod21Lens3[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "3.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod21.Prod21Lens4[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "4.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod21.Prod21Lens5[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "5.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod21.Prod21Lens6[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "6.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod21.Prod21Lens7[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "7.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod21.Prod21Lens8[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "8.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod21.Prod21Lens9[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "9.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod21.Prod21Lens10[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "10.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod21.Prod21Lens11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "11.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod21.Prod21Lens12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "12.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod21.Prod21Lens13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "13.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod21.Prod21Lens14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "14.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod21.Prod21Lens15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "15.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod21.Prod21Lens16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "16.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod21.Prod21Lens17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "17.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod21.Prod21Lens18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "18.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[Int]](
    Prod21.Prod21Lens19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "19.")

  include(LensTests[Prod21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Option[String]](
    Prod21.Prod21Lens20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String]).all, "20.")

}

object Cop21Test extends Properties("Cop21") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop21[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String], Functor])
}

object Prod22Test extends Properties("Prod22") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod22[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Applicative])

  include(MonoidTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]].monoid.all)

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod22.Prod22Lens0[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "0.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod22.Prod22Lens1[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "1.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod22.Prod22Lens2[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "2.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod22.Prod22Lens3[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "3.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod22.Prod22Lens4[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "4.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod22.Prod22Lens5[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "5.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod22.Prod22Lens6[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "6.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod22.Prod22Lens7[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "7.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod22.Prod22Lens8[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "8.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod22.Prod22Lens9[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "9.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod22.Prod22Lens10[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "10.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod22.Prod22Lens11[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "11.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod22.Prod22Lens12[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "12.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod22.Prod22Lens13[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "13.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod22.Prod22Lens14[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "14.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod22.Prod22Lens15[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "15.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod22.Prod22Lens16[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "16.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod22.Prod22Lens17[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "17.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod22.Prod22Lens18[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "18.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod22.Prod22Lens19[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "19.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[String]](
    Prod22.Prod22Lens20[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "20.")

  include(LensTests[Prod22[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Option[Int]](
    Prod22.Prod22Lens21[Option, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int]).all, "21.")

}

object Cop22Test extends Properties("Cop22") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop22[?[_], String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int, String, Int], Functor])
}

