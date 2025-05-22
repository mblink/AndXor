package andxor


import andxor.scalacheck.given
import org.scalacheck.{Arbitrary, Properties}
import cats.{Apply, Eq, Functor}
import cats.instances.int.*
import cats.instances.list.*
import cats.instances.option.*
import cats.instances.string.*
import cats.instances.tuple.*
import cats.instances.vector.*
import cats.kernel.laws.discipline.MonoidTests
import monocle.law.discipline.LensTests


object arbitrary {
  
    implicit def prod1Arb[F[_], A1](implicit
      a1: Arbitrary[F[A1]]
    ): Arbitrary[F[A1] *: EmptyTuple] =
      AndXor[A1].deriving[Arbitrary, F].apply

    implicit def cop1Arb[F[_], A1](implicit
      a1: Arbitrary[F[A1]]
    ): Arbitrary[F[A1]] =
      AndXor[A1].deriving[Arbitrary, F].alt

    implicit def prod1Eq[F[_], A1](implicit
      e1: Eq[F[A1]]
    ): Eq[F[A1] *: EmptyTuple] =
      AndXor[A1].deriving[Eq, F].divide

    implicit def cop1Eq[F[_], A1](implicit
      e1: Eq[F[A1]]
    ): Eq[F[A1]] =
      AndXor[A1].deriving[Eq, F].choose


    implicit def prod2Arb[F[_], A1, A2](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]]
    ): Arbitrary[F[A1] *: F[A2] *: EmptyTuple] =
      AndXor[A1, A2].deriving[Arbitrary, F].apply

    implicit def cop2Arb[F[_], A1, A2](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]]
    ): Arbitrary[F[A1] |: F[A2]] =
      AndXor[A1, A2].deriving[Arbitrary, F].alt

    implicit def prod2Eq[F[_], A1, A2](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]]
    ): Eq[F[A1] *: F[A2] *: EmptyTuple] =
      AndXor[A1, A2].deriving[Eq, F].divide

    implicit def cop2Eq[F[_], A1, A2](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]]
    ): Eq[F[A1] |: F[A2]] =
      AndXor[A1, A2].deriving[Eq, F].choose


    implicit def prod3Arb[F[_], A1, A2, A3](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: EmptyTuple] =
      AndXor[A1, A2, A3].deriving[Arbitrary, F].apply

    implicit def cop3Arb[F[_], A1, A2, A3](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3]] =
      AndXor[A1, A2, A3].deriving[Arbitrary, F].alt

    implicit def prod3Eq[F[_], A1, A2, A3](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: EmptyTuple] =
      AndXor[A1, A2, A3].deriving[Eq, F].divide

    implicit def cop3Eq[F[_], A1, A2, A3](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]]
    ): Eq[F[A1] |: F[A2] |: F[A3]] =
      AndXor[A1, A2, A3].deriving[Eq, F].choose


    implicit def prod4Arb[F[_], A1, A2, A3, A4](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4].deriving[Arbitrary, F].apply

    implicit def cop4Arb[F[_], A1, A2, A3, A4](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4]] =
      AndXor[A1, A2, A3, A4].deriving[Arbitrary, F].alt

    implicit def prod4Eq[F[_], A1, A2, A3, A4](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4].deriving[Eq, F].divide

    implicit def cop4Eq[F[_], A1, A2, A3, A4](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4]] =
      AndXor[A1, A2, A3, A4].deriving[Eq, F].choose


    implicit def prod5Arb[F[_], A1, A2, A3, A4, A5](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5].deriving[Arbitrary, F].apply

    implicit def cop5Arb[F[_], A1, A2, A3, A4, A5](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5]] =
      AndXor[A1, A2, A3, A4, A5].deriving[Arbitrary, F].alt

    implicit def prod5Eq[F[_], A1, A2, A3, A4, A5](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5].deriving[Eq, F].divide

    implicit def cop5Eq[F[_], A1, A2, A3, A4, A5](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5]] =
      AndXor[A1, A2, A3, A4, A5].deriving[Eq, F].choose


    implicit def prod6Arb[F[_], A1, A2, A3, A4, A5, A6](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6].deriving[Arbitrary, F].apply

    implicit def cop6Arb[F[_], A1, A2, A3, A4, A5, A6](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6]] =
      AndXor[A1, A2, A3, A4, A5, A6].deriving[Arbitrary, F].alt

    implicit def prod6Eq[F[_], A1, A2, A3, A4, A5, A6](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6].deriving[Eq, F].divide

    implicit def cop6Eq[F[_], A1, A2, A3, A4, A5, A6](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6]] =
      AndXor[A1, A2, A3, A4, A5, A6].deriving[Eq, F].choose


    implicit def prod7Arb[F[_], A1, A2, A3, A4, A5, A6, A7](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7].deriving[Arbitrary, F].apply

    implicit def cop7Arb[F[_], A1, A2, A3, A4, A5, A6, A7](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7].deriving[Arbitrary, F].alt

    implicit def prod7Eq[F[_], A1, A2, A3, A4, A5, A6, A7](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7].deriving[Eq, F].divide

    implicit def cop7Eq[F[_], A1, A2, A3, A4, A5, A6, A7](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7].deriving[Eq, F].choose


    implicit def prod8Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8].deriving[Arbitrary, F].apply

    implicit def cop8Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8].deriving[Arbitrary, F].alt

    implicit def prod8Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8].deriving[Eq, F].divide

    implicit def cop8Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8].deriving[Eq, F].choose


    implicit def prod9Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9].deriving[Arbitrary, F].apply

    implicit def cop9Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9].deriving[Arbitrary, F].alt

    implicit def prod9Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9].deriving[Eq, F].divide

    implicit def cop9Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9].deriving[Eq, F].choose


    implicit def prod10Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10].deriving[Arbitrary, F].apply

    implicit def cop10Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10].deriving[Arbitrary, F].alt

    implicit def prod10Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10].deriving[Eq, F].divide

    implicit def cop10Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10].deriving[Eq, F].choose


    implicit def prod11Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11].deriving[Arbitrary, F].apply

    implicit def cop11Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11].deriving[Arbitrary, F].alt

    implicit def prod11Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11].deriving[Eq, F].divide

    implicit def cop11Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11].deriving[Eq, F].choose


    implicit def prod12Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].deriving[Arbitrary, F].apply

    implicit def cop12Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].deriving[Arbitrary, F].alt

    implicit def prod12Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].deriving[Eq, F].divide

    implicit def cop12Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12].deriving[Eq, F].choose


    implicit def prod13Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13].deriving[Arbitrary, F].apply

    implicit def cop13Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13].deriving[Arbitrary, F].alt

    implicit def prod13Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13].deriving[Eq, F].divide

    implicit def cop13Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13].deriving[Eq, F].choose


    implicit def prod14Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14].deriving[Arbitrary, F].apply

    implicit def cop14Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14].deriving[Arbitrary, F].alt

    implicit def prod14Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14].deriving[Eq, F].divide

    implicit def cop14Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14].deriving[Eq, F].choose


    implicit def prod15Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15].deriving[Arbitrary, F].apply

    implicit def cop15Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15].deriving[Arbitrary, F].alt

    implicit def prod15Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15].deriving[Eq, F].divide

    implicit def cop15Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15].deriving[Eq, F].choose


    implicit def prod16Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]],
      a16: Arbitrary[F[A16]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: F[A16] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16].deriving[Arbitrary, F].apply

    implicit def cop16Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]],
      a16: Arbitrary[F[A16]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15] |: F[A16]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16].deriving[Arbitrary, F].alt

    implicit def prod16Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]],
      e16: Eq[F[A16]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: F[A16] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16].deriving[Eq, F].divide

    implicit def cop16Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]],
      e16: Eq[F[A16]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15] |: F[A16]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16].deriving[Eq, F].choose


    implicit def prod17Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]],
      a16: Arbitrary[F[A16]],
      a17: Arbitrary[F[A17]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: F[A16] *: F[A17] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17].deriving[Arbitrary, F].apply

    implicit def cop17Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]],
      a16: Arbitrary[F[A16]],
      a17: Arbitrary[F[A17]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15] |: F[A16] |: F[A17]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17].deriving[Arbitrary, F].alt

    implicit def prod17Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]],
      e16: Eq[F[A16]],
      e17: Eq[F[A17]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: F[A16] *: F[A17] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17].deriving[Eq, F].divide

    implicit def cop17Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]],
      e16: Eq[F[A16]],
      e17: Eq[F[A17]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15] |: F[A16] |: F[A17]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17].deriving[Eq, F].choose


    implicit def prod18Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]],
      a16: Arbitrary[F[A16]],
      a17: Arbitrary[F[A17]],
      a18: Arbitrary[F[A18]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: F[A16] *: F[A17] *: F[A18] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18].deriving[Arbitrary, F].apply

    implicit def cop18Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]],
      a16: Arbitrary[F[A16]],
      a17: Arbitrary[F[A17]],
      a18: Arbitrary[F[A18]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15] |: F[A16] |: F[A17] |: F[A18]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18].deriving[Arbitrary, F].alt

    implicit def prod18Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]],
      e16: Eq[F[A16]],
      e17: Eq[F[A17]],
      e18: Eq[F[A18]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: F[A16] *: F[A17] *: F[A18] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18].deriving[Eq, F].divide

    implicit def cop18Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]],
      e16: Eq[F[A16]],
      e17: Eq[F[A17]],
      e18: Eq[F[A18]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15] |: F[A16] |: F[A17] |: F[A18]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18].deriving[Eq, F].choose


    implicit def prod19Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]],
      a16: Arbitrary[F[A16]],
      a17: Arbitrary[F[A17]],
      a18: Arbitrary[F[A18]],
      a19: Arbitrary[F[A19]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: F[A16] *: F[A17] *: F[A18] *: F[A19] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19].deriving[Arbitrary, F].apply

    implicit def cop19Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]],
      a16: Arbitrary[F[A16]],
      a17: Arbitrary[F[A17]],
      a18: Arbitrary[F[A18]],
      a19: Arbitrary[F[A19]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15] |: F[A16] |: F[A17] |: F[A18] |: F[A19]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19].deriving[Arbitrary, F].alt

    implicit def prod19Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]],
      e16: Eq[F[A16]],
      e17: Eq[F[A17]],
      e18: Eq[F[A18]],
      e19: Eq[F[A19]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: F[A16] *: F[A17] *: F[A18] *: F[A19] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19].deriving[Eq, F].divide

    implicit def cop19Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]],
      e16: Eq[F[A16]],
      e17: Eq[F[A17]],
      e18: Eq[F[A18]],
      e19: Eq[F[A19]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15] |: F[A16] |: F[A17] |: F[A18] |: F[A19]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19].deriving[Eq, F].choose


    implicit def prod20Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]],
      a16: Arbitrary[F[A16]],
      a17: Arbitrary[F[A17]],
      a18: Arbitrary[F[A18]],
      a19: Arbitrary[F[A19]],
      a20: Arbitrary[F[A20]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: F[A16] *: F[A17] *: F[A18] *: F[A19] *: F[A20] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20].deriving[Arbitrary, F].apply

    implicit def cop20Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]],
      a16: Arbitrary[F[A16]],
      a17: Arbitrary[F[A17]],
      a18: Arbitrary[F[A18]],
      a19: Arbitrary[F[A19]],
      a20: Arbitrary[F[A20]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15] |: F[A16] |: F[A17] |: F[A18] |: F[A19] |: F[A20]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20].deriving[Arbitrary, F].alt

    implicit def prod20Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]],
      e16: Eq[F[A16]],
      e17: Eq[F[A17]],
      e18: Eq[F[A18]],
      e19: Eq[F[A19]],
      e20: Eq[F[A20]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: F[A16] *: F[A17] *: F[A18] *: F[A19] *: F[A20] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20].deriving[Eq, F].divide

    implicit def cop20Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]],
      e16: Eq[F[A16]],
      e17: Eq[F[A17]],
      e18: Eq[F[A18]],
      e19: Eq[F[A19]],
      e20: Eq[F[A20]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15] |: F[A16] |: F[A17] |: F[A18] |: F[A19] |: F[A20]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20].deriving[Eq, F].choose


    implicit def prod21Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]],
      a16: Arbitrary[F[A16]],
      a17: Arbitrary[F[A17]],
      a18: Arbitrary[F[A18]],
      a19: Arbitrary[F[A19]],
      a20: Arbitrary[F[A20]],
      a21: Arbitrary[F[A21]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: F[A16] *: F[A17] *: F[A18] *: F[A19] *: F[A20] *: F[A21] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21].deriving[Arbitrary, F].apply

    implicit def cop21Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]],
      a16: Arbitrary[F[A16]],
      a17: Arbitrary[F[A17]],
      a18: Arbitrary[F[A18]],
      a19: Arbitrary[F[A19]],
      a20: Arbitrary[F[A20]],
      a21: Arbitrary[F[A21]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15] |: F[A16] |: F[A17] |: F[A18] |: F[A19] |: F[A20] |: F[A21]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21].deriving[Arbitrary, F].alt

    implicit def prod21Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]],
      e16: Eq[F[A16]],
      e17: Eq[F[A17]],
      e18: Eq[F[A18]],
      e19: Eq[F[A19]],
      e20: Eq[F[A20]],
      e21: Eq[F[A21]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: F[A16] *: F[A17] *: F[A18] *: F[A19] *: F[A20] *: F[A21] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21].deriving[Eq, F].divide

    implicit def cop21Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]],
      e16: Eq[F[A16]],
      e17: Eq[F[A17]],
      e18: Eq[F[A18]],
      e19: Eq[F[A19]],
      e20: Eq[F[A20]],
      e21: Eq[F[A21]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15] |: F[A16] |: F[A17] |: F[A18] |: F[A19] |: F[A20] |: F[A21]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21].deriving[Eq, F].choose


    implicit def prod22Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]],
      a16: Arbitrary[F[A16]],
      a17: Arbitrary[F[A17]],
      a18: Arbitrary[F[A18]],
      a19: Arbitrary[F[A19]],
      a20: Arbitrary[F[A20]],
      a21: Arbitrary[F[A21]],
      a22: Arbitrary[F[A22]]
    ): Arbitrary[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: F[A16] *: F[A17] *: F[A18] *: F[A19] *: F[A20] *: F[A21] *: F[A22] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22].deriving[Arbitrary, F].apply

    implicit def cop22Arb[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit
      a1: Arbitrary[F[A1]],
      a2: Arbitrary[F[A2]],
      a3: Arbitrary[F[A3]],
      a4: Arbitrary[F[A4]],
      a5: Arbitrary[F[A5]],
      a6: Arbitrary[F[A6]],
      a7: Arbitrary[F[A7]],
      a8: Arbitrary[F[A8]],
      a9: Arbitrary[F[A9]],
      a10: Arbitrary[F[A10]],
      a11: Arbitrary[F[A11]],
      a12: Arbitrary[F[A12]],
      a13: Arbitrary[F[A13]],
      a14: Arbitrary[F[A14]],
      a15: Arbitrary[F[A15]],
      a16: Arbitrary[F[A16]],
      a17: Arbitrary[F[A17]],
      a18: Arbitrary[F[A18]],
      a19: Arbitrary[F[A19]],
      a20: Arbitrary[F[A20]],
      a21: Arbitrary[F[A21]],
      a22: Arbitrary[F[A22]]
    ): Arbitrary[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15] |: F[A16] |: F[A17] |: F[A18] |: F[A19] |: F[A20] |: F[A21] |: F[A22]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22].deriving[Arbitrary, F].alt

    implicit def prod22Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]],
      e16: Eq[F[A16]],
      e17: Eq[F[A17]],
      e18: Eq[F[A18]],
      e19: Eq[F[A19]],
      e20: Eq[F[A20]],
      e21: Eq[F[A21]],
      e22: Eq[F[A22]]
    ): Eq[F[A1] *: F[A2] *: F[A3] *: F[A4] *: F[A5] *: F[A6] *: F[A7] *: F[A8] *: F[A9] *: F[A10] *: F[A11] *: F[A12] *: F[A13] *: F[A14] *: F[A15] *: F[A16] *: F[A17] *: F[A18] *: F[A19] *: F[A20] *: F[A21] *: F[A22] *: EmptyTuple] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22].deriving[Eq, F].divide

    implicit def cop22Eq[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit
      e1: Eq[F[A1]],
      e2: Eq[F[A2]],
      e3: Eq[F[A3]],
      e4: Eq[F[A4]],
      e5: Eq[F[A5]],
      e6: Eq[F[A6]],
      e7: Eq[F[A7]],
      e8: Eq[F[A8]],
      e9: Eq[F[A9]],
      e10: Eq[F[A10]],
      e11: Eq[F[A11]],
      e12: Eq[F[A12]],
      e13: Eq[F[A13]],
      e14: Eq[F[A14]],
      e15: Eq[F[A15]],
      e16: Eq[F[A16]],
      e17: Eq[F[A17]],
      e18: Eq[F[A18]],
      e19: Eq[F[A19]],
      e20: Eq[F[A20]],
      e21: Eq[F[A21]],
      e22: Eq[F[A22]]
    ): Eq[F[A1] |: F[A2] |: F[A3] |: F[A4] |: F[A5] |: F[A6] |: F[A7] |: F[A8] |: F[A9] |: F[A10] |: F[A11] |: F[A12] |: F[A13] |: F[A14] |: F[A15] |: F[A16] |: F[A17] |: F[A18] |: F[A19] |: F[A20] |: F[A21] |: F[A22]] =
      AndXor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22].deriving[Eq, F].choose

}


object Prod1Test extends Properties("Prod1") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")
}

object Cop1Test extends Properties("Cop1") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String], Functor])
}


object Prod2Test extends Properties("Prod2") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")
}

object Cop2Test extends Properties("Cop2") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int], Functor])
}


object Prod3Test extends Properties("Prod3") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")
}

object Cop3Test extends Properties("Cop3") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String], Functor])
}


object Prod4Test extends Properties("Prod4") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")
}

object Cop4Test extends Properties("Cop4") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int], Functor])
}


object Prod5Test extends Properties("Prod5") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")
}

object Cop5Test extends Properties("Cop5") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String], Functor])
}


object Prod6Test extends Properties("Prod6") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")
}

object Cop6Test extends Properties("Cop6") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int], Functor])
}


object Prod7Test extends Properties("Prod7") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")
}

object Cop7Test extends Properties("Cop7") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String], Functor])
}


object Prod8Test extends Properties("Prod8") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "8.")
}

object Cop8Test extends Properties("Cop8") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int], Functor])
}


object Prod9Test extends Properties("Prod9") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "8.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "9.")
}

object Cop9Test extends Properties("Cop9") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String], Functor])
}


object Prod10Test extends Properties("Prod10") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "8.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "9.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "10.")
}

object Cop10Test extends Properties("Cop10") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int], Functor])
}


object Prod11Test extends Properties("Prod11") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "8.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "9.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "10.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "11.")
}

object Cop11Test extends Properties("Cop11") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String], Functor])
}


object Prod12Test extends Properties("Prod12") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "8.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "9.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "10.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "11.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "12.")
}

object Cop12Test extends Properties("Cop12") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int], Functor])
}


object Prod13Test extends Properties("Prod13") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "8.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "9.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "10.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "11.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "12.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "13.")
}

object Cop13Test extends Properties("Cop13") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String], Functor])
}


object Prod14Test extends Properties("Prod14") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "8.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "9.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "10.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "11.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "12.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "13.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "14.")
}

object Cop14Test extends Properties("Cop14") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int], Functor])
}


object Prod15Test extends Properties("Prod15") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "8.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "9.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "10.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "11.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "12.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "13.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "14.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "15.")
}

object Cop15Test extends Properties("Cop15") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String], Functor])
}


object Prod16Test extends Properties("Prod16") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "8.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "9.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "10.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "11.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "12.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "13.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "14.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "15.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "16.")
}

object Cop16Test extends Properties("Cop16") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int], Functor])
}


object Prod17Test extends Properties("Prod17") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "8.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "9.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "10.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "11.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "12.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "13.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "14.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "15.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "16.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "17.")
}

object Cop17Test extends Properties("Cop17") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String], Functor])
}


object Prod18Test extends Properties("Prod18") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "8.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "9.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "10.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "11.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "12.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "13.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "14.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "15.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "16.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "17.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "18.")
}

object Cop18Test extends Properties("Cop18") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int], Functor])
}


object Prod19Test extends Properties("Prod19") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "8.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "9.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "10.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "11.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "12.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "13.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "14.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "15.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "16.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "17.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "18.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "19.")
}

object Cop19Test extends Properties("Cop19") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String], Functor])
}


object Prod20Test extends Properties("Prod20") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "8.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "9.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "10.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "11.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "12.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "13.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "14.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "15.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "16.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "17.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "18.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "19.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "20.")
}

object Cop20Test extends Properties("Cop20") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int], Functor])
}


object Prod21Test extends Properties("Prod21") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "8.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "9.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "10.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "11.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "12.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "13.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "14.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "15.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "16.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "17.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "18.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "19.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[Int]]].asMonocle).all, "20.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: EmptyTuple, Option[String]]].asMonocle).all, "21.")
}

object Cop21Test extends Properties("Cop21") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String], Functor])
}


object Prod22Test extends Properties("Prod22") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: F[String] *: F[Int] *: EmptyTuple, Apply])

  include(MonoidTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple].monoid.all)

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "1.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "2.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "3.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "4.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "5.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "6.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "7.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "8.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "9.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "10.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "11.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "12.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "13.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "14.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "15.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "16.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "17.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "18.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "19.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "20.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[String]]].asMonocle).all, "21.")

  include(LensTests[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]](summon[Lens[Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: Option[String] *: Option[Int] *: EmptyTuple, Option[Int]]].asMonocle).all, "22.")
}

object Cop22Test extends Properties("Cop22") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int] |: F[String] |: F[Int], Functor])
}

