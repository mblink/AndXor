package andxor

import andxor.AndXorProperties.arbitrary._
import andxor.types._
import org.scalacheck.{Arbitrary, Properties}
import scalaz.{Applicative, Equal, Functor}
import scalaz.scalacheck.ScalazProperties.lens
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.std.vector._

object arbitrary {

  implicit def prod2Arb[F[_], A1, A2](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]]): Arbitrary[Prod2[F, A1, A2]] =
    AndXor[A1, A2].deriving[Arbitrary, F].apply

  implicit def cop2Arb[F[_], A1, A2](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]]): Arbitrary[Cop2[F, A1, A2]] =
    AndXor[A1, A2].deriving[Arbitrary, F].alt

  implicit def prod2Eq[F[_], A1, A2](implicit e0: Equal[F[A1]], e1: Equal[F[A2]]): Equal[Prod2[F, A1, A2]] =
    AndXor[A1, A2].deriving[Equal, F].divide

  implicit def cop2Eq[F[_], A1, A2](implicit e0: Equal[F[A1]], e1: Equal[F[A2]]): Equal[Cop2[F, A1, A2]] =
    AndXor[A1, A2].deriving[Equal, F].choose

  implicit def prod3Arb[F[_], A1, A2, A3](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]]): Arbitrary[Prod3[F, A1, A2, A3]] =
    AndXor[A1, A2, A3].deriving[Arbitrary, F].apply

  implicit def cop3Arb[F[_], A1, A2, A3](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]]): Arbitrary[Cop3[F, A1, A2, A3]] =
    AndXor[A1, A2, A3].deriving[Arbitrary, F].alt

  implicit def prod3Eq[F[_], A1, A2, A3](implicit e0: Equal[F[A1]], e1: Equal[F[A2]], e2: Equal[F[A3]]): Equal[Prod3[F, A1, A2, A3]] =
    AndXor[A1, A2, A3].deriving[Equal, F].divide

  implicit def cop3Eq[F[_], A1, A2, A3](implicit e0: Equal[F[A1]], e1: Equal[F[A2]], e2: Equal[F[A3]]): Equal[Cop3[F, A1, A2, A3]] =
    AndXor[A1, A2, A3].deriving[Equal, F].choose

  implicit def prod4Arb[F[_], A1, A2, A3, A4](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]]): Arbitrary[Prod4[F, A1, A2, A3, A4]] =
    AndXor[A1, A2, A3, A4].deriving[Arbitrary, F].apply

  implicit def cop4Arb[F[_], A1, A2, A3, A4](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]]): Arbitrary[Cop4[F, A1, A2, A3, A4]] =
    AndXor[A1, A2, A3, A4].deriving[Arbitrary, F].alt

  implicit def prod4Eq[F[_], A1, A2, A3, A4](implicit e0: Equal[F[A1]], e1: Equal[F[A2]], e2: Equal[F[A3]], e3: Equal[F[A4]]): Equal[Prod4[F, A1, A2, A3, A4]] =
    AndXor[A1, A2, A3, A4].deriving[Equal, F].divide

  implicit def cop4Eq[F[_], A1, A2, A3, A4](implicit e0: Equal[F[A1]], e1: Equal[F[A2]], e2: Equal[F[A3]], e3: Equal[F[A4]]): Equal[Cop4[F, A1, A2, A3, A4]] =
    AndXor[A1, A2, A3, A4].deriving[Equal, F].choose

  implicit def prod5Arb[F[_], A1, A2, A3, A4, A5](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]]): Arbitrary[Prod5[F, A1, A2, A3, A4, A5]] =
    AndXor[A1, A2, A3, A4, A5].deriving[Arbitrary, F].apply

  implicit def cop5Arb[F[_], A1, A2, A3, A4, A5](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]]): Arbitrary[Cop5[F, A1, A2, A3, A4, A5]] =
    AndXor[A1, A2, A3, A4, A5].deriving[Arbitrary, F].alt

  implicit def prod5Eq[F[_], A1, A2, A3, A4, A5](implicit e0: Equal[F[A1]], e1: Equal[F[A2]], e2: Equal[F[A3]], e3: Equal[F[A4]], e4: Equal[F[A5]]): Equal[Prod5[F, A1, A2, A3, A4, A5]] =
    AndXor[A1, A2, A3, A4, A5].deriving[Equal, F].divide

  implicit def cop5Eq[F[_], A1, A2, A3, A4, A5](implicit e0: Equal[F[A1]], e1: Equal[F[A2]], e2: Equal[F[A3]], e3: Equal[F[A4]], e4: Equal[F[A5]]): Equal[Cop5[F, A1, A2, A3, A4, A5]] =
    AndXor[A1, A2, A3, A4, A5].deriving[Equal, F].choose

  implicit def prod6Arb[F[_], A1, A2, A3, A4, A5, A6](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]]): Arbitrary[Prod6[F, A1, A2, A3, A4, A5, A6]] =
    AndXor[A1, A2, A3, A4, A5, A6].deriving[Arbitrary, F].apply

  implicit def cop6Arb[F[_], A1, A2, A3, A4, A5, A6](implicit a0: Arbitrary[F[A1]], a1: Arbitrary[F[A2]], a2: Arbitrary[F[A3]], a3: Arbitrary[F[A4]], a4: Arbitrary[F[A5]], a5: Arbitrary[F[A6]]): Arbitrary[Cop6[F, A1, A2, A3, A4, A5, A6]] =
    AndXor[A1, A2, A3, A4, A5, A6].deriving[Arbitrary, F].alt

  implicit def prod6Eq[F[_], A1, A2, A3, A4, A5, A6](implicit e0: Equal[F[A1]], e1: Equal[F[A2]], e2: Equal[F[A3]], e3: Equal[F[A4]], e4: Equal[F[A5]], e5: Equal[F[A6]]): Equal[Prod6[F, A1, A2, A3, A4, A5, A6]] =
    AndXor[A1, A2, A3, A4, A5, A6].deriving[Equal, F].divide

  implicit def cop6Eq[F[_], A1, A2, A3, A4, A5, A6](implicit e0: Equal[F[A1]], e1: Equal[F[A2]], e2: Equal[F[A3]], e3: Equal[F[A4]], e4: Equal[F[A5]], e5: Equal[F[A6]]): Equal[Cop6[F, A1, A2, A3, A4, A5, A6]] =
    AndXor[A1, A2, A3, A4, A5, A6].deriving[Equal, F].choose

}

object Prod2Test extends Properties("Prod2") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod2[?[_], String, Int], Applicative])

  include(lens.laws[Prod2[Option, String, Int], Option[String]](
    Prod2.Prod2Lens0[Option, String, Int]), "0.")

  include(lens.laws[Prod2[Option, String, Int], Option[Int]](
    Prod2.Prod2Lens1[Option, String, Int]), "1.")

}

object Cop2Test extends Properties("Cop2") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop2[?[_], String, Int], Functor])
}

object Prod3Test extends Properties("Prod3") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod3[?[_], String, Int, String], Applicative])

  include(lens.laws[Prod3[Option, String, Int, String], Option[String]](
    Prod3.Prod3Lens0[Option, String, Int, String]), "0.")

  include(lens.laws[Prod3[Option, String, Int, String], Option[Int]](
    Prod3.Prod3Lens1[Option, String, Int, String]), "1.")

  include(lens.laws[Prod3[Option, String, Int, String], Option[String]](
    Prod3.Prod3Lens2[Option, String, Int, String]), "2.")

}

object Cop3Test extends Properties("Cop3") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop3[?[_], String, Int, String], Functor])
}

object Prod4Test extends Properties("Prod4") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod4[?[_], String, Int, String, Int], Applicative])

  include(lens.laws[Prod4[Option, String, Int, String, Int], Option[String]](
    Prod4.Prod4Lens0[Option, String, Int, String, Int]), "0.")

  include(lens.laws[Prod4[Option, String, Int, String, Int], Option[Int]](
    Prod4.Prod4Lens1[Option, String, Int, String, Int]), "1.")

  include(lens.laws[Prod4[Option, String, Int, String, Int], Option[String]](
    Prod4.Prod4Lens2[Option, String, Int, String, Int]), "2.")

  include(lens.laws[Prod4[Option, String, Int, String, Int], Option[Int]](
    Prod4.Prod4Lens3[Option, String, Int, String, Int]), "3.")

}

object Cop4Test extends Properties("Cop4") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop4[?[_], String, Int, String, Int], Functor])
}

object Prod5Test extends Properties("Prod5") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod5[?[_], String, Int, String, Int, String], Applicative])

  include(lens.laws[Prod5[Option, String, Int, String, Int, String], Option[String]](
    Prod5.Prod5Lens0[Option, String, Int, String, Int, String]), "0.")

  include(lens.laws[Prod5[Option, String, Int, String, Int, String], Option[Int]](
    Prod5.Prod5Lens1[Option, String, Int, String, Int, String]), "1.")

  include(lens.laws[Prod5[Option, String, Int, String, Int, String], Option[String]](
    Prod5.Prod5Lens2[Option, String, Int, String, Int, String]), "2.")

  include(lens.laws[Prod5[Option, String, Int, String, Int, String], Option[Int]](
    Prod5.Prod5Lens3[Option, String, Int, String, Int, String]), "3.")

  include(lens.laws[Prod5[Option, String, Int, String, Int, String], Option[String]](
    Prod5.Prod5Lens4[Option, String, Int, String, Int, String]), "4.")

}

object Cop5Test extends Properties("Cop5") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop5[?[_], String, Int, String, Int, String], Functor])
}

object Prod6Test extends Properties("Prod6") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod6[?[_], String, Int, String, Int, String, Int], Applicative])

  include(lens.laws[Prod6[Option, String, Int, String, Int, String, Int], Option[String]](
    Prod6.Prod6Lens0[Option, String, Int, String, Int, String, Int]), "0.")

  include(lens.laws[Prod6[Option, String, Int, String, Int, String, Int], Option[Int]](
    Prod6.Prod6Lens1[Option, String, Int, String, Int, String, Int]), "1.")

  include(lens.laws[Prod6[Option, String, Int, String, Int, String, Int], Option[String]](
    Prod6.Prod6Lens2[Option, String, Int, String, Int, String, Int]), "2.")

  include(lens.laws[Prod6[Option, String, Int, String, Int, String, Int], Option[Int]](
    Prod6.Prod6Lens3[Option, String, Int, String, Int, String, Int]), "3.")

  include(lens.laws[Prod6[Option, String, Int, String, Int, String, Int], Option[String]](
    Prod6.Prod6Lens4[Option, String, Int, String, Int, String, Int]), "4.")

  include(lens.laws[Prod6[Option, String, Int, String, Int, String, Int], Option[Int]](
    Prod6.Prod6Lens5[Option, String, Int, String, Int, String, Int]), "5.")

}

object Cop6Test extends Properties("Cop6") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop6[?[_], String, Int, String, Int, String, Int], Functor])
}

