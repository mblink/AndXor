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

}

object Prod2Test extends Properties("Prod2") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod2[?[_], String, Int]])

  include(lens.laws[Prod2[Option, String, Int], Option[String]](
    Prod2.Prod2Lens0[Option, String, Int]), "0.")

  include(lens.laws[Prod2[Option, String, Int], Option[Int]](
    Prod2.Prod2Lens1[Option, String, Int]), "1.")

}

object Cop2Test extends Properties("Cop2") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop2[?[_], String, Int]])
}

object Prod3Test extends Properties("Prod3") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod3[?[_], String, Int, String]])

  include(lens.laws[Prod3[Option, String, Int, String], Option[String]](
    Prod3.Prod3Lens0[Option, String, Int, String]), "0.")

  include(lens.laws[Prod3[Option, String, Int, String], Option[Int]](
    Prod3.Prod3Lens1[Option, String, Int, String]), "1.")

  include(lens.laws[Prod3[Option, String, Int, String], Option[String]](
    Prod3.Prod3Lens2[Option, String, Int, String]), "2.")

}

object Cop3Test extends Properties("Cop3") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Cop3[?[_], String, Int, String]])
}

object Prod4Test extends Properties("Prod4") {
  import arbitrary._

  include(AndXorProperties.ftraverse.laws[Prod4[?[_], String, Int, String, Int]])

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

  include(AndXorProperties.ftraverse.laws[Cop4[?[_], String, Int, String, Int]])
}

