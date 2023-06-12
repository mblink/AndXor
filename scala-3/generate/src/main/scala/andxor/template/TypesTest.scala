package andxor.template

object TypesTest {
  def apply(tpeLists: List[(List[String], Int)]) =
    s"""
import andxor.AndXorProperties.arbitrary.*
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
  ${tpeLists.map { case (tpes, i) =>
    val tpesWJ = tpes.zip(LazyList.from(1))
    val fTpes = tpes.map(t => s"F[$t]")
    val prodTpe = s"${fTpes.mkString(" *: ")} *: EmptyTuple"
    val copTpe = s"${fTpes.mkString(" |: ")}"
    val arbInsts = tpesWJ.map { case (t, j) => s"a$j: Arbitrary[F[$t]]" }.mkString(",\n      ")
    val eqInsts = tpesWJ.map { case (t, j) => s"e$j: Eq[F[$t]]" }.mkString(",\n      ")

    s"""
    implicit def prod${i}Arb[F[_], ${tpes.mkString(", ")}](implicit
      $arbInsts
    ): Arbitrary[$prodTpe] =
      AndXor[${tpes.mkString(", ")}].deriving[Arbitrary, F].apply

    implicit def cop${i}Arb[F[_], ${tpes.mkString(", ")}](implicit
      $arbInsts
    ): Arbitrary[$copTpe] =
      AndXor[${tpes.mkString(", ")}].deriving[Arbitrary, F].alt

    implicit def prod${i}Eq[F[_], ${tpes.mkString(", ")}](implicit
      $eqInsts
    ): Eq[$prodTpe] =
      AndXor[${tpes.mkString(", ")}].deriving[Eq, F].divide

    implicit def cop${i}Eq[F[_], ${tpes.mkString(", ")}](implicit
      $eqInsts
    ): Eq[$copTpe] =
      AndXor[${tpes.mkString(", ")}].deriving[Eq, F].choose
"""
  }.mkString("\n")}
}

${tpeLists.map { case (_tpes, i) =>
  val tpes = _tpes.zipWithIndex.map { case (t, j) => if (j % 2 == 0) "String" else "Int" }
  val prodName = s"Prod${i}"
  val fTpes = (F: String) => tpes.map(t => s"$F[$t]")
  val prodTpe = (F: String) => (fTpes(F) :+ "EmptyTuple").mkString(" *: ")
  val copName = s"Cop${i}"
  val copTpe = (F: String) => fTpes(F).mkString(" |: ")

  s"""
object ${prodName}Test extends Properties("$prodName") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> ${prodTpe("F")}, Apply])

  include(MonoidTests[${prodTpe("Option")}].monoid.all)

  ${1.to(i).map(j =>
    s"include(LensTests[${prodTpe("Option")}, Option[${tpes(j - 1)}]](summon[Lens[${prodTpe("Option")}, Option[${tpes(j - 1)}]]].asMonocle).all, \"$j.\")"
  ).mkString("\n\n  ")}
}

object ${copName}Test extends Properties("$copName") {
  import arbitrary.*

  include(AndXorProperties.ftraverse.laws[[F[_]] =>> ${copTpe("F")}, Functor])
}
"""
  }.mkString("\n")
}
"""
}
