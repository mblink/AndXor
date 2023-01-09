package andxor.template

object DerivationTest {
  def apply(tpeLists: List[(List[String], Int)]) = {
    val testCCName = (ts: List[String]) => s"Test${ts.length}"
    val tparamsCCName = (ts: List[String]) => s"TParams${ts.length}"
    val hkCCName = (ts: List[String]) => s"HK${ts.length}"
    val testCCValueTpe = (i: Int) => List("Int", "String", "Boolean").apply(i % 3)

    s"""
// import andxor.argonaut.*
import andxor.circe.*
import andxor.scalacheck.{given, *}
import _root_.argonaut.{DecodeJson, EncodeJson}
import cats.{~>, Apply, Eq, Id, Show}
import cats.syntax.apply.*
import cats.syntax.eq.*
import io.circe.{Decoder, Encoder}
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.{forAllNoShrink, propBoolean}
import scala.compiletime.{summonAll, summonFrom}
import scala.util.Try

object typeclasses {
  inline given showLabelled[A: Show, L <: Singleton with String](using inline l: ValueOf[L]): Show[Labelled[A, L]] =
    Show.show(l => s"$${l.label} := $${Show[A].show(l.value)}" ++ "\\n")

  inline given showSingleton[A <: Singleton, L <: Singleton with String](using inline l: ValueOf[L]): Show[Labelled[A, L]] =
    Show.show(a => s"ADTValue := $${a.label}" ++ "\\n")

  type ShowF[A] = A => String
  val showToShowF: Show ~> ShowF = new (Show ~> ShowF) {
    def apply[A](f: Show[A]) = f.show
  }
  val showFToShow: ShowF ~> Show = new (ShowF ~> Show) {
    def apply[A](f: ShowF[A]) = Show.show(f)
  }

  given showDecide: Decidable[Show] = Decidable.fromIso[Show, ShowF](showToShowF, showFToShow)
  given showDivide: Divide[Show] = Divide.fromIso[Show, ShowF](showToShowF, showFToShow)

  trait Read[A] { def read(s: String): Option[A] }
  object Read {
    inline given readLabelled[A, L <: Singleton with String](using inline l: ValueOf[L], r: Read[A]): Read[Labelled[A, L]] =
      new Read[Labelled[A, L]] {
        def read(s: String): Option[Labelled[A, L]] =
          s.split("\\n").toList.flatMap(_.split(s"$${l.value} := ", 2).lift(1).flatMap(r.read(_))).headOption.map(Labelled(_))
      }

    inline given readAdtVal[A <: Singleton, L <: Singleton with String](
      using inline l: ValueOf[L],
      v: ValueOf[Labelled[A, L]]
    ): Read[Labelled[A, L]] =
      new Read[Labelled[A, L]] {
        def read(s: String): Option[Labelled[A, L]] =
          s.split("\\n").toList.flatMap(_.split(s"ADTValue := ", 2).lift(1).filter(_ == l.value)).headOption.map(_ => v.value)
      }

    inline given readList[A](using inline r: Read[A]): Read[List[A]] = new Read[List[A]] {
      def read(s: String): Option[List[A]] = Some(Nil)
    }

    inline given readOption[A](using inline r: Read[A]): Read[Option[A]] = new Read[Option[A]] {
      def read(s: String): Option[Option[A]] = r.read(s).map(Some(_))
    }

    inline given readStr: Read[String] = new Read[String] {
      def read(s: String): Option[String] =
        if (s.headOption.exists(_ == '"') && s.lastOption.exists(_ == '"')) Some(s.tail.init) else None
    }
    inline given readInt: Read[Int] = new Read[Int] { def read(s: String): Option[Int] = Try(s.toInt).toOption }
    inline given readBool: Read[Boolean] = new Read[Boolean] { def read(s: String): Option[Boolean] = Try(s.toBoolean).toOption }

    trait ReadApply extends Apply[Read] {
      def map[A, B](fa: Read[A])(f: A => B): Read[B] =
        new Read[B] { def read(s: String): Option[B] = fa.read(s).map(f) }
      def ap[A, B](f: Read[A => B])(fa: Read[A]): Read[B] =
        new Read[B] { def read(s: String): Option[B] = (f.read(s), fa.read(s)).mapN(_(_)) }
    }

    inline given readApply: Apply[Read] = new ReadApply {}

    inline given readAlt: Alt[Read] = new Alt[Read] with ReadApply {
      def alt[A](a1: Read[A], a2: Read[A]): Read[A] =
        new Read[A] { def read(s: String): Option[A] = a1.read(s).orElse(a2.read(s)) }
    }

    inline def derived[A]: Read[A] =
      summonFrom {
        case p: AndXorProdIso[A] =>
          given axoInstances: AndXorInstances[Read, p.andxor.Prod[Id]] =
            AndXorInstances(summonAll[Tuple.Map[p.andxor.Prod[Id], Read]])

          p.deriving[Read].apply

        case c: AndXorCopIso[A] =>
          given axoInstances: AndXorInstances[Read, c.andxor.Prod[Id]] =
            AndXorInstances(summonAll[Tuple.Map[c.andxor.Prod[Id], Read]])

          c.deriving[Read].alt
      }
  }

  trait Csv[A] { def toCsv(a: A): List[String] }
  object Csv {
    inline given csvStr: Csv[String] = new Csv[String] { def toCsv(x: String): List[String] = List(x) }
    inline given csvInt: Csv[Int] = new Csv[Int] { def toCsv(x: Int): List[String] = List(x.toString) }
    inline given csvBool: Csv[Boolean] = new Csv[Boolean] { def toCsv(x: Boolean): List[String] = List(x.toString) }
    inline given csvList[A](using inline c: Csv[A]): Csv[List[A]] =
      new Csv[List[A]] { def toCsv(l: List[A]): List[String] = l.flatMap(c.toCsv(_)) }

    inline given csvOption[A](using inline c: Csv[A]): Csv[Option[A]] =
      new Csv[Option[A]] { def toCsv(o: Option[A]): List[String] = o.fold(List[String](null))(c.toCsv(_)) }

    inline given csvLabelled[A, L <: Singleton with String](using inline c: Csv[A]): Csv[Labelled[A, L]] =
      new Csv[Labelled[A, L]] {
        def toCsv(a: Labelled[A, L]): List[String] = c.toCsv(a.value)
      }

    inline given csvSingleton[A <: Singleton, L <: Singleton & String](using inline l: ValueOf[L]): Csv[Labelled[A, L]] =
      new Csv[Labelled[A, L]] {
        def toCsv(a: Labelled[A, L]): List[String] = List(a.label)
      }

    type CsvF[A] = A => List[String]
    val csvToCsvF: Csv ~> CsvF = new(Csv ~> CsvF) {
      def apply[A](f: Csv[A]): CsvF[A] = f.toCsv
    }
    val csvFToCsv: CsvF ~> Csv = new (CsvF ~> Csv) {
      def apply[A](f: CsvF[A]): Csv[A] = new Csv[A] { def toCsv(a: A): List[String] = f(a) }
    }

    given csvDecide: Decidable[Csv] = Decidable.fromIso[Csv, CsvF](csvToCsvF, csvFToCsv)
    given csvDivide: Divide[Csv] = Divide.fromIso[Csv, CsvF](csvToCsvF, csvFToCsv)

    inline def derived[A]: Csv[A] =
      summonFrom {
        case p: AndXorProdIso[A] =>
          given axoInstances: AndXorInstances[Csv, p.LabelledProd[Id]] =
            AndXorInstances(summonAll[Tuple.Map[p.LabelledProd[Id], Csv]])

          p.derivingLabelled[Csv].divide

        case c: AndXorCopIso[A] =>
          given axoInstances: AndXorInstances[Csv, c.LabelledProd[Id]] =
            AndXorInstances(summonAll[Tuple.Map[c.LabelledProd[Id], Csv]])

          c.derivingLabelled[Csv].choose
      }
  }
}

object testTypes {
  import typeclasses.*

  case class NoInstances(s: String)

  // Test that zero-member coproduct warning is triggered
  // The nowarn annotation will cause a "does not suppress any warnings" error if not
  @annotation.nowarn("msg=zero-member coproduct")
  sealed trait EmptyCop

  // @@deriving(Arbitrary, Csv, Decoder, DecodeJson, Encoder, EncodeJson, Eq, Read, Show)
  sealed trait Foo derives Arbitrary, Csv //, Decoder, DecodeJson // , Encoder, EncodeJson, Eq, Read, Show
  case object Bar extends Foo
  case class Baz(s: String) extends Foo derives Arbitrary, Csv

  sealed trait Trait0 // derives Arbitrary, Csv
  sealed trait Trait1 extends Trait0 { val value: Option[Int] }
  sealed trait Trait2 extends Trait0
  sealed trait Trait3 extends Trait1
  case object Inst1 extends Trait0
  case class Inst2(value: Option[Int]) extends Trait1 derives Arbitrary, Csv
  case object Inst3 extends Trait1 { val value = Some(3) }
  case object Inst4 extends Trait2
  case object Inst5 extends Trait3 { val value = Some(5) }
  case class Inst6(x: String) extends Trait3 { val value = Try(x.toInt).toOption }

  sealed abstract class AbstractClass(val i: Int)
  object AbstractClass {
    case class Foo(s: String, b: Boolean) extends AbstractClass(1)
    case object Bar extends AbstractClass(2)
    case class Baz(override val i: Int) extends AbstractClass(3)
  }

  case class Multi(str: String)(val int: Int)

  case class HKFG[F[_[_]], G[_]](run: F[G])

  case class Covariant[+A](a: A)

  case class Contravariant[-A](s: String, i: Int) {
    def go(a: A): (String, Int) = (s"$$s -- $$a", i)
  }

  case class ErrorTest1[A](i: Int, a: A)

  case class ErrorTest2[A](as: List[A], ints: List[Int], test1s: List[ErrorTest1[A]])

  case class ErrorTest3[D[_], A](test1s: List[ErrorTest1[D[A]]], test2: ErrorTest2[A])
  object ErrorTest3 {
    def update[G[_], B](bs: List[B], ts: List[(Int, B)], d: List[Int]): ErrorTest3[G, B] =
      ErrorTest3(Nil, ErrorTest2(bs, d, ts.map(t => ErrorTest1[B](t._1, t._2))))
  }

  case class TParamsDup[A](a1: A, a2: A)

  ${tpeLists.map { case (tpes, _) => s"""
  case class ${testCCName(tpes)}(${tpes.zipWithIndex.map { case (_, i) => s"x$i: ${testCCValueTpe(i)}" }.mkString(", ")})

  case class ${tparamsCCName(tpes)}[${tpes.mkString(", ")}](${tpes.zipWithIndex.map { case (t, i) => s"x$i: $t" }.mkString(", ")})

  case class ${hkCCName(tpes)}[F[${tpes.map(_ => "_").mkString(", ")}], ${tpes.mkString(", ")}](run: F[${tpes.mkString(", ")}])
"""
  }.mkString("\n")}
}

object DerivingPluginTest extends Properties("DerivingPlugin") {
  import typeclasses.{Csv, Read}
  import testTypes.*

  def proof[A: Arbitrary: Csv: DecodeJson: Decoder: EncodeJson: Encoder: Eq: Show](label: String)(using r: Read[A]) =
    property(label) = forAllNoShrink((a: A) => {
      (implicitly[Csv[A]].toCsv(a).nonEmpty :| "CSV output was empty") &&
      ((implicitly[DecodeJson[A]].decodeJson(implicitly[EncodeJson[A]].encode(a)).toOption.get === a) :| "argonaut was not Eq") &&
      ((implicitly[Decoder[A]].decodeJson(implicitly[Encoder[A]].apply(a)) match {
        case Right(res) => res === a
        case _ => false
      }) :| "circe was not Eq") &&
      // can't really test `Read` because the implementations don't work for nested values
      (implicitly[Show[A]].show(a).nonEmpty :| "show/read was not Eq")
    })

  // proof[Foo]("Foo")
  /*proof[Baz]("Baz")
  proof[Trait0]("Trait0")
  proof[AbstractClass]("AbstractClass")
  proof[Multi]("Multi")
  proof[HKFG[FConst[String]#T, Id]]("HKFG")
  proof[Covariant[String]]("Covariant")
  proof[Contravariant[String]]("Contravariant")
  ${tpeLists.map { case (tpes, _) => s"""
  proof[${testCCName(tpes)}]("${testCCName(tpes)}")
  proof[${tparamsCCName(tpes)}[${tpes.map(_ => "String").mkString(", ")}]]("${tparamsCCName(tpes)}")
  proof[${hkCCName(tpes)}[${tparamsCCName(tpes)}, ${tpes.map(_ => "String").mkString(", ")}]]("${hkCCName(tpes)}")
"""
  }.mkString("\n")}*/
}
"""
  }
}
