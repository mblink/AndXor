package andxor


import andxor.scalacheck.{given, *}
import cats.{~>, Apply, Id, Show}
import cats.syntax.apply.*
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.{forAllNoShrink, propBoolean}
import scala.compiletime.{summonAll, summonFrom}
import scala.util.Try

object typeclasses {
  inline given showLabelled[A: Show, L <: Singleton with String](using inline l: ValueOf[L]): Show[Labelled[A, L]] =
    Show.show(l => s"${l.label} := ${Show[A].show(l.value)}" ++ "\n")

  inline given showSingleton[A <: Singleton, L <: Singleton with String](using inline l: ValueOf[L]): Show[Labelled[A, L]] =
    Show.show(a => s"ADTValue := ${a.label}" ++ "\n")

  type ShowF[A] = A => String
  val showToShowF: Show ~> ShowF = new (Show ~> ShowF) {
    def apply[A](f: Show[A]) = f.show
  }
  val showFToShow: ShowF ~> Show = new (ShowF ~> Show) {
    def apply[A](f: ShowF[A]) = Show.show(f)
  }

  given showDecide: Decidable[Show] = Decidable.fromIso[Show, ShowF](showToShowF, showFToShow)
  given showDivide: Divide[Show] = Divide.fromIso[Show, ShowF](showToShowF, showFToShow)

  inline given showCop[A](using c: AndXorCopIso[A], i: AndXorInstances[Show, c.LabelledProd[Id]]): Show[A] =
    c.derivingLabelled[Show].choose

  inline given showProd[A](using p: AndXorProdIso[A], i: AndXorInstances[Show, p.LabelledProd[Id]]): Show[A] =
    p.derivingLabelled[Show].divide

  trait Read[A] { def read(s: String): Option[A] }
  object Read {
    inline given readLabelled[A, L <: Singleton with String](using inline l: ValueOf[L], r: Read[A]): Read[Labelled[A, L]] =
      new Read[Labelled[A, L]] {
        def read(s: String): Option[Labelled[A, L]] =
          s.split("\n").toList.flatMap(_.split(s"${l.value} := ", 2).lift(1).flatMap(r.read(_))).headOption.map(Labelled(_))
      }

    inline given readAdtVal[A <: Singleton, L <: Singleton with String](
      using inline l: ValueOf[L],
      v: ValueOf[Labelled[A, L]]
    ): Read[Labelled[A, L]] =
      new Read[Labelled[A, L]] {
        def read(s: String): Option[Labelled[A, L]] =
          s.split("\n").toList.flatMap(_.split(s"ADTValue := ", 2).lift(1).filter(_ == l.value)).headOption.map(_ => v.value)
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
          given axoInstances: AndXorInstances[Read, p.LabelledProd[Id]] =
            AndXorInstances(summonAll[Tuple.Map[p.LabelledProd[Id], Read]])

          p.derivingLabelled[Read].apply

        case c: AndXorCopIso[A] =>
          given axoInstances: AndXorInstances[Read, c.LabelledProd[Id]] =
            AndXorInstances(summonAll[Tuple.Map[c.LabelledProd[Id], Read]])

          c.derivingLabelled[Read].alt
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

  sealed trait Foo derives Arbitrary, Csv, Read
  case object Bar extends Foo
  case class Baz(s: String) extends Foo derives Arbitrary, Csv, Read

  sealed trait Trait0 derives Arbitrary, Csv, Read
  sealed trait Trait1 extends Trait0 derives Arbitrary, Csv, Read { val value: Option[Int] }
  sealed trait Trait2 extends Trait0 derives Arbitrary, Csv, Read
  sealed trait Trait3 extends Trait1 derives Arbitrary, Csv, Read
  case object Inst1 extends Trait0
  case class Inst2(value: Option[Int]) extends Trait1 derives Arbitrary, Csv, Read
  case object Inst3 extends Trait1 { val value = Some(3) }
  case object Inst4 extends Trait2
  case object Inst5 extends Trait3 { val value = Some(5) }
  case class Inst6(x: String) extends Trait3 derives Arbitrary, Csv, Read { val value = Try(x.toInt).toOption }

  sealed abstract class AbstractClass(val i: Int) derives Arbitrary, Csv, Read
  object AbstractClass {
    case class Foo(s: String, b: Boolean) extends AbstractClass(1) derives Arbitrary, Csv, Read
    case object Bar extends AbstractClass(2)
    case class Baz(override val i: Int) extends AbstractClass(3) derives Arbitrary, Csv, Read
  }

  case class HKFG[F[_[_]], G[_]](run: F[G])
  object HKFG {
    given arbitrary[F[_[_]], G[_]](using a: Arbitrary[F[G]]): Arbitrary[HKFG[F, G]] = Arbitrary.derived
    given csv[F[_[_]], G[_]](using c: Csv[F[G]]): Csv[HKFG[F, G]] = Csv.derived
    given read[F[_[_]], G[_]](using r: Read[F[G]]): Read[HKFG[F, G]] = Read.derived
  }

  case class Covariant[+A](a: A) derives Arbitrary, Csv, Read

  case class Contravariant[-A](s: String, i: Int) derives Arbitrary, Csv, Read {
    def go(a: A): (String, Int) = (s"$s -- $a", i)
  }

  case class ErrorTest1[A](i: Int, a: A) derives Arbitrary, Csv, Read

  case class ErrorTest2[A](as: List[A], ints: List[Int], test1s: List[ErrorTest1[A]])
  object ErrorTest2 {
    given arbitrary[A](using as: Arbitrary[List[A]], ints: Arbitrary[List[Int]], test1s: Arbitrary[List[ErrorTest1[A]]]): Arbitrary[ErrorTest2[A]] = Arbitrary.derived
    given csv[A](using as: Csv[List[A]], ints: Csv[List[Int]], test1s: Csv[List[ErrorTest1[A]]]): Csv[ErrorTest2[A]] = Csv.derived
    given read[A](using as: Read[List[A]], ints: Read[List[Int]], test1s: Read[List[ErrorTest1[A]]]): Read[ErrorTest2[A]] = Read.derived
  }

  case class ErrorTest3[D[_], A](test1s: List[ErrorTest1[D[A]]], test2: ErrorTest2[A])
  object ErrorTest3 {
    given arbitrary[D[_], A](using test1s: Arbitrary[List[ErrorTest1[D[A]]]], test2: Arbitrary[ErrorTest2[A]]): Arbitrary[ErrorTest3[D, A]] = Arbitrary.derived
    given csv[D[_], A](using test1s: Csv[List[ErrorTest1[D[A]]]], test2: Csv[ErrorTest2[A]]): Csv[ErrorTest3[D, A]] = Csv.derived
    given read[D[_], A](using test1s: Read[List[ErrorTest1[D[A]]]], test2: Read[ErrorTest2[A]]): Read[ErrorTest3[D, A]] = Read.derived

    def update[G[_], B](bs: List[B], ts: List[(Int, B)], d: List[Int]): ErrorTest3[G, B] =
      ErrorTest3(Nil, ErrorTest2(bs, d, ts.map(t => ErrorTest1[B](t._1, t._2))))
  }

  case class TParamsDup[A](a1: A, a2: A) derives Arbitrary, Csv, Read

  
  case class Test1(x0: Int)
    derives Arbitrary, Csv, Read

  case class TParams1[A1](x0: A1)
    derives Arbitrary, Csv, Read

  case class HK1[F[_], A1](run: F[A1])
  object HK1 {
    given arbitrary[F[_], A1](using a: Arbitrary[F[A1]]): Arbitrary[HK1[F, A1]] = Arbitrary.derived
    given csv[F[_], A1](using c: Csv[F[A1]]): Csv[HK1[F, A1]] = Csv.derived
    given read[F[_], A1](using r: Read[F[A1]]): Read[HK1[F, A1]] = Read.derived
  }


  case class Test2(x0: Int, x1: String)
    derives Arbitrary, Csv, Read

  case class TParams2[A1, A2](x0: A1, x1: A2)
    derives Arbitrary, Csv, Read

  case class HK2[F[_, _], A1, A2](run: F[A1, A2])
  object HK2 {
    given arbitrary[F[_, _], A1, A2](using a: Arbitrary[F[A1, A2]]): Arbitrary[HK2[F, A1, A2]] = Arbitrary.derived
    given csv[F[_, _], A1, A2](using c: Csv[F[A1, A2]]): Csv[HK2[F, A1, A2]] = Csv.derived
    given read[F[_, _], A1, A2](using r: Read[F[A1, A2]]): Read[HK2[F, A1, A2]] = Read.derived
  }


  case class Test3(x0: Int, x1: String, x2: Boolean)
    derives Arbitrary, Csv, Read

  case class TParams3[A1, A2, A3](x0: A1, x1: A2, x2: A3)
    derives Arbitrary, Csv, Read

  case class HK3[F[_, _, _], A1, A2, A3](run: F[A1, A2, A3])
  object HK3 {
    given arbitrary[F[_, _, _], A1, A2, A3](using a: Arbitrary[F[A1, A2, A3]]): Arbitrary[HK3[F, A1, A2, A3]] = Arbitrary.derived
    given csv[F[_, _, _], A1, A2, A3](using c: Csv[F[A1, A2, A3]]): Csv[HK3[F, A1, A2, A3]] = Csv.derived
    given read[F[_, _, _], A1, A2, A3](using r: Read[F[A1, A2, A3]]): Read[HK3[F, A1, A2, A3]] = Read.derived
  }

}

object DerivationTest extends Properties("Derivation") {
  import typeclasses.{Csv, Read, given}
  import testTypes.*

  def proof[A: Arbitrary: Csv: Show](label: String)(using r: Read[A]) =
    property(label) = forAllNoShrink((a: A) => {
      (implicitly[Csv[A]].toCsv(a).nonEmpty :| "CSV output was empty") &&
      // can't really test `Read` because the implementations don't work for nested values
      (implicitly[Show[A]].show(a).nonEmpty :| "show/read was not Eq")
    })

  proof[Foo]("Foo")
  proof[Baz]("Baz")
  proof[Trait0]("Trait0")
  proof[AbstractClass]("AbstractClass")
  proof[HKFG[FConst[String], Id]]("HKFG")
  proof[Covariant[String]]("Covariant")
  proof[Contravariant[String]]("Contravariant")
  
  proof[Test1]("Test1")
  proof[TParams1[String]]("TParams1")
  proof[HK1[TParams1, String]]("HK1")


  proof[Test2]("Test2")
  proof[TParams2[String, String]]("TParams2")
  proof[HK2[TParams2, String, String]]("HK2")


  proof[Test3]("Test3")
  proof[TParams3[String, String, String]]("TParams3")
  proof[HK3[TParams3, String, String, String]]("HK3")

}
