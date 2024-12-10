package andxor


import andxor.scalacheck.{given, *}
import cats.{~>, Apply, Id, Show}
import cats.syntax.apply.*
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.{forAllNoShrink, propBoolean}
import scala.compiletime.{summonAll, summonFrom}
import scala.util.Try

object typeclasses {
  given showLabelled[A: Show, L <: Singleton with String](using l: ValueOf[L]): Show[Labelled[A, L]] =
    Show.show(l => s"${l.label} := ${Show[A].show(l.value)}" ++ "\n")

  given showSingleton[A <: Singleton, L <: Singleton with String](using l: ValueOf[L]): Show[Labelled[A, L]] =
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

  given showCop[A](using c: AndXorCopIso[A], i: AndXorInstances[Show, c.LabelledProd[Id]]): Show[A] =
    c.derivingLabelled[Show].choose

  given showProd[A](using p: AndXorProdIso[A], i: AndXorInstances[Show, p.LabelledProd[Id]]): Show[A] =
    p.derivingLabelled[Show].divide

  trait Read[A] { def read(s: String): Option[A] }
  object Read {
    given readLabelled[A, L <: Singleton with String](using l: ValueOf[L], r: Read[A]): Read[Labelled[A, L]] =
      new Read[Labelled[A, L]] {
        def read(s: String): Option[Labelled[A, L]] =
          s.split("\n").toList.flatMap(_.split(s"${l.value} := ", 2).lift(1).flatMap(r.read(_))).headOption.map(Labelled(_))
      }

    given readAdtVal[A <: Singleton, L <: Singleton with String](
      using l: ValueOf[L],
      v: ValueOf[Labelled[A, L]]
    ): Read[Labelled[A, L]] =
      new Read[Labelled[A, L]] {
        def read(s: String): Option[Labelled[A, L]] =
          s.split("\n").toList.flatMap(_.split(s"ADTValue := ", 2).lift(1).filter(_ == l.value)).headOption.map(_ => v.value)
      }

    given readList[A](using r: Read[A]): Read[List[A]] = new Read[List[A]] {
      def read(s: String): Option[List[A]] = Some(Nil)
    }

    given readOption[A](using r: Read[A]): Read[Option[A]] = new Read[Option[A]] {
      def read(s: String): Option[Option[A]] = r.read(s).map(Some(_))
    }

    given readStr: Read[String] = new Read[String] {
      def read(s: String): Option[String] =
        if (s.headOption.exists(_ == '"') && s.lastOption.exists(_ == '"')) Some(s.tail.init) else None
    }
    given readInt: Read[Int] = new Read[Int] { def read(s: String): Option[Int] = Try(s.toInt).toOption }
    given readBool: Read[Boolean] = new Read[Boolean] { def read(s: String): Option[Boolean] = Try(s.toBoolean).toOption }

    trait ReadApply extends Apply[Read] {
      def map[A, B](fa: Read[A])(f: A => B): Read[B] =
        new Read[B] { def read(s: String): Option[B] = fa.read(s).map(f) }
      def ap[A, B](f: Read[A => B])(fa: Read[A]): Read[B] =
        new Read[B] { def read(s: String): Option[B] = (f.read(s), fa.read(s)).mapN(_(_)) }
    }

    given readApply: Apply[Read] = new ReadApply {}

    given readAlt: Alt[Read] = new Alt[Read] with ReadApply {
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
    given csvStr: Csv[String] = new Csv[String] { def toCsv(x: String): List[String] = List(x) }
    given csvInt: Csv[Int] = new Csv[Int] { def toCsv(x: Int): List[String] = List(x.toString) }
    given csvBool: Csv[Boolean] = new Csv[Boolean] { def toCsv(x: Boolean): List[String] = List(x.toString) }
    given csvList[A](using c: Csv[A]): Csv[List[A]] =
      new Csv[List[A]] { def toCsv(l: List[A]): List[String] = l.flatMap(c.toCsv(_)) }

    given csvOption[A](using c: Csv[A]): Csv[Option[A]] =
      new Csv[Option[A]] { def toCsv(o: Option[A]): List[String] = o.fold(List[String](null))(c.toCsv(_)) }

    given csvLabelled[A, L <: Singleton with String](using c: Csv[A]): Csv[Labelled[A, L]] =
      new Csv[Labelled[A, L]] {
        def toCsv(a: Labelled[A, L]): List[String] = c.toCsv(a.value)
      }

    given csvSingleton[A <: Singleton, L <: Singleton & String](using l: ValueOf[L]): Csv[Labelled[A, L]] =
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


  case class Test4(x0: Int, x1: String, x2: Boolean, x3: Int)
    derives Arbitrary, Csv, Read

  case class TParams4[A1, A2, A3, A4](x0: A1, x1: A2, x2: A3, x3: A4)
    derives Arbitrary, Csv, Read

  case class HK4[F[_, _, _, _], A1, A2, A3, A4](run: F[A1, A2, A3, A4])
  object HK4 {
    given arbitrary[F[_, _, _, _], A1, A2, A3, A4](using a: Arbitrary[F[A1, A2, A3, A4]]): Arbitrary[HK4[F, A1, A2, A3, A4]] = Arbitrary.derived
    given csv[F[_, _, _, _], A1, A2, A3, A4](using c: Csv[F[A1, A2, A3, A4]]): Csv[HK4[F, A1, A2, A3, A4]] = Csv.derived
    given read[F[_, _, _, _], A1, A2, A3, A4](using r: Read[F[A1, A2, A3, A4]]): Read[HK4[F, A1, A2, A3, A4]] = Read.derived
  }


  case class Test5(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String)
    derives Arbitrary, Csv, Read

  case class TParams5[A1, A2, A3, A4, A5](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5)
    derives Arbitrary, Csv, Read

  case class HK5[F[_, _, _, _, _], A1, A2, A3, A4, A5](run: F[A1, A2, A3, A4, A5])
  object HK5 {
    given arbitrary[F[_, _, _, _, _], A1, A2, A3, A4, A5](using a: Arbitrary[F[A1, A2, A3, A4, A5]]): Arbitrary[HK5[F, A1, A2, A3, A4, A5]] = Arbitrary.derived
    given csv[F[_, _, _, _, _], A1, A2, A3, A4, A5](using c: Csv[F[A1, A2, A3, A4, A5]]): Csv[HK5[F, A1, A2, A3, A4, A5]] = Csv.derived
    given read[F[_, _, _, _, _], A1, A2, A3, A4, A5](using r: Read[F[A1, A2, A3, A4, A5]]): Read[HK5[F, A1, A2, A3, A4, A5]] = Read.derived
  }


  case class Test6(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean)
    derives Arbitrary, Csv, Read

  case class TParams6[A1, A2, A3, A4, A5, A6](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5, x5: A6)
    derives Arbitrary, Csv, Read

  case class HK6[F[_, _, _, _, _, _], A1, A2, A3, A4, A5, A6](run: F[A1, A2, A3, A4, A5, A6])
  object HK6 {
    given arbitrary[F[_, _, _, _, _, _], A1, A2, A3, A4, A5, A6](using a: Arbitrary[F[A1, A2, A3, A4, A5, A6]]): Arbitrary[HK6[F, A1, A2, A3, A4, A5, A6]] = Arbitrary.derived
    given csv[F[_, _, _, _, _, _], A1, A2, A3, A4, A5, A6](using c: Csv[F[A1, A2, A3, A4, A5, A6]]): Csv[HK6[F, A1, A2, A3, A4, A5, A6]] = Csv.derived
    given read[F[_, _, _, _, _, _], A1, A2, A3, A4, A5, A6](using r: Read[F[A1, A2, A3, A4, A5, A6]]): Read[HK6[F, A1, A2, A3, A4, A5, A6]] = Read.derived
  }


  case class Test7(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int)
    derives Arbitrary, Csv, Read

  case class TParams7[A1, A2, A3, A4, A5, A6, A7](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5, x5: A6, x6: A7)
    derives Arbitrary, Csv, Read

  case class HK7[F[_, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7](run: F[A1, A2, A3, A4, A5, A6, A7])
  object HK7 {
    given arbitrary[F[_, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7](using a: Arbitrary[F[A1, A2, A3, A4, A5, A6, A7]]): Arbitrary[HK7[F, A1, A2, A3, A4, A5, A6, A7]] = Arbitrary.derived
    given csv[F[_, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7](using c: Csv[F[A1, A2, A3, A4, A5, A6, A7]]): Csv[HK7[F, A1, A2, A3, A4, A5, A6, A7]] = Csv.derived
    given read[F[_, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7](using r: Read[F[A1, A2, A3, A4, A5, A6, A7]]): Read[HK7[F, A1, A2, A3, A4, A5, A6, A7]] = Read.derived
  }


  case class Test8(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String)
    derives Arbitrary, Csv, Read

  case class TParams8[A1, A2, A3, A4, A5, A6, A7, A8](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5, x5: A6, x6: A7, x7: A8)
    derives Arbitrary, Csv, Read

  case class HK8[F[_, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8](run: F[A1, A2, A3, A4, A5, A6, A7, A8])
  object HK8 {
    given arbitrary[F[_, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8](using a: Arbitrary[F[A1, A2, A3, A4, A5, A6, A7, A8]]): Arbitrary[HK8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Arbitrary.derived
    given csv[F[_, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8](using c: Csv[F[A1, A2, A3, A4, A5, A6, A7, A8]]): Csv[HK8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Csv.derived
    given read[F[_, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8](using r: Read[F[A1, A2, A3, A4, A5, A6, A7, A8]]): Read[HK8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Read.derived
  }


  case class Test9(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean)
    derives Arbitrary, Csv, Read

  case class TParams9[A1, A2, A3, A4, A5, A6, A7, A8, A9](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5, x5: A6, x6: A7, x7: A8, x8: A9)
    derives Arbitrary, Csv, Read

  case class HK9[F[_, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9](run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9])
  object HK9 {
    given arbitrary[F[_, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9](using a: Arbitrary[F[A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Arbitrary[HK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Arbitrary.derived
    given csv[F[_, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9](using c: Csv[F[A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Csv[HK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Csv.derived
    given read[F[_, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9](using r: Read[F[A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Read[HK9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Read.derived
  }


  case class Test10(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int)
    derives Arbitrary, Csv, Read

  case class TParams10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5, x5: A6, x6: A7, x7: A8, x8: A9, x9: A10)
    derives Arbitrary, Csv, Read

  case class HK10[F[_, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10])
  object HK10 {
    given arbitrary[F[_, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](using a: Arbitrary[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Arbitrary[HK10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Arbitrary.derived
    given csv[F[_, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](using c: Csv[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Csv[HK10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Csv.derived
    given read[F[_, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](using r: Read[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Read[HK10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Read.derived
  }


  case class Test11(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String)
    derives Arbitrary, Csv, Read

  case class TParams11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5, x5: A6, x6: A7, x7: A8, x8: A9, x9: A10, x10: A11)
    derives Arbitrary, Csv, Read

  case class HK11[F[_, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11])
  object HK11 {
    given arbitrary[F[_, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](using a: Arbitrary[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Arbitrary[HK11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Arbitrary.derived
    given csv[F[_, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](using c: Csv[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Csv[HK11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Csv.derived
    given read[F[_, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](using r: Read[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Read[HK11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Read.derived
  }


  case class Test12(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean)
    derives Arbitrary, Csv, Read

  case class TParams12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5, x5: A6, x6: A7, x7: A8, x8: A9, x9: A10, x10: A11, x11: A12)
    derives Arbitrary, Csv, Read

  case class HK12[F[_, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12])
  object HK12 {
    given arbitrary[F[_, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](using a: Arbitrary[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Arbitrary[HK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Arbitrary.derived
    given csv[F[_, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](using c: Csv[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Csv[HK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Csv.derived
    given read[F[_, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](using r: Read[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Read[HK12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Read.derived
  }


  case class Test13(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int)
    derives Arbitrary, Csv, Read

  case class TParams13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5, x5: A6, x6: A7, x7: A8, x8: A9, x9: A10, x10: A11, x11: A12, x12: A13)
    derives Arbitrary, Csv, Read

  case class HK13[F[_, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13])
  object HK13 {
    given arbitrary[F[_, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](using a: Arbitrary[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Arbitrary[HK13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Arbitrary.derived
    given csv[F[_, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](using c: Csv[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Csv[HK13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Csv.derived
    given read[F[_, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](using r: Read[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Read[HK13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Read.derived
  }


  case class Test14(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String)
    derives Arbitrary, Csv, Read

  case class TParams14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5, x5: A6, x6: A7, x7: A8, x8: A9, x9: A10, x10: A11, x11: A12, x12: A13, x13: A14)
    derives Arbitrary, Csv, Read

  case class HK14[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14])
  object HK14 {
    given arbitrary[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](using a: Arbitrary[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Arbitrary[HK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Arbitrary.derived
    given csv[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](using c: Csv[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Csv[HK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Csv.derived
    given read[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](using r: Read[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Read[HK14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Read.derived
  }


  case class Test15(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean)
    derives Arbitrary, Csv, Read

  case class TParams15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5, x5: A6, x6: A7, x7: A8, x8: A9, x9: A10, x10: A11, x11: A12, x12: A13, x13: A14, x14: A15)
    derives Arbitrary, Csv, Read

  case class HK15[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15])
  object HK15 {
    given arbitrary[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](using a: Arbitrary[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Arbitrary[HK15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Arbitrary.derived
    given csv[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](using c: Csv[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Csv[HK15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Csv.derived
    given read[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](using r: Read[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Read[HK15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Read.derived
  }


  case class Test16(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int)
    derives Arbitrary, Csv, Read

  case class TParams16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5, x5: A6, x6: A7, x7: A8, x8: A9, x9: A10, x10: A11, x11: A12, x12: A13, x13: A14, x14: A15, x15: A16)
    derives Arbitrary, Csv, Read

  case class HK16[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16])
  object HK16 {
    given arbitrary[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](using a: Arbitrary[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Arbitrary[HK16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Arbitrary.derived
    given csv[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](using c: Csv[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Csv[HK16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Csv.derived
    given read[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](using r: Read[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Read[HK16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Read.derived
  }


  case class Test17(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String)
    derives Arbitrary, Csv, Read

  case class TParams17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5, x5: A6, x6: A7, x7: A8, x8: A9, x9: A10, x10: A11, x11: A12, x12: A13, x13: A14, x14: A15, x15: A16, x16: A17)
    derives Arbitrary, Csv, Read

  case class HK17[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17])
  object HK17 {
    given arbitrary[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](using a: Arbitrary[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Arbitrary[HK17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Arbitrary.derived
    given csv[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](using c: Csv[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Csv[HK17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Csv.derived
    given read[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](using r: Read[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Read[HK17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Read.derived
  }


  case class Test18(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String, x17: Boolean)
    derives Arbitrary, Csv, Read

  case class TParams18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5, x5: A6, x6: A7, x7: A8, x8: A9, x9: A10, x10: A11, x11: A12, x12: A13, x13: A14, x14: A15, x15: A16, x16: A17, x17: A18)
    derives Arbitrary, Csv, Read

  case class HK18[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18])
  object HK18 {
    given arbitrary[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](using a: Arbitrary[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Arbitrary[HK18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Arbitrary.derived
    given csv[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](using c: Csv[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Csv[HK18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Csv.derived
    given read[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](using r: Read[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Read[HK18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Read.derived
  }


  case class Test19(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String, x17: Boolean, x18: Int)
    derives Arbitrary, Csv, Read

  case class TParams19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5, x5: A6, x6: A7, x7: A8, x8: A9, x9: A10, x10: A11, x11: A12, x12: A13, x13: A14, x14: A15, x15: A16, x16: A17, x17: A18, x18: A19)
    derives Arbitrary, Csv, Read

  case class HK19[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19])
  object HK19 {
    given arbitrary[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](using a: Arbitrary[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Arbitrary[HK19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Arbitrary.derived
    given csv[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](using c: Csv[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Csv[HK19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Csv.derived
    given read[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](using r: Read[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Read[HK19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Read.derived
  }


  case class Test20(x0: Int, x1: String, x2: Boolean, x3: Int, x4: String, x5: Boolean, x6: Int, x7: String, x8: Boolean, x9: Int, x10: String, x11: Boolean, x12: Int, x13: String, x14: Boolean, x15: Int, x16: String, x17: Boolean, x18: Int, x19: String)
    derives Arbitrary, Csv, Read

  case class TParams20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](x0: A1, x1: A2, x2: A3, x3: A4, x4: A5, x5: A6, x6: A7, x7: A8, x8: A9, x9: A10, x10: A11, x11: A12, x12: A13, x13: A14, x14: A15, x15: A16, x16: A17, x17: A18, x18: A19, x19: A20)
    derives Arbitrary, Csv, Read

  case class HK20[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](run: F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20])
  object HK20 {
    given arbitrary[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](using a: Arbitrary[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Arbitrary[HK20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Arbitrary.derived
    given csv[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](using c: Csv[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Csv[HK20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Csv.derived
    given read[F[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](using r: Read[F[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Read[HK20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Read.derived
  }

}

sealed trait BaseDerivationTest { self: Properties =>
  import typeclasses.{Csv, Read}

  final def proof[A: Arbitrary: Csv: Show](label: String)(using r: Read[A]) =
    property(label) = forAllNoShrink((a: A) => {
      (implicitly[Csv[A]].toCsv(a).nonEmpty :| "CSV output was empty") &&
      // can't really test `Read` because the implementations don't work for nested values
      (implicitly[Show[A]].show(a).nonEmpty :| "show/read was not Eq")
    })
}

object DerivationTest1 extends Properties("Derivation1"), BaseDerivationTest {
  import typeclasses.given
  import testTypes.*

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


  proof[Test4]("Test4")
  proof[TParams4[String, String, String, String]]("TParams4")
  proof[HK4[TParams4, String, String, String, String]]("HK4")


  proof[Test5]("Test5")
  proof[TParams5[String, String, String, String, String]]("TParams5")
  proof[HK5[TParams5, String, String, String, String, String]]("HK5")


  proof[Test6]("Test6")
  proof[TParams6[String, String, String, String, String, String]]("TParams6")
  proof[HK6[TParams6, String, String, String, String, String, String]]("HK6")


  proof[Test7]("Test7")
  proof[TParams7[String, String, String, String, String, String, String]]("TParams7")
  proof[HK7[TParams7, String, String, String, String, String, String, String]]("HK7")

}

object DerivationTest2 extends Properties("Derivation"), BaseDerivationTest {
  import typeclasses.{Csv, Read, given}
  import testTypes.*

  
  proof[Test8]("Test8")
  proof[TParams8[String, String, String, String, String, String, String, String]]("TParams8")
  proof[HK8[TParams8, String, String, String, String, String, String, String, String]]("HK8")


  proof[Test9]("Test9")
  proof[TParams9[String, String, String, String, String, String, String, String, String]]("TParams9")
  proof[HK9[TParams9, String, String, String, String, String, String, String, String, String]]("HK9")


  proof[Test10]("Test10")
  proof[TParams10[String, String, String, String, String, String, String, String, String, String]]("TParams10")
  proof[HK10[TParams10, String, String, String, String, String, String, String, String, String, String]]("HK10")


  proof[Test11]("Test11")
  proof[TParams11[String, String, String, String, String, String, String, String, String, String, String]]("TParams11")
  proof[HK11[TParams11, String, String, String, String, String, String, String, String, String, String, String]]("HK11")


  proof[Test12]("Test12")
  proof[TParams12[String, String, String, String, String, String, String, String, String, String, String, String]]("TParams12")
  proof[HK12[TParams12, String, String, String, String, String, String, String, String, String, String, String, String]]("HK12")


  proof[Test13]("Test13")
  proof[TParams13[String, String, String, String, String, String, String, String, String, String, String, String, String]]("TParams13")
  proof[HK13[TParams13, String, String, String, String, String, String, String, String, String, String, String, String, String]]("HK13")


  proof[Test14]("Test14")
  proof[TParams14[String, String, String, String, String, String, String, String, String, String, String, String, String, String]]("TParams14")
  proof[HK14[TParams14, String, String, String, String, String, String, String, String, String, String, String, String, String, String]]("HK14")


  proof[Test15]("Test15")
  proof[TParams15[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String]]("TParams15")
  proof[HK15[TParams15, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String]]("HK15")


  proof[Test16]("Test16")
  proof[TParams16[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String]]("TParams16")
  proof[HK16[TParams16, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String]]("HK16")


  proof[Test17]("Test17")
  proof[TParams17[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String]]("TParams17")
  proof[HK17[TParams17, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String]]("HK17")


  proof[Test18]("Test18")
  proof[TParams18[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String]]("TParams18")
  proof[HK18[TParams18, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String]]("HK18")


  proof[Test19]("Test19")
  proof[TParams19[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String]]("TParams19")
  proof[HK19[TParams19, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String]]("HK19")


  proof[Test20]("Test20")
  proof[TParams20[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String]]("TParams20")
  proof[HK20[TParams20, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String]]("HK20")

}
