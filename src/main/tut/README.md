Inspired by scalaz-deriving.

Provides macroless composition of covariant or contravariant
typeclasses for product and coproduct types.

Given a set of types, produces a representation of Coproduct as nested disjunctions,
as well as Product as a tuple. Also provides an `inj` method for lifting a compatible type
into a Coproduct representation, and a `lift` method for lifting a compatible type
into a Product representation when that Product is monoidal.

With an instance of Decidable, Alt, Divide, or Apply for a given typeclass,
provides an instance for the corresponding Coproduct, or Product respectively.

```tut
import andxor._
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.std.tuple._
import scalaz.syntax.monoid._
import scalaz.syntax.either._
import scalaz.{Show, \/, ~>}
import scalaz.Id.Id
val SIS = AndXor[String, Int, List[String]]
implicit val ds: Decidable[Show] = new Decidable[Show] {
  def choose2[Z, A1, A2](a1: => Show[A1], a2: =>Show[A2])(f: Z => (A1 \/ A2)): Show[Z] =
    Show.show[Z]((z: Z) => f(z).fold(a1.show(_), a2.show(_)))

  def contramap[A, B](fa: Show[A])(f: B => A): Show[B] =
    Show.show[B]((b: B) => fa.show(f(b)))
}
SIS.derivingId[Show].choose.show(SIS.inj("foo"))
SIS.derivingId[Show].choose.show(SIS.inj(2))
SIS.derivingId[Show].choose.show(SIS.inj(List("bar", "baz")))

// lift into monoidal product
val ls = SIS.lift(List(4)) |+| SIS.lift(List("foo")) |+| SIS.lift(List(List("bar")))
val os = SIS.lift(Option(4)) |+| SIS.lift(Option("foo")) |+| SIS.lift(Option(List("bar")))

// convert between F[_]s using a ~>
val l2o = new (List ~> Option) { def apply[A](l: List[A]): Option[A] = l.headOption }
FFunctor[SIS.Prod].map(ls)(l2o)
val o2l = new (Option ~> List) { def apply[A](o: Option[A]): List[A] = o.toList }
FFunctor[SIS.Prod].map(os)(o2l)

// sequence Cop or Prod to Id
FTraverse[SIS.Cop].sequence[Id, Option](SIS.inj(Option("foo")))
FTraverse[SIS.Prod].sequence[Id, Option](SIS.Prod((Option("foo"), Option(1), Option(List("bar")))))

// map given index of Cop or Prod
import andxor.MapN.syntax._
SIS.inj(Option(2)).run.map1(_.map(_.length)).map2(_.map(_.toString ++ "!"))
SIS.lift(Option("foo")).run.map2(_.map(_.toString ++ "!")).map1(_.map(_.length))

// map a unique type at an arbitrary index of a Cop or Prod
SIS.lift(2).run.mapAt((_: Int) + 3)
SIS.inj(List("Hello ", "Goodbye cruel ")).run.mapAt((_: List[String]).map(_ ++ "world"))

// extract specific type from Cop or Prod
SIS.extractC[Option, Option[String]](SIS.inj(Option("foo")))
SIS.extractC[Option, Option[Int]](SIS.inj(Option("foo")))
SIS.extractP[Option, Option[String]](SIS.lift(Option("foo")))
SIS.extractP[Option, Option[Int]](SIS.lift(Option(1)))
```
