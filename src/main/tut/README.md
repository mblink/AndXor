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
import andxor.{AndXor, Decidable}
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.std.tuple._
import scalaz.syntax.monoid._
import scalaz.syntax.either._
import scalaz.{Show, \/, ~>}
val SIS = AndXor.build[String, Int, List[String]]
implicit val ds: Decidable[Show] = new Decidable[Show] {
  def choose2[Z, A1, A2](a1: => Show[A1], a2: =>Show[A2])(f: Z => (A1 \/ A2)): Show[Z] =
    Show.show[Z]((z: Z) => f(z).fold(a1.show(_), a2.show(_)))

  def contramap[A, B](fa: Show[A])(f: B => A): Show[B] =
    Show.show[B]((b: B) => fa.show(f(b)))
}
SIS.combine[Show].choose.show(SIS.inj("foo": scalaz.Id.Id[String]))
SIS.combine[Show].choose.show(SIS.inj(2))
SIS.combine[Show].choose.show(SIS.inj(List("bar", "baz")))

// lift into monoidal product
val SISF = AndXor.buildF[String, Int, List[String]]
val SISL = SISF[List]
val ls = SISL.lift(List(4)) |+| SISL.lift(List("foo")) |+| SISL.lift(List(List("bar")))
val SISO = SISF[Option]
val os = SISO.lift(Option(4)) |+| SISO.lift(Option("foo")) |+| SISO.lift(Option(List("bar")))

// convert between F[_]s using a ~>
val l2o = new (List ~> Option) { def apply[A](l: List[A]): Option[A] = l.headOption }
SISL.transformP(l2o)(ls)
val o2l = new (Option ~> List) { def apply[A](o: Option[A]): List[A] = o.toList }
SISO.transformP(o2l)(os)

// sequence Cop or Prod to Id
SISO.sequenceC(SISO.inj(Option("foo")))
SISO.sequenceP(SISO.Prod((Option("foo"), Option(1), Option(List("bar")))))

// map given index of Cop or Prod
import andxor.MapN.syntax._
SISO.inj(Option(2)).run.map1(_.map(_.length)).map2(_.map(_.toString ++ "!"))
SISO.lift(Option("foo")).run.map2(_.map(_.toString ++ "!")).map1(_.map(_.length))

// extract specific type from Cop or Prod
SISO.extractC[Option[String]](SISO.inj(Option("foo")))
SISO.extractC[Option[Int]](SISO.inj(Option("foo")))
SISO.extractP[Option[String]](SISO.lift(Option("foo")))
SISO.extractP[Option[Int]](SISO.lift(Option(1)))

// substitute F[_] for G[_] at a specific index
SIS.subst2[Option]
```
