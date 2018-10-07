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
import andxor.{AndXorF3, AndXor3}
import andxor.Decidable
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.std.tuple._
import scalaz.syntax.monoid._
import scalaz.syntax.either._
import scalaz.{Show, \/, ~>}
val SIS = AndXor3[String, Int, List[String]]
import SIS.instances._ // for inject instances
implicit val ds: Decidable[Show] = new Decidable[Show] { def choose2[Z, A1, A2](a1: => Show[A1], a2: =>Show[A2])(f: Z => (A1 \/ A2)): Show[Z] = Show.show[Z]((z: Z) => f(z).fold(a1.show(_), a2.show(_))) }
SIS.combine[Show].choose.show(SIS.inj("foo"))
SIS.combine[Show].choose.show(SIS.inj(2))
SIS.combine[Show].choose.show(SIS.inj(List("bar", "baz")))

// lift into monoidal product
val SISF = AndXorF3[String, Int, List[String]]
val SISL = SISF[List]
import SISL.instances._ // for inject instances
val ls = SISL.lift(List(4)) |+| SISL.lift(List("foo")) |+| SISL.lift(List(List("bar")))
val SISO = SISF[Option]
import SISO.instances._ // for inject instances
val os = SISO.lift(Option(4)) |+| SISO.lift(Option("foo")) |+| SISO.lift(Option(List("bar")))

// convert between F[_]s using a ~>
val l2o = new (List ~> Option) { def apply[A](l: List[A]): Option[A] = l.headOption }
SISL.transformP(l2o)(ls)
val o2l = new (Option ~> List) { def apply[A](o: Option[A]): List[A] = o.toList }
SISO.transformP(o2l)(os)

// map given index of Cop or Prod
import andxor.MapN.syntax._
SISO.inj(Option(2)).map1(_.map(_.length)).map2(_.map(_.toString ++ "!"))
SISO.lift(Option("foo")).map2(_.map(_.toString ++ "!")).map1(_.map(_.length))
```
