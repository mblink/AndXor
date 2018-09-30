Inspired by scalaz-deriving.

Provides macroless composition of covariant or contravariant
typeclasses for product and coproduct types.

Given a set of types, produces a representation of Coproduct as nested disjunctions,
as well as Product as a tuple. Also provides an `inj` method for lifting a compatible type
into a Coproduct representation, and a `lift` method for lifting a compatible type
into a Product representation when that Product is monoidal.

With an instance of Decidable, Alt, Divide, or Apply for a given typeclass,
provides an instance for the corresponding Coproduct, or Product respectively.

```scala
scala> import ldr.{LDRF3, LDR3}
import ldr.{LDRF3, LDR3}

scala> import ldr.Decidable
import ldr.Decidable

scala> import scalaz.std.anyVal._
import scalaz.std.anyVal._

scala> import scalaz.std.list._
import scalaz.std.list._

scala> import scalaz.std.option._
import scalaz.std.option._

scala> import scalaz.std.string._
import scalaz.std.string._

scala> import scalaz.std.tuple._
import scalaz.std.tuple._

scala> import scalaz.syntax.monoid._
import scalaz.syntax.monoid._

scala> import scalaz.syntax.either._
import scalaz.syntax.either._

scala> import scalaz.{Show, \/, ~>}
import scalaz.{Show, $bslash$div, $tilde$greater}

scala> val SIS = LDR3[String, Int, List[String]]
SIS: ldr.LDR3[String,Int,List[String]] = ldr.LDR3$$anon$1@408e3270

scala> import SIS.instances._ // for inject instances
import SIS.instances._

scala> implicit val ds: Decidable[Show] = new Decidable[Show] { def choose2[Z, A1, A2](a1: => Show[A1], a2: =>Show[A2])(f: Z => (A1 \/ A2)): Show[Z] = Show.show[Z]((z: Z) => f(z).fold(a1.show(_), a2.show(_))) }
ds: ldr.Decidable[scalaz.Show] = $anon$1@54a7a903

scala> SIS.combine[Show].choose.show(SIS.inj("foo"))
res0: scalaz.Cord = "foo"

scala> SIS.combine[Show].choose.show(SIS.inj(2))
res1: scalaz.Cord = 2

scala> SIS.combine[Show].choose.show(SIS.inj(List("bar", "baz")))
res2: scalaz.Cord = ["bar","baz"]

scala> // lift into monoidal product
     | val SISF = LDRF3[String, Int, List[String]]
SISF: ldr.LDRF3[String,Int,List[String]] = ldr.LDRF3$$anon$5@46634173

scala> val SISL = SISF[List]
SISL: SISF.Repr[List] = ldr.LDRF3$$anon$3@576420a2

scala> import SISL.instances._ // for inject instances
import SISL.instances._

scala> val ls = SISL.lift(List(4)) |+| SISL.lift(List("foo")) |+| SISL.lift(List(List("bar")))
ls: SISL.Prod = (List(foo),List(4),List(List(bar)))

scala> val SISO = SISF[Option]
SISO: SISF.Repr[Option] = ldr.LDRF3$$anon$3@540f7812

scala> import SISO.instances._ // for inject instances
import SISO.instances._

scala> val os = SISO.lift(Option(4)) |+| SISO.lift(Option("foo")) |+| SISO.lift(Option(List("bar")))
os: SISO.Prod = (Some(foo),Some(4),Some(List(bar)))

scala> // convert between F[_]s using a ~>
     | val l2o = new (List ~> Option) { def apply[A](l: List[A]): Option[A] = l.headOption }
l2o: List ~> Option = $anon$1@42c31c52

scala> SISL.transformP(l2o)(ls)
res5: (Option[String], Option[Int], Option[List[String]]) = (Some(foo),Some(4),Some(List(bar)))

scala> val o2l = new (Option ~> List) { def apply[A](o: Option[A]): List[A] = o.toList }
o2l: Option ~> List = $anon$1@67f0eda1

scala> SISO.transformP(o2l)(os)
res6: (List[String], List[Int], List[List[String]]) = (List(foo),List(4),List(List(bar)))

scala> // map given index of Cop or Prod
     | import ldr.MapN.syntax._
import ldr.MapN.syntax._

scala> SISO.inj(Option(2)).map1(_.map(_.length)).map2(_.map(_.toString ++ "!"))
res8: Option[Int] \/ (Option[String] \/ Option[List[String]]) = \/-(-\/(Some(2!)))

scala> SISO.lift(Option("foo")).map2(_.map(_.toString ++ "!")).map1(_.map(_.length))
res9: (Option[Int], Option[String], Option[List[String]]) = (Some(3),None,None)
```
