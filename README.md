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
scala> import andxor.{AndXorF3, AndXor3, Decidable, Inj}
import andxor.{AndXorF3, AndXor3, Decidable, Inj}

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

scala> val SIS = AndXor3[String, Int, List[String]]
SIS: andxor.AndXor3[String,Int,List[String]] = andxor.AndXor3$$anon$1@2f4dc0f5

scala> import SIS.instances._ // for inject instances
import SIS.instances._

scala> implicit val ds: Decidable[Show] = new Decidable[Show] { def choose2[Z, A1, A2](a1: => Show[A1], a2: =>Show[A2])(f: Z => (A1 \/ A2)): Show[Z] = Show.show[Z]((z: Z) => f(z).fold(a1.show(_), a2.show(_))) }
ds: andxor.Decidable[scalaz.Show] = $anon$1@68f4c415

scala> SIS.combine[Show].choose.show(SIS.inj("foo"))
res0: scalaz.Cord = "foo"

scala> SIS.combine[Show].choose.show(SIS.inj(2))
res1: scalaz.Cord = 2

scala> SIS.combine[Show].choose.show(SIS.inj(List("bar", "baz")))
res2: scalaz.Cord = ["bar","baz"]

scala> // lift into monoidal product
     | val SISF = AndXorF3[String, Int, List[String]]
SISF: andxor.AndXorF3[String,Int,List[String]] = andxor.AndXorF3$$anon$5@2e171711

scala> val SISL = SISF[List]
SISL: SISF.Repr[List] = andxor.AndXorF3$$anon$3@43b364d6

scala> import SISL.instances._ // for inject instances
import SISL.instances._

scala> val ls = SISL.lift(List(4)) |+| SISL.lift(List("foo")) |+| SISL.lift(List(List("bar")))
ls: SISL.Prod = (List(foo),List(4),List(List(bar)))

scala> val SISO = SISF[Option]
SISO: SISF.Repr[Option] = andxor.AndXorF3$$anon$3@40c3b391

scala> import SISO.instances._ // for inject instances
import SISO.instances._

scala> val os = SISO.lift(Option(4)) |+| SISO.lift(Option("foo")) |+| SISO.lift(Option(List("bar")))
os: SISO.Prod = (Some(foo),Some(4),Some(List(bar)))

scala> // convert between F[_]s using a ~>
     | val l2o = new (List ~> Option) { def apply[A](l: List[A]): Option[A] = l.headOption }
l2o: List ~> Option = $anon$1@2fde1897

scala> SISL.transformP(l2o)(ls)
res5: (Option[String], Option[Int], Option[List[String]]) = (Some(foo),Some(4),Some(List(bar)))

scala> val o2l = new (Option ~> List) { def apply[A](o: Option[A]): List[A] = o.toList }
o2l: Option ~> List = $anon$1@3d2bfb23

scala> SISO.transformP(o2l)(os)
res6: (List[String], List[Int], List[List[String]]) = (List(foo),List(4),List(List(bar)))

scala> // map given index of Cop or Prod
     | import andxor.MapN.syntax._
import andxor.MapN.syntax._

scala> SISO.inj(Option(2)).map1(_.map(_.length)).map2(_.map(_.toString ++ "!"))
res8: Option[Int] \/ (Option[String] \/ Option[List[String]]) = \/-(-\/(Some(2!)))

scala> SISO.lift(Option("foo")).map2(_.map(_.toString ++ "!")).map1(_.map(_.length))
res9: (Option[Int], Option[String], Option[List[String]]) = (Some(3),None,None)

scala> // extract specific type from Cop or Prod
     | SISO.extractC[Option[String]](SISO.inj(Option("foo")))
res11: Option[Option[String]] = Some(Some(foo))

scala> SISO.extractC[Option[Int]](SISO.inj(Option("foo")))
res12: Option[Option[Int]] = None

scala> SISO.extractP[Option[String]](SISO.lift(Option("foo")))
res13: Option[String] = Some(foo)

scala> SISO.extractP[Option[Int]](SISO.lift(Option(1)))
res14: Option[Int] = Some(1)

scala> // convert a Prod to a List[Cop]
     | SISL.toListP((List("foo"), List(1), List(List("bar"))))
res16: List[SISL.Cop] = List(-\/(List(foo)), \/-(-\/(List(1))), \/-(\/-(List(List(bar)))))

scala> // inject into applicative
     | Inj.inject[List[SISO.Cop], Option[String]](Some("foo"))
res18: List[SISO.Cop] = List(-\/(Some(foo)))

scala> Inj.inject[Option[SISO.Cop], Option[Int]](Some(1))
res19: Option[SISO.Cop] = Some(\/-(-\/(Some(1))))
```
