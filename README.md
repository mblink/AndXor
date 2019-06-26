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
scala> import andxor._
import andxor._

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

scala> import scalaz.Id.Id
import scalaz.Id.Id

scala> val SIS = AndXor[String, Int, List[String]]
SIS: andxor.AndXor3[String,Int,List[String]] = andxor.AndXor3$$anon$4@41f85572

scala> implicit val ds: Decidable[Show] = new Decidable[Show] {
     |   def choose2[Z, A1, A2](a1: => Show[A1], a2: =>Show[A2])(f: Z => (A1 \/ A2)): Show[Z] =
     |     Show.show[Z]((z: Z) => f(z).fold(a1.show(_), a2.show(_)))
     | 
     |   def contramap[A, B](fa: Show[A])(f: B => A): Show[B] =
     |     Show.show[B]((b: B) => fa.show(f(b)))
     | }
ds: andxor.Decidable[scalaz.Show] = $anon$1@a0ca3f4

scala> SIS.derivingId[Show].choose.show(SIS.inj("foo"))
res0: scalaz.Cord = "foo"

scala> SIS.derivingId[Show].choose.show(SIS.inj(2))
res1: scalaz.Cord = 2

scala> SIS.derivingId[Show].choose.show(SIS.inj(List("bar", "baz")))
res2: scalaz.Cord = ["bar","baz"]

scala> // lift into monoidal product
     | val ls = SIS.lift(List(4)) |+| SIS.lift(List("foo")) |+| SIS.lift(List(List("bar")))
ls: SIS.Prod[List] = (List(foo),List(4),List(List(bar)))

scala> val os = SIS.lift(Option(4)) |+| SIS.lift(Option("foo")) |+| SIS.lift(Option(List("bar")))
os: SIS.Prod[Option] = (Some(foo),Some(4),Some(List(bar)))

scala> // convert between F[_]s using a ~>
     | val l2o = new (List ~> Option) { def apply[A](l: List[A]): Option[A] = l.headOption }
l2o: List ~> Option = $anon$1@3f0083e7

scala> FFunctor[SIS.Prod].map(ls)(l2o)
res5: SIS.Prod[Option] = (Some(foo),Some(4),Some(List(bar)))

scala> val o2l = new (Option ~> List) { def apply[A](o: Option[A]): List[A] = o.toList }
o2l: Option ~> List = $anon$1@1bf93246

scala> FFunctor[SIS.Prod].map(os)(o2l)
res6: SIS.Prod[List] = (List(foo),List(4),List(List(bar)))

scala> // sequence Cop or Prod to Id
     | FTraverse[SIS.Cop].sequence[Id, Option](SIS.inj(Option("foo")))
res8: Option[SIS.Cop[scalaz.Id.Id]] = Some(-\/(foo))

scala> FTraverse[SIS.Prod].sequence[Id, Option](SIS.Prod((Option("foo"), Option(1), Option(List("bar")))))
res9: Option[SIS.Prod[scalaz.Id.Id]] = Some((foo,1,List(bar)))

scala> // map given index of Cop or Prod
     | import andxor.MapN.syntax._
import andxor.MapN.syntax._

scala> SIS.inj(Option(2)).run.map1(_.map(_.length)).map2(_.map(_.toString ++ "!"))
res11: Option[Int] \/ (Option[String] \/ AnyRef{type T[F[_]] >: F[List[String]] <: F[List[String]]}#T[Option]) = \/-(-\/(Some(2!)))

scala> SIS.lift(Option("foo")).run.map2(_.map(_.toString ++ "!")).map1(_.map(_.length))
res12: (Option[Int], Option[String], AnyRef{type T[F[_]] >: F[List[String]] <: F[List[String]]}#T[Option]) = (Some(3),None,None)

scala> // map a unique type at an arbitrary index of a Cop or Prod
     | SIS.lift(2).run.mapAt((_: Int) + 3)
res14: (AnyRef{type T[F[_]] >: F[String] <: F[String]}#T[scalaz.Id.Id], Int, AnyRef{type T[F[_]] >: F[List[String]] <: F[List[String]]}#T[scalaz.Id.Id]) = ("",5,List())

scala> SIS.inj(List("Hello ", "Goodbye cruel ")).run.mapAt((_: List[String]).map(_ ++ "world"))
res15: List[String] \/ (AnyRef{type T[F[_]] >: F[Int] <: F[Int]}#T[List] \/ AnyRef{type T[F[_]] >: F[List[String]] <: F[List[String]]}#T[List]) = -\/(List(Hello world, Goodbye cruel world))

scala> // extract specific type from Cop or Prod
     | SIS.extractC[Option, Option[String]](SIS.inj(Option("foo")))
res17: Option[Option[String]] = Some(Some(foo))

scala> SIS.extractC[Option, Option[Int]](SIS.inj(Option("foo")))
res18: Option[Option[Int]] = None

scala> SIS.extractP[Option, Option[String]](SIS.lift(Option("foo")))
res19: Option[String] = Some(foo)

scala> SIS.extractP[Option, Option[Int]](SIS.lift(Option(1)))
res20: Option[Int] = Some(1)
```
