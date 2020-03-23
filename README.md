Inspired by scalaz-deriving.

Provides macroless composition of covariant or contravariant
typeclasses for product and coproduct types.

Given a set of types, produces a representation of Coproduct as nested disjunctions,
as well as Product as a tuple. Also provides an `inj` method for lifting a compatible type
into a Coproduct representation, and a `lift` method for lifting a compatible type
into a Product representation when that Product is monoidal.

With an instance of Decidable, Alt, Divide, or Apply for a given typeclass,
provides an instance for the corresponding Coproduct, or Product respectively.

### Usage

#### Imports

```scala
import andxor._
import scalaz.{Show, ~>}
import scalaz.Id.Id
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.std.tuple._
import scalaz.syntax.monoid._
import scalaz.syntax.id._
import scalaz.syntax.show._
```

#### Construct an AndXor

```scala
val SIS = AndXor[String, Int, List[String]]
// SIS: AndXor3[String, Int, List[String]] = andxor.AndXor3$$anon$7@378564ef
```

#### Lift values into a coproduct

```scala
val cop1 = SIS.inj("foo")
// cop1: SIS.Cop[Id] = -\/("foo")
val cop2 = SIS.inj(2)
// cop2: SIS.Cop[Id] = \/-(-\/(2))
val cop3 = SIS.inj(List("bar", "baz"))
// cop3: SIS.Cop[Id] = \/-(\/-(List("bar", "baz")))
```

#### Lift values into a product

```scala
val listProd = SIS.lift(List(4)) |+| SIS.lift(List("foo")) |+| SIS.lift(List(List("bar")))
// listProd: SIS.Prod[List] = (List("foo"), List(4), List(List("bar")))
val optionProd = SIS.lift(Option(4)) |+| SIS.lift(Option("foo")) |+| SIS.lift(Option(List("bar")))
// optionProd: SIS.Prod[Option] = (Some("foo"), Some(4), Some(List("bar")))
```

#### Derive typeclasses

```scala
// `Decidable[Show]` is provided for derivation over coproducts
implicit val showCop = SIS.derivingId[Show].choose
// showCop: Show[SIS.Cop[Id]] = scalaz.Show$$anon$5@3cec797c
List(cop1, cop2, cop3).shows
// res0: String = "[\"foo\",2,[\"bar\",\"baz\"]]"

// define a `Divide[Show]` for derivation over products
implicit val divideShow: Divide[Show] = new Divide[Show] {
  def contramap[A, B](fa: Show[A])(f: B => A): Show[B] = Show.show(b => fa.show(f(b)))
  def divide2[A1, A2, Z](a1: => Show[A1], a2: => Show[A2])(f: Z => (A1, A2)): Show[Z] =
    Show.shows(z => f(z) |> { case (x, y) => a1.shows(x) ++ ", " ++ a2.shows(y) })
}
// divideShow: Divide[Show] = repl.Session$App$$anon$1@6b6c01a0
implicit val showListProd = SIS.deriving[Show, List].divide
// showListProd: Show[SIS.Prod[List]] = scalaz.Show$$anon$6@3d621e3a
implicit val showOptionProd = SIS.deriving[Show, Option].divide
// showOptionProd: Show[SIS.Prod[Option]] = scalaz.Show$$anon$6@2cb93619
(listProd.shows, optionProd.shows)
// res1: (String, String) = (
//   "[\"foo\"], [4], [[\"bar\"]]",
//   "Some(\"foo\"), Some(4), Some([\"bar\"])"
// )
```

#### Convert between `F[_]`s using a `~>`

```scala
FFunctor[SIS.Prod].map(listProd)(new (List ~> Option) {
  def apply[A](l: List[A]): Option[A] = l.headOption
})
// res2: SIS.Prod[Option] = (Some("foo"), Some(4), Some(List("bar")))
FFunctor[SIS.Prod].map(optionProd)(new (Option ~> List) {
  def apply[A](o: Option[A]): List[A] = o.toList
})
// res3: SIS.Prod[List] = (List("foo"), List(4), List(List("bar")))
```

#### Sequence a `Prod` or `Cop` into `Id`

```scala
FTraverseCop[SIS.Cop].sequence[Id, Option](SIS.inj(Option("foo")))
// res4: Option[SIS.Cop[Id]] = Some(-\/("foo"))
FTraverseProd[SIS.Prod].sequence[Id, Option](SIS.Prod((Option("foo"), Option(1), Option(List("bar")))))
// res5: Option[SIS.Prod[Id]] = Some(("foo", 1, List("bar")))
```

#### Map given index of Cop or Prod

```scala
import andxor.MapN.syntax._
SIS.inj(Option(2)).run.map1(_.map(_.length)).map2(_.map(_.toString ++ "!"))
// res6: scalaz.\/[Option[Int], scalaz.\/[Option[String], Option[List[String]]]] = \/-(
//   -\/(Some("2!"))
// )
SIS.lift(Option("foo")).run.map2(_.map(_.toString ++ "!")).map1(_.map(_.length))
// res7: (Option[Int], Option[String], Option[List[String]]) = (
//   Some(3),
//   None,
//   None
// )
```

#### Map a unique type at an arbitrary index of a Cop or Prod

```scala
SIS.lift(2).run.mapAt((_: Int) + 3)
// res8: (Id[String], Int, Id[List[String]]) = ("", 5, List())
SIS.inj(List("Hello ", "Goodbye cruel ")).run.mapAt((_: List[String]).map(_ ++ "world"))
// res9: scalaz.\/[Id[String], scalaz.\/[Id[Int], List[String]]] = \/-(
//   \/-(List("Hello world", "Goodbye cruel world"))
// )
```

#### Extract specific type from Cop or Prod

```scala
SIS.extractC[Option, Option[String]](SIS.inj(Option("foo")))
// res10: Option[Option[String]] = Some(Some("foo"))
SIS.extractC[Option, Option[Int]](SIS.inj(Option("foo")))
// res11: Option[Option[Int]] = None
SIS.extractP[Option, Option[String]](SIS.lift(Option("foo")))
// res12: Option[String] = Some("foo")
SIS.extractP[Option, Option[Int]](SIS.lift(Option(1)))
// res13: Option[Int] = Some(1)
```
