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
import cats.{~>, Id, Show}
import cats.instances.int._
import cats.instances.list._
import cats.instances.option._
import cats.instances.string._
import cats.instances.tuple._
import cats.syntax.semigroup._
import cats.syntax.show._
```

#### Construct an AndXor

```scala
val SIS = AndXor[String, Int, List[String]]
// SIS: AndXor3[String, Int, List[String]] = andxor.AndXor3$$anon$7@1db08002
```

#### Lift values into a coproduct

```scala
val cop1 = SIS.inj("foo")
// cop1: SIS.Cop[Id] = Left("foo")
val cop2 = SIS.inj(2)
// cop2: SIS.Cop[Id] = Right(Left(2))
val cop3 = SIS.inj(List("bar", "baz"))
// cop3: SIS.Cop[Id] = Right(Right(List("bar", "baz")))
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
// showCop: Show[SIS.Cop[Id]] = cats.Show$$anon$2@c599fb7
List(cop1, cop2, cop3).show
// res0: String = "List(foo, 2, List(bar, baz))"

// define a `Divide[Show]` for derivation over products
implicit val divideShow: Divide[Show] = new Divide[Show] {
  def contramap[A, B](fa: Show[A])(f: B => A): Show[B] = Show.show(b => fa.show(f(b)))
  def divide2[A1, A2, Z](a1: Show[A1], a2: Show[A2])(f: Z => (A1, A2)): Show[Z] =
    Show.show { z =>
      val (x, y) = f(z)
      a1.show(x) + ", " + a2.show(y)
    }
}
// divideShow: Divide[Show] = repl.Session$App$$anon$1@30da373d
implicit val showListProd = SIS.deriving[Show, List].divide
// showListProd: Show[SIS.Prod[List]] = cats.Show$$anon$2@499d523e
implicit val showOptionProd = SIS.deriving[Show, Option].divide
// showOptionProd: Show[SIS.Prod[Option]] = cats.Show$$anon$2@c8c946
(listProd.show, optionProd.show)
// res1: (String, String) = (
//   "List(foo), List(4), List(List(bar))",
//   "Some(foo), Some(4), Some(List(bar))"
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
// res4: Option[SIS.Cop[Id]] = Some(Left("foo"))
FTraverseProd[SIS.Prod].sequence[Id, Option](SIS.Prod((Option("foo"), Option(1), Option(List("bar")))))
// res5: Option[SIS.Prod[Id]] = Some(("foo", 1, List("bar")))
```

#### Map given index of Cop or Prod

```scala
import andxor.MapN.syntax._
SIS.inj(Option(2)).run.map1(_.map(_.length)).map2(_.map(_.toString + "!"))
// res6: Either[Option[Int], Either[Option[String], Option[List[String]]]] = Right(
//   Left(Some("2!"))
// )
SIS.lift(Option("foo")).run.map2(_.map(_.toString + "!")).map1(_.map(_.length))
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
SIS.inj(List("Hello ", "Goodbye cruel ")).run.mapAt((_: List[String]).map(_ + "world"))
// res9: Either[Id[String], Either[Id[Int], List[String]]] = Right(
//   Right(List("Hello world", "Goodbye cruel world"))
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
