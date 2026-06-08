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
import cats.syntax.semigroup._
import cats.syntax.show._
```

#### Construct an AndXor

```scala
val SIS = AndXor[String, Int, List[String]]
// SIS: AndXor3[String, Int, List[String]] = andxor.AndXor3@5d3c0b48
```

#### Lift values into a coproduct

```scala
val cop1: SIS.Cop[Id] = SIS.inj("foo")
// cop1: Either[Id[String], Cop[Id]] = Left("foo")
val cop2: SIS.Cop[Id] = SIS.inj(2)
// cop2: Either[Id[String], Cop[Id]] = Right(Left(2))
val cop3: SIS.Cop[Id] = SIS.inj(List("bar", "baz"))
// cop3: Either[Id[String], Cop[Id]] = Right(Right(List("bar", "baz")))
```

#### Lift values into a product

```scala
val listProd: SIS.Prod[List] = SIS.lift(List(4)) |+| SIS.lift(List("foo")) |+| SIS.lift(List(List("bar")))
// listProd: *:[List[String], Prod[List]] = (
//   List("foo"),
//   List(4),
//   List(List("bar"))
// )
val optionProd: SIS.Prod[Option] = SIS.lift(Option(4)) |+| SIS.lift(Option("foo")) |+| SIS.lift(Option(List("bar")))
// optionProd: *:[Option[String], Prod[[A >: Nothing <: Any] => Option[A]]] = (
//   Some("foo"),
//   Some(4),
//   Some(List("bar"))
// )
```

#### Derive typeclasses

```scala
// `Decidable[Show]` is provided for derivation over coproducts
implicit val showCop: Show[SIS.Cop[Id]] = SIS.derivingId[Show].choose
// showCop: Show[Cop[Id]] = cats.Show$$$Lambda/0x0000000303596118@5405b270
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
// divideShow: Divide[[T >: Nothing <: Any] => Show[T]] = repl.MdocSession$MdocApp$$anon$9@23f9cbf1
implicit val showListProd: Show[SIS.Prod[List]] = SIS.deriving[Show, List].divide
// showListProd: Show[Prod[List]] = cats.Show$$$Lambda/0x0000000303596118@49aae0b9
implicit val showOptionProd: Show[SIS.Prod[Option]] = SIS.deriving[Show, Option].divide
// showOptionProd: Show[Prod[[A >: Nothing <: Any] => Option[A]]] = cats.Show$$$Lambda/0x0000000303596118@4335d487
(listProd.show, optionProd.show)
// res1: Tuple2[String, String] = (
//   "List(foo), List(4), List(List(bar))",
//   "Some(foo), Some(4), Some(List(bar))"
// )
```

#### Convert between `F[_]`s using a `~>`

```scala
FFunctor[SIS.Prod].map(listProd)(new (List ~> Option) {
  def apply[A](l: List[A]): Option[A] = l.headOption
})
// res2: *:[Option[String], Prod[[A >: Nothing <: Any] => Option[A]]] = (
//   Some("foo"),
//   Some(4),
//   Some(List("bar"))
// )
FFunctor[SIS.Prod].map(optionProd)(new (Option ~> List) {
  def apply[A](o: Option[A]): List[A] = o.toList
})
// res3: *:[List[String], Prod[[A >: Nothing <: Any] => List[A]]] = (
//   List("foo"),
//   List(4),
//   List(List("bar"))
// )
```

#### Sequence a `Prod` or `Cop` into `Id`

```scala
FTraverseCop[SIS.Cop].sequence[Id, Option](SIS.inj(Option("foo")))
// res4: Option[Cop[Id]] = Some(Left("foo"))
FTraverseProd[SIS.Prod].sequence[Id, Option]((Option("foo"), Option(1), Option(List("bar"))))
// res5: Option[Prod[Id]] = Some(("foo", 1, List("bar")))
```

#### Map given index of Cop or Prod

```scala
val mappedCop = {
  import andxor.either._
  SIS.inj(Option(2)).map1(_.map(_.length)).map2(_.map(_.toString + "!"))
}
// mappedCop: Either[Option[Int], Either[Option[String], Cop[Option]]] = Right(
//   Left(Some("2!"))
// )

val mappedProd = {
  import andxor.tuple._
  SIS.lift(Option("foo")).map2(_.map(_.toString + "!")).map1(_.map(_.length))
}
// mappedProd: *:[Option[Int], *:[Option[String], *:[Option[List[String]], EmptyTuple]]] = (
//   Some(3),
//   None,
//   None
// )
```

#### Extract specific type from Cop or Prod

```scala
SIS.extractC[Option, Option[String]](SIS.inj(Option("foo")))
// res6: Option[Option[String]] = Some(Some("foo"))
SIS.extractC[Option, Option[Int]](SIS.inj(Option("foo")))
// res7: Option[Option[Int]] = None
SIS.extractP[Option, Option[String]](SIS.lift(Option("foo")))
// res8: Option[String] = Some("foo")
SIS.extractP[Option, Option[Int]](SIS.lift(Option(1)))
// res9: Option[Int] = Some(1)
```
