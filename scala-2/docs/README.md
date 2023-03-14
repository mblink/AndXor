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

```scala mdoc:silent
import andxor._
import cats.{~>, Id, Show}
```

#### Construct an AndXor

```scala mdoc
val SIS = AndXor[String, Int, List[String]]
```

#### Lift values into a coproduct

```scala mdoc
val cop1 = SIS.inj("foo")
val cop2 = SIS.inj(2)
val cop3 = SIS.inj(List("bar", "baz"))
```

#### Lift values into a product

```scala mdoc
val listProd = SIS.lift(List(4)) |+| SIS.lift(List("foo")) |+| SIS.lift(List(List("bar")))
val optionProd = SIS.lift(Option(4)) |+| SIS.lift(Option("foo")) |+| SIS.lift(Option(List("bar")))
```

#### Derive typeclasses

```scala mdoc
// `Decidable[Show]` is provided for derivation over coproducts
implicit val showCop = SIS.derivingId[Show].choose
List(cop1, cop2, cop3).show

// define a `Divide[Show]` for derivation over products
implicit val divideShow: Divide[Show] = new Divide[Show] {
  def contramap[A, B](fa: Show[A])(f: B => A): Show[B] = Show.show(b => fa.show(f(b)))
  def divide2[A1, A2, Z](a1: Show[A1], a2: Show[A2])(f: Z => (A1, A2)): Show[Z] =
    Show.show { z =>
      val (x, y) = f(z)
      a1.show(x) + ", " + a2.show(y)
    }
}
implicit val showListProd = SIS.deriving[Show, List].divide
implicit val showOptionProd = SIS.deriving[Show, Option].divide
(listProd.show, optionProd.show)
```

#### Convert between `F[_]`s using a `~>`

```scala mdoc
FFunctor[SIS.Prod].map(listProd)(new (List ~> Option) {
  def apply[A](l: List[A]): Option[A] = l.headOption
})
FFunctor[SIS.Prod].map(optionProd)(new (Option ~> List) {
  def apply[A](o: Option[A]): List[A] = o.toList
})
```

#### Sequence a `Prod` or `Cop` into `Id`

```scala mdoc
FTraverseCop[SIS.Cop].sequence[Id, Option](SIS.inj(Option("foo")))
FTraverseProd[SIS.Prod].sequence[Id, Option](SIS.Prod((Option("foo"), Option(1), Option(List("bar")))))
```

#### Map given index of Cop or Prod

```scala mdoc
import andxor.MapN.syntax._
SIS.inj(Option(2)).run.map1(_.map(_.length)).map2(_.map(_.toString + "!"))
SIS.lift(Option("foo")).run.map2(_.map(_.toString + "!")).map1(_.map(_.length))
```

#### Map a unique type at an arbitrary index of a Cop or Prod

```scala mdoc
SIS.lift(2).run.mapAt((_: Int) + 3)
SIS.inj(List("Hello ", "Goodbye cruel ")).run.mapAt((_: List[String]).map(_ + "world"))
```

#### Extract specific type from Cop or Prod

```scala mdoc
SIS.extractC[Option, Option[String]](SIS.inj(Option("foo")))
SIS.extractC[Option, Option[Int]](SIS.inj(Option("foo")))
SIS.extractP[Option, Option[String]](SIS.lift(Option("foo")))
SIS.extractP[Option, Option[Int]](SIS.lift(Option(1)))
```
