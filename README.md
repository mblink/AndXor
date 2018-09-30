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
scala> import ldr.{LDRK3, LDR3}
import ldr.{LDRK3, LDR3}

scala> import ldr.Decidable
import ldr.Decidable

scala> import scalaz.std.list._
import scalaz.std.list._

scala> import scalaz.std.anyVal._
import scalaz.std.anyVal._

scala> import scalaz.std.string._
import scalaz.std.string._

scala> import scalaz.std.tuple._
import scalaz.std.tuple._

scala> import scalaz.syntax.monoid._
import scalaz.syntax.monoid._

scala> import scalaz.syntax.either._
import scalaz.syntax.either._

scala> import scalaz.{Show, \/}
import scalaz.{Show, $bslash$div}

scala> val SIS = LDR3[String, Int, List[String]]
SIS: ldr.LDR3[String,Int,List[String]] = ldr.LDR3$$anon$1@11c208d7

scala> import SIS.instances._ // for inject instances
import SIS.instances._

scala> implicit val ds: Decidable[Show] = new Decidable[Show] { def choose2[Z, A1, A2](a1: => Show[A1], a2: =>Show[A2])(f: Z => (A1 \/ A2)): Show[Z] = Show.show[Z]((z: Z) => f(z).fold(a1.show(_), a2.show(_))) }
ds: ldr.Decidable[scalaz.Show] = $anon$1@42577f0e

scala> SIS.combine[Show].choose.show(SIS.inj("foo"))
res0: scalaz.Cord = "foo"

scala> SIS.combine[Show].choose.show(SIS.inj(2))
res1: scalaz.Cord = 2

scala> SIS.combine[Show].choose.show(SIS.inj(List("bar", "baz")))
res2: scalaz.Cord = ["bar","baz"]

scala> // lift into monoidal product
     | val SISL = LDRK3[List, String, Int, List[String]]
SISL: ldr.LDRK3[List,String,Int,List[String]] = ldr.LDRK3$$anon$2@2dd5448

scala> import SISL.instances._ // for inject instances
import SISL.instances._

scala> SISL.lift(List(4)) |+| SISL.lift(List("foo")) |+| SISL.lift(List(List("bar")))
res4: SISL.Prod = (List(foo),List(4),List(List(bar)))
```
