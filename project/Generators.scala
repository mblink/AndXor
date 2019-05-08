package andxor

import scala.meta._
import scala.meta.gen.CompanionGenerator

object generators {
  object LabelledCovariant extends CompanionGenerator("deriveLabelledCovariant") {
    override def extendCompanion(c: Defn.Class): List[Stat] = {
      println(s"HERE: $c")
      val hi: Lit.String = Lit.String("hi")
      val hiMethod: Defn.Def = q"def hi = println($hi)"

      hiMethod :: Nil
    }
  }
}
