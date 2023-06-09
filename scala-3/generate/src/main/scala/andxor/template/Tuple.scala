package andxor.template

object Tuple {
  def apply(tpeLists: List[(List[String], Int)]) =
    s"""
import cats.Monoid

// These methods could be implemented without `.asInstanceOf`,
// but treating tuples as `Array`s yields significantly better performance
object tuple {
${tpeLists.map { case (tpes, i) => s"""
  extension [${(tpes :+ "T <: Tuple").mkString(", ")}](t: ${(tpes :+ "T").mkString(" *: ")}) {
    inline final def t$i: ${tpes(i - 1)} = t.productElement(${i - 1}).asInstanceOf[${tpes(i - 1)}]

    inline final def map$i[B](f: A$i => B): ${(tpes.updated(i - 1, "B") :+ "T").mkString(" *: ")} = {
      val arr = t.toArray
      arr.update(${i - 1}, f(arr(${i - 1}).asInstanceOf[A$i]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[${(tpes.updated(i - 1, "B") :+ "T").mkString(" *: ")}]
    }
  }
"""
}.mkString("\n")}
}

trait TupleInj {
  import andxor.tuple._

${tpeLists.map { case (tpes, i) => s"""
  implicit def injT$i[${(tpes :+ "T <: Tuple").mkString(", ")}](
    using M: Monoid[${(tpes :+ "T").mkString(" *: ")}],
  ): Inj[${(tpes :+ "T").mkString(" *: ")}, A$i] =
    Inj.instance((a: A$i) => M.empty.map$i(_ => a))
"""
}.mkString("\n")}
}
"""
}
