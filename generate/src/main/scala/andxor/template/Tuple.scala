package andxor.template

object Tuple {
  def apply(tpeLists: List[(List[String], Int)]) =
    s"""
import cats.Monoid
import monocle.Lens

// These methods could be implemented without `.asInstanceOf`,
// but treating tuples as `Array`s yields significantly better performance
object tuple {
${tpeLists.map { case (tpes, i) => s"""
  extension [${(tpes :+ "T <: Tuple").mkString(", ")}](t: ${(tpes :+ "T").mkString(" *: ")}) {
    inline final def t$i: ${tpes(i - 1)} = t.toArray(${i - 1}).asInstanceOf[${tpes(i - 1)}]

    inline final def map$i[B](f: A$i => B): ${(tpes.updated(i - 1, "B") :+ "T").mkString(" *: ")} = {
      val arr = t.toArray
      Tuple.fromArray(arr.updated(${i - 1}, f(arr(${i - 1}).asInstanceOf[A$i]): Any)).asInstanceOf[${(tpes.updated(i - 1, "B") :+ "T").mkString(" *: ")}]
    }
  }

  given lensT$i[${(tpes :+ "T <: Tuple").mkString(", ")}]: Lens[${(tpes :+ "T").mkString(" *: ")}, A$i] =
    Lens((_: ${(tpes :+ "T").mkString(" *: ")}).t$i)((a: A$i) => (_: ${(tpes :+ "T").mkString(" *: ")}).map$i(_ => a))

  given injT$i[${(tpes :+ "T <: Tuple").mkString(", ")}](
    using M: Monoid[${(tpes :+ "T").mkString(" *: ")}],
  ): Inj[${(tpes :+ "T").mkString(" *: ")}, A$i] =
    Inj.instance((a: A$i) => M.empty.map$i(_ => a))
"""
}.mkString("\n")}
}
"""
}
