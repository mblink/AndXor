package andxor.template

object FTraverseEitherNInstances {
  def apply(tpeLists: List[(List[String], Int)]) =
    s"""
import cats.Functor

private[andxor] trait FTraverseEitherNInstances { self: FTraverseInstances =>
${tpeLists.map { case (tpes, i) =>
  val fTpes = tpes.map(t => s"F[$t]")
  val copTpe = fTpes.mkString(" |: ")
  val prevCopTpe = fTpes.drop(1).mkString(" |: ")

  s"""
  final implicit def either$i[TC[f[_]] <: Functor[f], ${tpes.mkString(", ")}](
    using F: FTraverse[[F[_]] =>> $prevCopTpe, TC]
  ): FTraverse[[F[_]] =>> $copTpe, TC] =
    new Either[TC, ${tpes.head}, [F[_]] =>> $prevCopTpe] { protected val FR = F }
"""
  }.mkString("\n")}
}
"""
}
