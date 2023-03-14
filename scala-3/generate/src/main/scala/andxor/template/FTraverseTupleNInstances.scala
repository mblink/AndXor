package andxor.template

object FTraverseTupleNInstances {
  def apply(tpeLists: List[(List[String], Int)]) =
    s"""
import cats.Apply

private[andxor] trait FTraverseTupleNInstances { self: FTraverseInstances =>
${tpeLists.map { case (tpes, i) =>
  val fTpes = tpes.map(t => s"F[$t]")
  val prodTpe = (fTpes :+ "EmptyTuple").mkString(" *: ")
  val prevProdTpe = (fTpes.tail :+ "EmptyTuple").mkString(" *: ")

  s"""
  final given tuple$i[TC[f[_]] <: Apply[f], ${tpes.mkString(", ")}](
    using F: FTraverse[[F[_]] =>> $prevProdTpe, TC]
  ): FTraverse[[F[_]] =>> $prodTpe, TC] =
    new TupleN[TC, ${tpes.head}, [F[_]] =>> $prevProdTpe] { protected val FT = F }
"""
  }.mkString("\n")}
}
"""
}
