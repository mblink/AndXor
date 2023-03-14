package andxor.template

object AndXorNConstructors {
  def apply(tpeLists: List[(List[String], Int)]) =
    s"""
import andxor.types._

trait AndXorNConstructors {
  def nest[A[_[_]]]: AndXor._1Nested[A] = axoN[A]

${tpeLists.map { case (tpes, i) => s"""
  @inline final def apply[${tpes.mkString(", ")}](using d: Dummy$i): AndXor._$i[${tpes.mkString(", ")}] =
    axo[${tpes.head}]${if (tpes.tail.isEmpty) "" else s" *: apply[${tpes.tail.mkString(", ")}]"}
"""
}.mkString("\n")}
}
"""
}
