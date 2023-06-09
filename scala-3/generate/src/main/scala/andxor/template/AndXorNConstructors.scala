package andxor.template

object AndXorNConstructors {
  def apply(tpeLists: List[(List[String], Int)]) =
    s"""
import andxor.types._

trait AndXorNConstructors { ctors =>
  def nest[A[_[_]]]: AndXor._1Nested[A] = axoN[A]

${tpeLists.map { case (tpes, i) =>
  val axo =
    if (tpes.tail.isEmpty) s"axo[${tpes.head}]"
    else s"new AndXor$i[${tpes.mkString(", ")}] {}"

  s"""
  @inline final def apply[${tpes.mkString(", ")}](using @annotation.unused d: Dummy$i): AndXor$i[${tpes.mkString(", ")}] =
    $axo
"""
}.mkString("\n")}
}
"""
}
