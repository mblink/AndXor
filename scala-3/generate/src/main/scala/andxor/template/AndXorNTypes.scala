package andxor.template

object AndXorNTypes {
  def apply(tpeLists: List[(List[String], Int)]) =
    s"""
trait AndXorNTypes {
${tpeLists.map { case (tpes, i) => s"""
  final type _$i[${tpes.mkString(", ")}] = AndXor.Next.Aux[${tpes.head}, ? <: AndXor._${i - 1}[${tpes.tail.mkString(", ")}]]
"""
}.mkString("\n")}
}
"""
}
