package andxor.template

object AndXorNTypes {
  def apply(tpeLists: List[(List[String], Int)]) =
    s"""
${tpeLists.map { case (tpes, i) =>
  val hkTpes = tpes.map(t => s"$t[_[_]]")
  s"""
class AndXor$i[${tpes.mkString(", ")}] extends AndXorNext[${tpes.head}, AndXor${i - 1}[${tpes.tail.mkString(", ")}]] {
  final val prev = AndXor[${tpes.tail.mkString(", ")}]
}

class AndXor${i}Nested[${hkTpes.mkString(", ")}] extends AndXorNextNested[${tpes.head}, AndXor${i - 1}Nested[${tpes.tail.mkString(", ")}]] {
  final val prev = AndXor.nest[${tpes.tail.mkString(", ")}]
}
"""
}.mkString("\n")}
"""
}
