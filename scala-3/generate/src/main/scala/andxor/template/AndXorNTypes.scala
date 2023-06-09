package andxor.template

object AndXorNTypes {
  def apply(tpeLists: List[(List[String], Int)]) =
    s"""
type AndXor1[A] = AndXor._1[A]

${tpeLists.map { case (tpes, i) => s"""
trait AndXor$i[${tpes.mkString(", ")}] extends AndXor.Next[${tpes.head}] {
  final type Prev = AndXor${i - 1}[${tpes.tail.mkString(", ")}]
  final val prev: Prev = AndXor[${tpes.tail.mkString(", ")}]
}
"""
}.mkString("\n")}
"""
}
