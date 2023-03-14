package andxor.template

object Dummy {
  def apply(tpeLists: List[(List[String], Int)]) =
    s"""
trait Dummy {
${tpeLists.map { case (_, i) =>
  s"  sealed trait Dummy$i; object Dummy$i { given inst: Dummy$i = new Dummy$i {} }"
}.mkString("\n")}
}
"""
}
