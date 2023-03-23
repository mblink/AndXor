package andxor.template

object Either {
  def apply(tpeLists: List[(List[String], Int)]) =
    s"""
import monocle.Optional

package object either {
${tpeLists.map { case (tpes, i) =>
  val either0 = (initTpe: String, numRights: Int) => (a: String) => 1.to(numRights).foldRight(s"$initTpe($a)")((_, acc) => s"Right($acc)")
  val either = either0("Left", i - 1)
  val eitherLast = either0("Right", i - 2)
  s"""
  extension [${(tpes :+ "T").mkString(", ")}](x: ${(tpes :+ "T").mkString(" |: ")}) {
    inline final def map$i[B](f: A$i => B): ${(tpes.updated(i - 1, "B") :+ "T").mkString(" |: ")} = x match {
      case ${either("a")} => ${either("f(a)")}
      case _ => x.asInstanceOf[${(tpes.updated(i - 1, "B") :+ "T").mkString(" |: ")}]
    }
  }

  implicit def optionalE$i[${(tpes :+ "T").mkString(", ")}]: Optional[${(tpes :+ "T").mkString(" |: ")}, A$i] =
    Optional((_: ${(tpes :+ "T").mkString(" |: ")}) match {
      case ${either("a")} => Some(a)
      case _ => None
    })((a: A$i) => (_: ${(tpes :+ "T").mkString(" |: ")}).map$i(_ => a))

  ${if (i == 1) "" else s"""
  implicit def optionalE${i}Last[${tpes.mkString(", ")}]: Optional[${tpes.mkString(" |: ")}, A$i] =
    Optional((_: ${tpes.mkString(" |: ")}) match {
      case ${eitherLast("a")} => Some(a)
      case _ => None
    })((a: A$i) => (_: ${tpes.mkString(" |: ")}) match {
      case ${eitherLast("_")} => ${eitherLast("a")}
      case e => e
    })
"""}
"""
}.mkString("\n")}
}
"""
}
