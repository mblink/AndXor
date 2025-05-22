package andxor.template

object Either {
  def apply(tpeLists: List[(List[String], Int)]) =
    s"""
package object either {
${tpeLists.map { case (tpes, i) =>
  val either0 = (initTpe: String, numRights: Int) => (a: String) => 1.to(numRights).foldRight(s"$initTpe($a)")((_, acc) => s"Right($acc)")
  val either = either0("Left", i - 1)
  s"""
  extension [${(tpes :+ "T").mkString(", ")}](x: ${(tpes :+ "T").mkString(" |: ")}) {
    inline final def map$i[B](f: A$i => B): ${(tpes.updated(i - 1, "B") :+ "T").mkString(" |: ")} = x match {
      case ${either("a")} => ${either("f(a)")}
      case _ => x.asInstanceOf[${(tpes.updated(i - 1, "B") :+ "T").mkString(" |: ")}]
    }
  }
"""
}.mkString("\n")}
}
"""
}
