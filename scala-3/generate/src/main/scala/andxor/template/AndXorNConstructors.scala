package andxor.template

object AndXorNConstructors {
  def apply(tpeLists: List[(List[String], Int)]) =
    s"""
trait AndXorNConstructors { ctors =>
${tpeLists.map { case (tpes, i) =>
  val hkTpes = tpes.map(t => s"$t[_[_]]")
  val axo = s"new AndXor$i[${tpes.mkString(", ")}]"
  val nestedAxo = s"new AndXor${i}Nested[${tpes.mkString(", ")}]"

  s"  @inline final def apply[${tpes.mkString(", ")}]: AndXor$i[${tpes.mkString(", ")}] = $axo\n" ++
  s"  @inline final def nest[${hkTpes.mkString(", ")}]: AndXor${i}Nested[${tpes.mkString(", ")}] = $nestedAxo"
}.mkString("\n")}
}
"""
}
