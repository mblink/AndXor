package andxor

package object template {
  val maxLen = 100

  def range(start: Int, end: Int): List[Int] = start.to(end).toList

  val parens = (s: String) => s"($s)"

  def mkTpeList(start: Int, end: Int, a: String = "A"): List[(List[String], Int)] =
    start.to(end).toList.map(i => 1.to(i).toList.map(x => s"${a}${x}") -> i)
}
