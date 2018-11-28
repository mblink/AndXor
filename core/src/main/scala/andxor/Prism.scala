package andxor

trait Prism[A, B] {
  def getOption(a: A): Option[B]
  def reverseGet(b: B): A
}
