package andxor

trait LazyListCompat {
  type LazyList[+A] = scala.LazyList[A]
  val LazyList = scala.LazyList
}
