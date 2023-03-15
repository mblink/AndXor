package andxor

trait LazyListCompat {
  type LazyList[+A] = Stream[A]
  val LazyList = Stream

  implicit class LazyListObjectOps(val l: LazyList.type) {
    def unfold[A, B](seed: A)(f: A => Option[(B, A)]): LazyList[B] =
      f(seed).fold(Stream.empty[B]) { case (b, a) => Stream.cons(b, unfold(a)(f)) }

    def from[A](i: Iterable[A]): LazyList[A] = i.toStream
  }
}
