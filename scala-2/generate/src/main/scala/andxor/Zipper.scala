package andxor

import cats.CoflatMap

/**
 * Provides a pointed stream, which is a non-empty zipper-like stream structure that tracks an index (focus)
 * position in a LazyList. Focus can be moved forward and backwards through the stream, elements can be inserted
 * before or after the focused position, and the focused item can be deleted.
 * <p/>
 * Based on the pointedlist library by Jeff Wheeler.
 */
final case class Zipper[+A](lefts: LazyList[A], focus: A, rights: LazyList[A]) {
  import Zipper._

  def map[B](f: A => B): Zipper[B] =
    zipper(lefts map f, f(focus), rights map f)

  /**
   * Get the Stream representation of this Zipper. This fully traverses `lefts`. `rights` is
   * not evaluated.
   */
  def toStream: LazyList[A] =
    lefts.reverse ++ focus #:: rights

  /**
   * Update the focus in this zipper.
   */
  def update[AA >: A](focus: AA): Zipper[AA] = {
    this.copy(this.lefts, focus, this.rights)
  }

  /**
   * Apply f to the focus and update with the result.
   */
  def modify[AA >: A](f: A => AA): Zipper[AA] = this.update(f(this.focus))

  /**
   * Possibly moves to next element to the right of focus.
   */
  def next: Option[Zipper[A]] = rights match {
    case r #:: rs => Some(zipper(LazyList.cons(focus, lefts), r, rs))
    case _ => None
  }

  /**
   * Possibly moves to the previous element to the left of focus.
   */
  def previous: Option[Zipper[A]] = lefts match {
    case l #:: ls => Some(zipper(ls, l, LazyList.cons(focus, rights)))
    case _ => None
  }

  def foldLeft[B](b: B)(f: (B, A) => B): B =
    LazyList.cons(focus, rights).foldLeft(lefts.foldRight(b)((a, b) => f(b, a)))(f)

  def foldRight[B](b: => B)(f: (A, => B) => B): B =
    lefts.foldLeft(LazyList.cons(focus, rights).foldRight(b)((a, b) => f(a, b)))((a, b) => f(b, a))

  def length: Int =
    this.foldLeft(0)((b, _) => b + 1)

  /**
   * Whether the focus is on the first element in the zipper.
   */
  def atStart: Boolean = lefts.isEmpty

  /**
   * Whether the focus is on the last element in the zipper.
   */
  def atEnd: Boolean = rights.isEmpty

  /**
   * A zipper of all positions of the zipper, with focus on the current position.
   */
  def positions: Zipper[Zipper[A]] = {
    val left = LazyList.unfold(this)(_.previous.map(x => (x, x)))
    val right = LazyList.unfold(this)(_.next.map(x => (x, x)))

    zipper(left, this, right)
  }

  /**
   * The index of the focus.
   */
  def index: Int = lefts.length
}

object Zipper extends ZipperInstances {
  def zipper[A](ls: LazyList[A], a: A, rs: LazyList[A]): Zipper[A] =
    Zipper(ls, a, rs)
}

sealed abstract class ZipperInstances {
  implicit val zipperCoflatMap: CoflatMap[Zipper] = new CoflatMap[Zipper] {
    def coflatMap[A, B](fa: Zipper[A])(f: Zipper[A] => B): Zipper[B] = map(fa.positions)(f)
    def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] = fa.map(f)
  }
}
