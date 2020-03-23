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
    case LazyList() => None
    case r #:: rs     => Some(zipper(LazyList.cons(focus, lefts), r, rs))
  }

  /**
   * Possibly moves to the previous element to the left of focus.
   */
  def previous: Option[Zipper[A]] = lefts match {
    case LazyList() => None
    case l #:: ls     => Some(zipper(ls, l, LazyList.cons(focus, rights)))
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

  // def traverse[G[_]: Applicative, B](f: A => G[B]): G[Zipper[B]] = {
  //   val z = (Zipper.zipper(_: LazyList[B], _: B, _: LazyList[B])).curried
  //   val G = Applicative[G]
  //   import std.LazyList.streamInstance
  //   G.apF(G.apF(G.map(Traverse[Stream].traverse[G, A, B](lefts.reverse)(f))(s => z(s.reverse)))(f(focus)))(Traverse[Stream].traverse[G, A, B](rights)(f))
  // }

  def ap[B](f: => Zipper[A => B]): Zipper[B] = {
    val ls = lefts.zip(f.lefts) map {
      case (aa, ff) => ff(aa)
    }
    val rs = rights.zip(f.rights) map {
      case (aa, ff) => ff(aa)
    }
    zipper(ls, f.focus(focus), rs)
  }

  override def toString: String = {
    "Zipper(<lefts>, " + focus + ", <rights>)"
  }
}

object Zipper extends ZipperInstances {
  def zipper[A](ls: LazyList[A], a: A, rs: LazyList[A]): Zipper[A] =
    Zipper(ls, a, rs)
}

sealed abstract class ZipperInstances {
  // import Zipper._

  implicit val zipperCoflatMap: CoflatMap[Zipper] = new CoflatMap[Zipper] {
    def coflatMap[A, B](fa: Zipper[A])(f: Zipper[A] => B): Zipper[B] = map(fa.positions)(f)
    def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] = fa.map(f)
  }

  // implicit val zipperInstance: Traverse1[Zipper] with Applicative[Zipper] with Comonad[Zipper] = new Traverse1[Zipper] with Applicative[Zipper] with Comonad[Zipper] {
  //   import std.LazyList._
  //   override def cojoin[A](a: Zipper[A]): Zipper[Zipper[A]] =
  //     a.positions
  //   def cobind[A, B](fa: Zipper[A])(f: Zipper[A] => B): Zipper[B] =
  //     map(cojoin(fa))(f)
  //   def copoint[A](p: Zipper[A]): A =
  //     p.focus
  //   override def traverseImpl[G[_] : Applicative, A, B](za: Zipper[A])(f: A => G[B]): G[Zipper[B]] =
  //     za traverse f
  //   override def foldRight[A, B](fa: Zipper[A], z: => B)(f: (A, => B) => B): B =
  //     fa.foldRight(z)(f)
  //   override def foldLeft[A, B](fa: Zipper[A], z: B)(f: (B, A) => B): B =
  //     fa.foldLeft(z)(f)
  //   override def foldMap[A, B](fa: Zipper[A])(f: A => B)(implicit F: Monoid[B]) =
  //     fa.foldLeft(F.zero)((b, a) => F.append(b, f(a)))
  //   def point[A](a: => A): Zipper[A] =
  //     zipper(LazyList.continually(a), a, LazyList.continually(a))
  //   def ap[A, B](fa: => Zipper[A])(f: => Zipper[A => B]): Zipper[B] =
  //     fa ap f
  //   override def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] =
  //     fa map f
  //   override def all[A](fa: Zipper[A])(f: A => Boolean) =
  //     fa.lefts.forall(f) && f(fa.focus) && fa.rights.forall(f)
  //   override def any[A](fa: Zipper[A])(f: A => Boolean) =
  //     fa.lefts.exists(f) || f(fa.focus) || fa.rights.exists(f)
  //   override def foldMap1[A, B](fa: Zipper[A])(f: A => B)(implicit F: Semigroup[B]) =
  //     fa.rights.foldLeft(
  //       Foldable[Stream].foldMapRight1Opt(fa.lefts)(f)((a, b) => F.append(b, f(a))) match {
  //         case Some(b) => F.append(b, f(fa.focus))
  //         case None => f(fa.focus)
  //       }
  //     )((b, a) => F.append(b, f(a)))
  //   override def foldMapRight1[A, B](fa: Zipper[A])(z: A => B)(f: (A, => B) => B) =
  //     Foldable[Stream].foldLeft(
  //       fa.lefts,
  //       Foldable[Stream].foldMapRight1Opt(fa.rights)(z)(f) match {
  //         case Some(b) => f(fa.focus, b)
  //         case None => z(fa.focus)
  //       }
  //     )((b, a) => f(a, b))
  //   override def foldMapLeft1[A, B](fa: Zipper[A])(z: A => B)(f: (B, A) => B) =
  //     fa.rights.foldLeft(
  //       Foldable[Stream].foldMapRight1Opt(fa.lefts)(z)((a, b) => f(b, a)) match {
  //         case Some(b) => f(b, fa.focus)
  //         case None => z(fa.focus)
  //       }
  //     )(f)
  //   override def traverse1Impl[G[_], A, B](fa: Zipper[A])(f: A => G[B])(implicit G: Apply[G]) = {
  //     val F = Traverse1[OneAnd[Stream, ?]]
  //     fa.lefts.reverse match {
  //       case h1 #:: t1 =>
  //         val x = G.map(F.traverse1(OneAnd(h1, t1))(f)) { s => (s.head #:: s.tail).reverse }
  //         fa.rights match {
  //           case h2 #:: t2 =>
  //             G.apply3(x, f(fa.focus), F.traverse1(OneAnd(h2, t2))(f)) { (l, z, r) =>
  //               Zipper(l, z, r.head #:: r.tail)
  //             }
  //           case LazyList.Empty =>
  //             G.apply2(x, f(fa.focus)) { (l, z) =>
  //               Zipper(l, z, LazyList.Empty)
  //             }
  //         }
  //       case LazyList.Empty =>
  //         fa.rights match {
  //           case h2 #:: t2 =>
  //             G.apply2(f(fa.focus), F.traverse1(OneAnd(h2, t2))(f)) { (z, r) =>
  //               Zipper(LazyList.Empty, z, r.head #:: r.tail)
  //             }
  //           case LazyList.Empty =>
  //             G.map(f(fa.focus)) { z =>
  //               Zipper(LazyList.Empty, z, LazyList.Empty)
  //             }
  //         }
  //     }
  //   }
  // }

  // implicit def zipperEqual[A: Equal]: Equal[Zipper[A]] = new Equal[Zipper[A]] {
  //   import std.LazyList.streamEqual
  //   def equal(a1: Zipper[A], a2: Zipper[A]) =
  //     streamEqual[A].equal(a1.lefts, a2.lefts) && Equal[A].equal(a1.focus, a2.focus) && streamEqual[A].equal(a1.rights, a2.rights)
  // }

  // implicit def zipperShow[A: Show]: Show[Zipper[A]] = new Show[Zipper[A]]{
  //   import std.LazyList._

  //   override def show(f: Zipper[A]) =
  //     Cord("Zipper(",
  //       Show[LazyList[A]].show(f.lefts), ", ",
  //       Show[A].show(f.focus), ", ",
  //       Show[LazyList[A]].show(f.rights), ")")
  // }
}
