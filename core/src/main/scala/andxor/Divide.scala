package andxor

import scala.language.higherKinds
import scalaz.{Contravariant, Monoid}

trait Divide[F[_]] extends Contravariant[F] {
  final def divide[A, B, C](fa: =>F[A], fb: =>F[B])(f: C => (A, B)): F[C] =
    divide2(fa, fb)(f)

  final def divide1[A1, Z](a1: F[A1])(f: Z => A1): F[Z] = contramap(a1)(f)

  def tuple2[A1, A2](a1: =>F[A1], a2: =>F[A2]): F[(A1, A2)] = divide2(a1, a2)(identity)

  def divide2[A1, A2, Z](a1: =>F[A1], a2: =>F[A2])(f: Z => (A1, A2)): F[Z]
}

object Divide {
  implicit def divideFunction1[O](implicit M: Monoid[O]): Divide[? => O] = new Divide[? => O] {
    def contramap[A, B](r: A => O)(f: B => A): B => O = b => r(f(b))
    def divide2[A1, A2, Z](a1: => A1 => O, a2: => A2 => O)(f: Z => (A1, A2)): Z => O = z => {
      val (x, y) = f(z)
      M.append(a1(x), a2(y))
    }
  }
}
