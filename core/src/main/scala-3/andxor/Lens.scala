package andxor

import cats.syntax.either._

opaque type Iso[A, B] = monocle.Iso[A, B]

object Iso {
  def apply[A, B](g: A => B)(r: B => A): Iso[A, B] = monocle.Iso[A, B](g)(r)

  extension[A, B](i: Iso[A, B]) {
    @inline final def asMonocle: monocle.Iso[A, B] = i
    @inline final def get(a: A): B = asMonocle.get(a)
    @inline final def reverseGet(b: B): A = asMonocle.reverseGet(b)
  }
}

opaque type Lens[S, A] = monocle.Lens[S, A]

sealed trait LensLP {
  given lensTupleTail[A, H, T <: Tuple](using l: Lens[T, A]): Lens[H *: T, A] =
    Lens((t: H *: T) => l.get(t.tail))((a: A) => (t: H *: T) => t.head *: l.replace(a)(t.tail))
}

object Lens extends LensLP {
  def apply[S, A](g: S => A)(r: A => S => S): Lens[S, A] = monocle.Lens[S, A](g)(r)

  extension[S, A](l: Lens[S, A]) {
    @inline final def asMonocle: monocle.Lens[S, A] = l
    @inline final def get(s: S): A = asMonocle.get(s)
    @inline final def replace(a: A): S => S = asMonocle.replace(a)
  }

  given lensTupleHead[H, T <: Tuple]: Lens[H *: T, H] =
    Lens((_: H *: T).head)((h: H) => (t: H *: T) => h *: t.tail)
}

opaque type Optional[S, A] = monocle.Optional[S, A]

sealed trait OptionalLP {
  final given optionalEitherRecurse[L, R, A](using o: Optional[R, A]): Optional[L |: R, A] =
    Optional((_: L |: R).toOption.flatMap(o.getOption))((a: A) => (_: L |: R).map(o.replace(a)))
}

object Optional extends OptionalLP {
  def apply[S, A](g: S => Option[A])(r: A => S => S): Optional[S, A] = monocle.Optional[S, A](g)(r)

  extension[S, A](o: Optional[S, A]) {
    @inline final def asMonocle: monocle.Optional[S, A] = o
    @inline final def getOption(s: S): Option[A] = asMonocle.getOption(s)
    @inline final def replace(a: A): S => S = asMonocle.replace(a)
  }

  given optionalEitherL[L, R]: Optional[L |: R, L] =
    Optional((_: L |: R).swap.toOption)((l: L) => (_: L |: R).leftMap(_ => l))

  given optionalEitherR[L, R]: Optional[L |: R, R] =
    Optional((_: L |: R).toOption)((r: R) => (_: L |: R).map(_ => r))
}
