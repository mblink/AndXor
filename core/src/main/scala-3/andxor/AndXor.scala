package andxor

import cats.{Apply, Id}
import scala.util.chaining.*

// Right-associative alias for `Either` that yields more compact code and easier chaining
type |:[L, R] = Either[L, R]

inline def axo[A]: AndXor1[A] = new AndXor1[A]
inline def axoN[A[_[_]]]: AndXor1Nested[A] = new AndXor1Nested[A]

sealed trait AndXor { self =>
  type Cop[F[_]]
  type Prod[F[_]] <: Tuple

  protected type Self

  inline final def Cop[F[_]](c: Cop[F]): Cop[F] = c
  inline final def Prod[F[_]](p: Prod[F]): Prod[F] = p

  def nest[X[_[_]]]: AndXor.AppendNested[Self, X]
  final def apply[X]: AndXor.AppendNested[Self, [f[_]] =>> f[X]] = nest[[f[_]] =>> f[X]]

  def *:[X[_[_]]](@annotation.unused a: AndXor1Nested[X]): AndXor.PrependNested[X, Self]

  inline def inj[F[_], A](a: A)(implicit i: Inj[Cop[F], A]): Cop[F] = i(a)
  inline def injId[A](a: A)(implicit i: Inj[Cop[Id], Id[A]]): Cop[Id] = i(a)
  inline def lift[F[_], A](a: A)(implicit i: Inj[Prod[F], A]): Prod[F] = i(a)
  inline def liftId[A](a: A)(implicit i: Inj[Prod[Id], Id[A]]): Prod[Id] = i(a)
  inline def extractC[F[_], B](c: Cop[F])(implicit l: Optional[Cop[F], B]): Option[B] = l.getOption(c)
  inline def extractP[F[_], B](p: Prod[F])(implicit l: Lens[Prod[F], B]): B = l.get(p)
}

sealed trait AndXorNever

sealed trait AndXorEmpty extends AndXor { self =>
  final type Cop[F[_]] = AndXorNever
  final type Prod[F[_]] = EmptyTuple

  protected final type Self = AndXorEmpty

  @inline final def nest[X[_[_]]]: AndXor.AppendNested[Self, X] = axoN[X]
  @inline final def *:[X[_[_]]](@annotation.unused a: AndXor1Nested[X]): AndXor.PrependNested[X, Self] = axoN[X]
}
case object AndXorEmpty extends AndXorEmpty

sealed trait AndXorNonEmpty extends AndXor {
  @inline def deriving[TC[_], F[_]](using i: AndXorInstances[TC, Prod[F]]): AndXorDeriving[TC, Cop[F], Prod[F]]
  @inline final def derivingId[TC[_]](using i: AndXorInstances[TC, Prod[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] =
    deriving[TC, Id]
}

class AndXor1Nested[Head[_[_]]] extends AndXorNonEmpty { self =>
  final type Cop[F[_]] = Head[F]
  final type Prod[F[_]] = Head[F] *: EmptyTuple

  protected final type Self = AndXor1Nested[Head]

  @inline final def nest[X[_[_]]]: AndXor.AppendNested[Self, X] =
    new AndXorNextNested[Head, AndXor1Nested[X]] {
      val prev = axoN[X]
    }

  @inline final def *:[X[_[_]]](@annotation.unused a: AndXor1Nested[X]): AndXor.PrependNested[X, Self] =
    new AndXorNextNested[X, AndXor1Nested[Head]] {
      val prev = self
    }

  @inline final def deriving[TC[_], F[_]](using i: AndXorInstances[TC, Prod[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =
    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      def mkChoose[A](f: A => Cop[F])(using d: Decidable[TC]): TC[A] =
        d.contramap(i.instances.head)(f)

      def mkAlt[A](f: Cop[F] => A)(using a: Alt[TC]): TC[A] =
        a.map(i.instances.head)(f)

      def mkDivide[A](f: A => Prod[F])(using d: Divide[TC]): TC[A] =
        d.contramap(i.instances.head)(f(_).head)

      def mkApply[A](f: Prod[F] => A)(using a: Apply[TC]): TC[A] =
        a.map(i.instances.head)(h => f(h *: EmptyTuple))
    }
}

class AndXor1[Head] extends AndXor1Nested[[f[_]] =>> f[Head]]

object AndXor1 {
  given inst[A]: AndXor1[A] = new AndXor1[A]
}

abstract class AndXorNextNested[Next[_[_]], Prev <: AndXorNonEmpty] extends AndXorNonEmpty { self =>
  val prev: Prev

  final type PrevCop[f[_]] = prev.Cop[f]
  final type PrevProd[f[_]] = prev.Prod[f]

  final type Cop[F[_]] = Next[F] |: PrevCop[F]
  final type Prod[F[_]] = Next[F] *: PrevProd[F]
  object Prod {
    def apply[F[_]](p: Prod[F]): Prod[F] = p
  }

  protected final type Self = AndXorNextNested[Next, Prev]

  @inline final def nest[X[_[_]]]: AndXor.AppendNested[Self, X] =
    (new AndXorNextNested[Next, AndXor.AppendNested[prev.Self, X]] {
      val prev = self.prev.nest[X]
    }).asInstanceOf[AndXor.AppendNested[Self, X]]

  @inline final def *:[X[_[_]]](@annotation.unused a: AndXor1Nested[X]): AndXor.PrependNested[X, Self] =
    new AndXorNextNested[X, AndXorNextNested[Next, Prev]] {
      val prev = self
    }

  @inline final def deriving[TC[_], F[_]](using i: AndXorInstances[TC, Prod[F]]): AndXorDeriving[TC, Cop[F], Prod[F]] =
    new AndXorDeriving[TC, Cop[F], Prod[F]] {
      private lazy val headInstance = i.instances.head
      private lazy implicit val tailInstances: AndXorInstances[TC, PrevProd[F]] = AndXorInstances(i.instances.tail)
      private lazy val tailDeriving = prev.deriving[TC, F]

      def mkChoose[A](f: A => Cop[F])(using d: Decidable[TC]): TC[A] =
        d.choose2(headInstance, tailDeriving.choose)(f)

      def mkAlt[A](f: Cop[F] => A)(using a: Alt[TC]): TC[A] =
        a.altly2(headInstance, tailDeriving.alt)(f)

      def mkDivide[A](f: A => Prod[F])(using d: Divide[TC]): TC[A] =
        d.divide2(headInstance, tailDeriving.divide)(f(_).pipe(t => (t.head, t.tail)))

      def mkApply[A](f: Prod[F] => A)(using a: Apply[TC]): TC[A] =
        a.map2(headInstance, tailDeriving.apply)((h, t) => f(h *: t))
    }
}

abstract class AndXorNext[Next, Prev <: AndXorNonEmpty] extends AndXorNextNested[[f[_]] =>> f[Next], Prev]

object AndXor extends AndXorNConstructors {
  type PrependNested[X[_[_]], A] <: AndXorNonEmpty = A match {
    case AndXorEmpty => AndXor1Nested[X]
    case _ => AndXorNextNested[X, A]
  }

  type Prepend[X, A] = PrependNested[[f[_]] =>> f[X], A]

  type AppendNested[A, X[_[_]]] <: AndXorNonEmpty = A match {
    case AndXorEmpty => AndXor1Nested[X]
    case AndXor1Nested[h] => AndXorNextNested[h, AndXor1Nested[X]]
    case AndXorNextNested[n, p] => AndXorNextNested[n, AppendNested[p, X]]
  }

  type Append[A, X] = AppendNested[A, [f[_]] =>> f[X]]
}
