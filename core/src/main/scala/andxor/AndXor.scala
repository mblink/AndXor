package andxor

import andxor.types.*
import cats.{Apply, Id}
import monocle.{Lens, Optional}
import scala.compiletime.summonAll
import scala.util.chaining.*

// Right-associative alias for `Either` that yields more compact code and easier chaining
type |:[L, R] = Either[L, R]

inline def axo[A]: AndXor._1[A] = AndXor._1.inst[A]
inline def axoN[A[_[_]]]: AndXor._1Nested[A] = AndXor._1Nested.inst[A]

sealed trait AndXor { self =>
  type Cop[F[_]]
  type Prod[F[_]] <: Tuple

  inline final def Cop[F[_]](c: Cop[F]): Cop[F] = c
  inline final def Prod[F[_]](p: Prod[F]): Prod[F] = p

  inline def nest[X[_[_]]]: AndXor.MakeNextNested[X, self.type]
  inline final def apply[X]: AndXor.MakeNextNested[FConst[X], self.type] = nest[FConst[X]]
  inline final def *:[X[_[_]]](a: AndXor._1Nested[X]): AndXor.MakeNextNested[X, self.type] = nest[X]

  inline def inj[F[_], A](a: A)(implicit i: Inj[Cop[F], A]): Cop[F] = i(a)
  inline def injId[A](a: A)(implicit i: Inj[Cop[Id], Id[A]]): Cop[Id] = i(a)
  inline def lift[F[_], A](a: A)(implicit i: Inj[Prod[F], A]): Prod[F] = i(a)
  inline def liftId[A](a: A)(implicit i: Inj[Prod[Id], Id[A]]): Prod[Id] = i(a)
  inline def extractC[F[_], B](c: Cop[F])(implicit l: Optional[Cop[F], B]): Option[B] = l.getOption(c)
  inline def extractP[F[_], B](p: Prod[F])(implicit l: Lens[Prod[F], B]): B = l.get(p)
}

object AndXor
extends AndXorNTypes
with AndXorNConstructors {
  sealed trait Never

  sealed trait Empty extends AndXor { self =>
    final type Cop[F[_]] = Never
    final type Prod[F[_]] = EmptyTuple

    inline final def nest[X[_[_]]]: AndXor._1Nested[X] = axoN[X]
  }
  case object Empty

  sealed trait NonEmpty extends AndXor { self =>
    inline final def nest[X[_[_]]]: AndXor.NextNested.Aux[X, self.type] =
      new AndXor.NextNested[X] {
        type Prev = self.type
        val prev = self
      }

    @inline def deriving[TC[_], F[_]](using i: AndXorInstances[TC, Prod[F]]): AndXorDeriving[TC, Cop[F], Prod[F]]

    @inline final def derivingId[TC[_]](using i: AndXorInstances[TC, Prod[Id]]): AndXorDeriving[TC, Cop[Id], Prod[Id]] =
      deriving[TC, Id]
  }

  trait _1Nested[Head[_[_]]] extends NonEmpty {
    final type Cop[F[_]] = Head[F]
    final type Prod[F[_]] = Head[F] *: EmptyTuple

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

  object _1Nested {
    given inst[A[_[_]]]: _1Nested[A] = new _1Nested[A] {}
  }

  type _1[Head] = _1Nested[FConst[Head]]

  object _1 {
    given inst[A]: _1[A] = new _1Nested[FConst[A]] {}
  }

  trait NextNested[Next[_[_]]] extends NonEmpty {
    type Prev <: NonEmpty
    val prev: Prev

    final type PrevCop[f[_]] = prev.Cop[f]
    final type PrevProd[f[_]] = prev.Prod[f]

    final type Cop[F[_]] = Next[F] |: PrevCop[F]
    final type Prod[F[_]] = Next[F] *: PrevProd[F]

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

  object NextNested {
    type Aux[N[_[_]], P <: NonEmpty] = AndXor.NextNested[N] { type Prev = P }
  }

  type Next[N] = NextNested[FConst[N]]

  object Next {
    type Aux[N, P <: NonEmpty] = AndXor.Next[N] { type Prev = P }
  }

  type MakeNext[X, A] = A match {
    case Empty => AndXor._1[X]
    case _ => AndXor.Next.Aux[X, A]
  }

  type MakeNextNested[X[_[_]], A] = A match {
    case Empty => AndXor._1Nested[X]
    case _ => AndXor.NextNested.Aux[X, A]
  }
}
