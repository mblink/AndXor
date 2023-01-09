package andxor

import cats.{Monoid, MonoidK}
import scala.annotation.tailrec
import scala.collection.mutable.{PriorityQueue => PQ}
import scala.util.chaining.*

trait FoldMap[Prod[_[_]], Cop[_[_]]] {
  def emptyProd[F[_]](using PE: MonoidK[F]): Prod[F]
  def unconsAll[F[_], G[_]](p: Prod[F])(using U: Uncons[F, G]): (List[Cop[G]], Prod[F])
  def unconsOne[F[_], G[_]](p: Prod[F], c: Cop[G])(using U: Uncons[F, G]): (Option[Cop[G]], Prod[F])

  @tailrec
  private final def appendAll[G[_], C](out: C, q: PQ[Cop[G]], f: (C, Cop[G]) => C): C =
    q.isEmpty match {
      case true => out
      case false => appendAll(f(out, q.dequeue()), q, f)
    }

  final def fold[F[_], G[_], C](p: Prod[F], zero: C)(f: (C, Cop[G]) => C)(
    using O: Ordering[Cop[G]],
    PE: MonoidK[F],
    U: Uncons[F, G]
  ): C = {
    @tailrec
    def go(prod: Prod[F], q: PQ[Cop[G]], out: C): C =
      (prod == emptyProd[F]) match {
        case true => appendAll(out, q, f)
        case false => q.isEmpty match {
          case true => {
            val (hs, ts) = unconsAll(prod)
            q ++= hs
            go(ts, q, out)
          }
          case false =>
            val cop = q.dequeue()
            val (toAdd, newProd) = unconsOne[F, G](prod, cop)
            go(newProd, q ++= toAdd, f(out, cop))
        }
      }

    val Q = new PQ[Cop[G]]()(O)
    val (hs, ts) = unconsAll(p)
    Q ++= hs
    go(ts, Q, zero)
  }

  final def foldMap[F[_], G[_], C](p: Prod[F])(map: Cop[G] => C)(
    using O: Ordering[Cop[G]],
    M: Monoid[C],
    PE: MonoidK[F],
    U: Uncons[F, G]
  ): C = fold(p, M.empty)((x, c: Cop[G]) => M.combine(x, map(c)))
}

object FoldMap {
  @inline def apply[Prod[_[_]], Cop[_[_]]](using ev: FoldMap[Prod, Cop]): FoldMap[Prod, Cop] = ev

  given axo1[X]: FoldMap[[F[_]] =>> F[X] *: EmptyTuple, [F[_]] =>> F[X]] =
    new FoldMap[[F[_]] =>> F[X] *: EmptyTuple, [F[_]] =>> F[X]] {
      def emptyProd[F[_]](using PE: MonoidK[F]): F[X] *: EmptyTuple = PE.empty[X] *: EmptyTuple

      def unconsAll[F[_], G[_]](p: F[X] *: EmptyTuple)(using U: Uncons[F, G]): (List[G[X]], F[X] *: EmptyTuple) =
        U(p.head) match {
          case (Some(h), t) => (List(h), t *: EmptyTuple)
          case (None, t) => (Nil, t *: EmptyTuple)
        }

      def unconsOne[F[_], G[_]](p: F[X] *: EmptyTuple, c: G[X])(using U: Uncons[F, G]): (Option[G[X]], F[X] *: EmptyTuple) =
        U(p.head).pipe { case (h, t) => (h, t *: EmptyTuple) }
    }

  given axoN[X, CT[_[_]], PT[_[_]] <: Tuple](using F: FoldMap[PT, CT]): FoldMap[[F[_]] =>> F[X] *: PT[F], [F[_]] =>> F[X] |: CT[F]] =
    new FoldMap[[F[_]] =>> F[X] *: PT[F], [F[_]] =>> F[X] |: CT[F]] {
      def emptyProd[F[_]](using PE: MonoidK[F]): F[X] *: PT[F] = PE.empty[X] *: F.emptyProd[F]

      def unconsAll[F[_], G[_]](p: F[X] *: PT[F])(using U: Uncons[F, G]): (List[G[X] |: CT[G]], F[X] *: PT[F]) =
        (U(p.head), F.unconsAll[F, G](p.tail)) match {
          case ((Some(h), t), (ct, pt)) => (Left(h) :: ct.map(Right(_)), t *: pt)
          case ((None, t), (ct, pt)) => (ct.map(Right(_)), t *: pt)
        }

      def unconsOne[F[_], G[_]](p: F[X] *: PT[F], c: G[X] |: CT[G])(using U: Uncons[F, G]): (Option[G[X] |: CT[G]], F[X] *: PT[F]) =
        c match {
          case Left(_) => U(p.head).pipe { case (h, t) => (h.map(Left(_)), t *: p.tail) }
          case Right(ct) => F.unconsOne[F, G](p.tail, ct).pipe { case (h, t) => (h.map(Right(_)), p.head *: t) }
        }
    }

  object syntax {
    extension [Prod[_[_]], Cop[_[_]], F[_], G[_]](prod: Prod[F]) {
      def foldMap[C](map: Cop[G] => C)(using
        M: Monoid[C],
        F: FoldMap[Prod, Cop],
        O: Ordering[Cop[G]],
        PE: MonoidK[F],
        U: Uncons[F, G]
      ): C = F.foldMap(prod)(map)
    }
  }
}
