package andxor

import cats.{Monoid, MonoidK}
import scala.annotation.tailrec
import scala.collection.mutable.{PriorityQueue => PQ}

trait FoldMap[Prod[_[_]], Cop[_[_]]] {
  def emptyProd[F[_]](implicit PE: MonoidK[F]): Prod[F]
  def unconsAll[F[_], G[_]](p: Prod[F])(implicit U: Uncons[F, G]): (List[Cop[G]], Prod[F])
  def unconsOne[F[_], G[_]](p: Prod[F], c: Cop[G])(implicit U: Uncons[F, G]): (Option[Cop[G]], Prod[F])

  @tailrec
  private def appendAll[G[_], C](out: C, q: PQ[Cop[G]], f: (C, Cop[G]) => C): C =
    q.isEmpty match {
      case true => out
      case false =>
        val newOut = f(out, q.dequeue())
        appendAll(newOut, q, f)
    }

  def fold[F[_], G[_], C](p: Prod[F], zero: C)(f: (C, Cop[G]) => C)(
    implicit O: Ordering[Cop[G]],
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

  def foldMap[F[_], G[_], C](p: Prod[F])(map: Cop[G] => C)(
    implicit O: Ordering[Cop[G]],
    M: Monoid[C],
    PE: MonoidK[F],
    U: Uncons[F, G]
  ): C = fold(p, M.empty)((x, c: Cop[G]) => M.combine(x, map(c)))
}

object FoldMap {
  def apply[Prod[_[_]], Cop[_[_]]](implicit ev: FoldMap[Prod, Cop]): FoldMap[Prod, Cop] = ev

  object syntax {
    implicit class FoldMapOps[Prod[_[_]], Cop[_[_]], F[_], G[_]](prod: Prod[F])(
      implicit F: FoldMap[Prod, Cop],
      O: Ordering[Cop[G]],
      PE: MonoidK[F],
      U: Uncons[F, G]
    ) {
      def foldMap[C](map: Cop[G] => C)(implicit M: Monoid[C]): C = F.foldMap(prod)(map)
    }
  }
}
