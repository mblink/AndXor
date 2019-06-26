package andxor

import scala.annotation.tailrec
import scala.collection.mutable.{PriorityQueue => PQ}
import scalaz.Monoid

trait FoldMap[Prod[_[_]], Cop[_[_]]] {
  def unconsAll[F[_], G[_]](p: Prod[F])(implicit U: Uncons[F, G]): (List[Cop[G]], Prod[F])
  def unconsOne[F[_], G[_]](p: Prod[F], c: Cop[G])(implicit U: Uncons[F, G]): (Option[Cop[G]], Prod[F])

  @tailrec
  private def appendAll[G[_], C](out: C, q: PQ[Cop[G]], map: Cop[G] => C)(implicit M: Monoid[C]): C =
    q.isEmpty match {
      case true => out
      case false =>
        val newOut = M.append(out, map(q.dequeue))
        appendAll(newOut, q, map)
    }

  def foldMap[F[_], G[_], C](p: Prod[F])(map: Cop[G] => C)(
    implicit O: Ordering[Cop[G]],
    M: Monoid[C],
    MP: Monoid[Prod[F]],
    U: Uncons[F, G]
  ): C = {
    @tailrec
    def go(prod: Prod[F], q: PQ[Cop[G]], out: C): C =
      (prod == MP.zero) match {
        case true => appendAll(out, q, map)
        case false => q.isEmpty match {
          case true => {
            val (hs, ts) = unconsAll(prod)
            q ++= hs
            go(ts, q, out)
          }
          case false =>
            val cop = q.dequeue
            val (toAdd, newProd) = unconsOne[F, G](prod, cop)
            go(newProd, q ++= toAdd, M.append(out, map(cop)))
        }
      }

    val Q = new PQ[Cop[G]]()(O)
    val (hs, ts) = unconsAll(p)
    Q ++= hs
    go(ts, Q, M.zero)
  }
}

object FoldMap {
  def apply[Prod[_[_]], Cop[_[_]]](implicit ev: FoldMap[Prod, Cop]): FoldMap[Prod, Cop] = ev

  object syntax {
    implicit class FoldMapOps[Prod[_[_]], Cop[_[_]], F[_], G[_]](prod: Prod[F])(
      implicit F: FoldMap[Prod, Cop],
      O: Ordering[Cop[G]],
      MP: Monoid[Prod[F]],
      U: Uncons[F, G]
    ) {
      def foldMap[C](map: Cop[G] => C)(implicit M: Monoid[C]): C = F.foldMap(prod)(map)
    }
  }
}
