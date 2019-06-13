package andxor

import scala.annotation.tailrec
import scala.collection.mutable.{PriorityQueue => PQ}
import scalaz.Monoid
import scalaz.Id.Id

trait FoldMap[Prod[_[_]], Cop[_[_]]] {
  def unconsAll[F[_]](p: Prod[F])(implicit U: Uncons[F]): (List[Cop[Id]], Prod[F])
  def unconsOne[F[_]](p: Prod[F], c: Cop[Id])(implicit U: Uncons[F]): (Option[Cop[Id]], Prod[F])

  @tailrec
  private def appendAll[C](out: C, q: PQ[Cop[Id]], map: Cop[Id] => C)(implicit M: Monoid[C]): C =
    q.isEmpty match {
      case true => out
      case false =>
        val newOut = M.append(out, map(q.dequeue))
        appendAll(newOut, q, map)
    }

  def foldMap[F[_], C](p: Prod[F])(map: Cop[Id] => C)(
    implicit O: Ordering[Cop[Id]],
    M: Monoid[C],
    MP: Monoid[Prod[F]],
    U: Uncons[F],
  ): C = {
    @tailrec
    def go(prod: Prod[F], q: PQ[Cop[Id]], out: C): C =
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
            val (toAdd, newProd) = unconsOne[F](prod, cop)
            go(newProd, q ++= toAdd, M.append(out, map(cop)))
        }
      }

    val Q = new PQ[Cop[Id]]()(O)
    val (hs, ts) = unconsAll(p)
    Q ++= hs
    go(ts, Q, M.zero)
  }
}

// object Transform {
//   def apply[F[_[_]]](implicit ev: Transform[F]): Transform[F] = ev

//   object ops {
//     implicit class TransformOps[F[_[_]], G[_]](fg: F[G])(implicit T: Transform[F]) {
//       def transform[H[_]](nt: G ~> H): F[H] = T.transform(nt)(fg)
//     }
//   }
// }
