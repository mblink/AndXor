package andxor

import andxor.types.{Cop1, Prod1, TCDeps1}
import scala.annotation.tailrec
import scalaz.{Apply, Functor, PlusEmpty, Monoid, ~>}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXor1[A1] extends AndXor {
  type Prod[F[_]] = Prod1[F, A1]
  object Prod { def apply[F[_]](p: F[A1]): Prod[F] = Prod1[F, A1](p) }

  type Cop[F[_]] = Cop1[F, A1]
  object Cop { def apply[F[_]](c: F[A1]): Cop[F] = Cop1[F, A1](c) }

  type TCDeps[TC[_], F[_]] = TCDeps1[TC, F, A1]

  def mkChoose[TC[_], F[_], B](f: B => Cop[F])(implicit d: Decidable[TC], tcs: TCDeps[TC, F]): TC[B] =
    d.contramap(tcs.a0)(f(_).run)

  def mkAlt[TC[_], F[_], B](f: Cop[F] => B)(implicit a: Alt[TC], tcs: TCDeps[TC, F]): TC[B] =
    a.map(tcs.a0)(x => f(Cop(x)))

  def mkDivide[TC[_], F[_], B](f: B => Prod[F])(implicit d: Divide[TC], tcs: TCDeps[TC, F]): TC[B] =
    d.contramap(tcs.a0)(f(_).run)

  def mkApply[TC[_], F[_], B](f: Prod[F] => B)(implicit a: Apply[TC], tcs: TCDeps[TC, F]): TC[B] =
    a.map(tcs.a0)(x => f(Prod(x)))

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = choose[Inj[Cop[F], ?], F]
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = divide[Inj[Prod[F], ?], F]
    implicit def injCopToProdEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Cop[F]] = choose[Inj[Prod[F], ?], F]
    implicit def injProdToVecCopEv[F[_]]: Inj[Vector[Cop[F]], Prod[F]] = divide[Inj[Vector[Cop[F]], ?], F]
  }

  def transformP[F[_], G[_]](nt: (F ~> G)): Prod[F] => Prod[G] =
    (p: Prod[F]) => Prod[G](nt(p.run))

  def transformC[F[_], G[_]](nt: (F ~> G)): Cop[F] => Cop[G] =
    (c: Cop[F]) => Cop[G](nt(c.run))

  // format: off
  def sequenceP[F[_]](p: Prod[F])(implicit A: Apply[F]): F[Prod[Id]] =
    A.map(
    A.map(p.run)((i0: A1) =>
      i0))(Prod[Id](_))

  def sequenceC[F[_]](cop: Cop[F])(implicit FF: Functor[F]): F[Cop[Id]] =
    cop.run match {
      case x => FF.map(x)(y => Cop[Id](y))
    }

  def extractC[F[_], B](c: Cop[F])(implicit inj: Inj[Option[B], Cop[F]]): Option[B] = inj(c)

  def extractP[F[_], B](p: Prod[F])(implicit inj: Inj[B, Prod[F]]): B = inj(p)

  def foldMap[G[_], C](p: Prod[G])(map: Cop[Id] => C)(implicit O: Ordering[Cop[Id]], M: Monoid[C], PE: PlusEmpty[G], U: Uncons[G]): C = {
    import scala.collection.mutable.{PriorityQueue => PQ}

    def uncons(p: Prod[G]): (List[Cop[Id]], Prod[G]) = {
      val ht1 = U(p.run)
      (List(ht1._1.map(injId(_: A1))).flatten,
        Prod[G](ht1._2))
    }

    @tailrec
    def appendAll(out: C, q: PQ[Cop[Id]]): C =
      q.isEmpty match {
        case true => out
        case false =>
          val newOut = M.append(out, map(q.dequeue))
          appendAll(newOut, q)
      }

    @tailrec
    def go(prod: Prod[G], q: PQ[Cop[Id]], out: C): C =
      (prod.run.==(PE.empty[A1])) match {
        case true => appendAll(out, q)
        case false => q.isEmpty match {
          case true => {
            val (hs, ts) = uncons(prod)
            q ++= hs
            go(ts, q, out)
          }
          case false => q.dequeue.run match {
            case dj @ _ =>
              val (h, t) = U(prod.run)
              go(Prod[G](t),
                q ++= h.map(injId(_: A1)), M.append(out, map(Cop[Id](dj))))

          }
        }
      }
    val Q = new PQ[Cop[Id]]()(O)
    val (hs, ts) = uncons(p)
    Q ++= hs
    go(ts, Q, M.zero)
  }
  // format: on
}

object AndXor1 {
  def apply[A1]: AndXor1[A1] =
    new AndXor1[A1] {}
}
