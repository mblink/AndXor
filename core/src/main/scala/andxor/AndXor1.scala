package andxor

import andxor.types.{Cop1, Prod1}
import scala.annotation.tailrec
import scalaz.{Apply, Functor, PlusEmpty, Monoid, ~>}
import scalaz.Id.Id

trait AndXor1[A1] extends AndXor {
  type Prod[F[_]] = Prod1[F, A1]
  object Prod {
    def apply[F[_]](p: F[A1]): Prod[F] = Prod1[F, A1](p)
  }

  type Cop[F[_]] = Cop1[F, A1]
  object Cop {
    def apply[F[_]](c: F[A1]): Cop[F] = Cop1[F, A1](c)
  }

  def mkChoose[TC[_], F[_], B](f: B => Cop[F])(implicit d: Decidable[TC], a0: TC[F[A1]]): TC[B] =
    d.contramap(a0)(f(_).run)

  def mkAlt[TC[_], F[_], B](f: Cop[F] => B)(implicit a: Alt[TC], a0: TC[F[A1]]): TC[B] =
    a.map(a0)(x => f(Cop(x)))

  def mkDivide[TC[_], F[_], B](f: B => Prod[F])(implicit d: Divide[TC], a0: TC[F[A1]]): TC[B] =
    d.contramap(a0)(f(_).run)

  def mkApply[TC[_], F[_], B](f: Prod[F] => B)(implicit a: Apply[TC], a0: TC[F[A1]]): TC[B] =
    a.map(a0)(x => f(Prod(x)))

  def mkChoose[TC[_], B](f: B => Cop[Id])(implicit d: Decidable[TC], a0: TC[A1], dummy: DummyImplicit): TC[B] = mkChoose[TC, Id, B](f)
  def mkAlt[TC[_], B](f: Cop[Id] => B)(implicit a: Alt[TC], a0: TC[A1], dummy: DummyImplicit): TC[B] = mkAlt[TC, Id, B](f)
  def mkDivide[TC[_], B](f: B => Prod[Id])(implicit d: Divide[TC], a0: TC[A1], dummy: DummyImplicit): TC[B] = mkDivide[TC, Id, B](f)
  def mkApply[TC[_], B](f: Prod[Id] => B)(implicit a: Apply[TC], a0: TC[A1], dummy: DummyImplicit): TC[B] = mkApply[TC, Id, B](f)

  def choose[TC[_], F[_]](implicit d: Decidable[TC], a0: TC[F[A1]]): TC[Cop[F]] = mkChoose[TC, F, Cop[F]](identity)
  def alt[TC[_], F[_]](implicit a: Alt[TC], a0: TC[F[A1]]): TC[Cop[F]] = mkAlt[TC, F, Cop[F]](identity)
  def divide[TC[_], F[_]](implicit d: Divide[TC], a0: TC[F[A1]]): TC[Prod[F]] = mkDivide[TC, F, Prod[F]](identity)
  def apply[TC[_], F[_]](implicit a: Apply[TC], a0: TC[F[A1]]): TC[Prod[F]] = mkApply[TC, F, Prod[F]](identity)

  def choose[TC[_]](implicit d: Decidable[TC], a0: TC[A1], dummy: DummyImplicit): TC[Cop[Id]] = mkChoose[TC, Cop[Id]](identity)
  def alt[TC[_]](implicit a: Alt[TC], a0: TC[A1], dummy: DummyImplicit): TC[Cop[Id]] = mkAlt[TC, Cop[Id]](identity)
  def divide[TC[_]](implicit d: Divide[TC], a0: TC[A1], dummy: DummyImplicit): TC[Prod[Id]] = mkDivide[TC, Prod[Id]](identity)
  def apply[TC[_]](implicit a: Apply[TC], a0: TC[A1], dummy: DummyImplicit): TC[Prod[Id]] = mkApply[TC, Prod[Id]](identity)

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = choose[Inj[Cop[F], ?], F]
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = divide[Inj[Prod[F], ?], F]
  }

  def transformP[F[_], G[_]](nt: (F ~> G)): Prod[F] => Prod[G] =
    Transform[Prod].transform(nt)

  def transformC[F[_], G[_]](nt: (F ~> G)): Cop[F] => Cop[G] =
    Transform[Cop].transform(nt)

  def sequenceP[F[_]](p: Prod[F])(implicit F: Apply[F]): F[Prod[Id]] =
    Sequence[Prod, Apply].sequence(p)

  def sequenceC[F[_]](c: Cop[F])(implicit F: Functor[F]): F[Cop[Id]] =
    Sequence[Cop, Functor].sequence(c)

  def extractC[F[_], B](c: Cop[F])(implicit inj: Inj[Option[B], Cop[F]]): Option[B] = inj(c)

  def extractP[F[_], B](p: Prod[F])(implicit inj: Inj[B, Prod[F]]): B = inj(p)

  // format: off
  def foldMap[F[_], C](p: Prod[F])(map: Cop[Id] => C)(
    implicit O: Ordering[Cop[Id]],
    M: Monoid[C],
    PE: PlusEmpty[F],
    U: Uncons[F]
  ): C = {
    import scala.collection.mutable.{PriorityQueue => PQ}

    def uncons(p: Prod[F]): (List[Cop[Id]], Prod[F]) = {
      val (h1, t1) = U(p.run)
      (List(h1.map(inj(_: Id[A1]))).flatten,
        Prod[F](t1))
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
    def go(prod: Prod[F], q: PQ[Cop[Id]], out: C): C =
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
              go(Prod[F](t),
                q ++= h.map(inj(_: Id[A1])), M.append(out, map(Cop[Id](dj))))

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
