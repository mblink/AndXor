package andxor

import andxor.types.{Cop5, Prod5}
import scala.annotation.tailrec
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id

trait AndXor5[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor] extends AndXor {
  type Prod[F[_]] = Prod5[F, A1, A2, A3, A4, A5]
  object Prod {
    def apply[F[_]](p: (A1#Prod[F], A2#Prod[F], A3#Prod[F], A4#Prod[F], A5#Prod[F])): Prod[F] = Prod5[F, A1, A2, A3, A4, A5](p)
  }

  type Cop[F[_]] = Cop5[F, A1, A2, A3, A4, A5]
  object Cop {
    def apply[F[_]](c: (A1#Cop[F] \/ (A2#Cop[F] \/ (A3#Cop[F] \/ (A4#Cop[F] \/ A5#Cop[F]))))): Cop[F] = Cop5[F, A1, A2, A3, A4, A5](c)
  }

  def mkChoose[TC[_], F[_], B](f: B => Cop[F])(implicit d: Decidable[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]], a2: TC[A3#Cop[F]], a3: TC[A4#Cop[F]], a4: TC[A5#Cop[F]]): TC[B] =
    Combine.choose5(a0, a1, a2, a3, a4)(f(_).run)

  def mkAlt[TC[_], F[_], B](f: Cop[F] => B)(implicit a: Alt[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]], a2: TC[A3#Cop[F]], a3: TC[A4#Cop[F]], a4: TC[A5#Cop[F]]): TC[B] =
    Combine.altly5(a0, a1, a2, a3, a4)(x => f(Cop(x)))

  def mkDivide[TC[_], F[_], B](f: B => Prod[F])(implicit d: Divide[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]], a2: TC[A3#Prod[F]], a3: TC[A4#Prod[F]], a4: TC[A5#Prod[F]]): TC[B] =
    Combine.divide5(a0, a1, a2, a3, a4)(f(_).run)

  def mkApply[TC[_], F[_], B](f: Prod[F] => B)(implicit a: Apply[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]], a2: TC[A3#Prod[F]], a3: TC[A4#Prod[F]], a4: TC[A5#Prod[F]]): TC[B] =
    Combine.apply5(a0, a1, a2, a3, a4) {
      case (i0, i1, i2, i3, i4) =>
        f(Prod((i0, i1, i2, i3, i4)))
    }

  def mkChoose[TC[_], B](f: B => Cop[Id])(implicit d: Decidable[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], a2: TC[A3#Cop[Id]], a3: TC[A4#Cop[Id]], a4: TC[A5#Cop[Id]], dummy: DummyImplicit): TC[B] =
    mkChoose[TC, Id, B](f)
  def mkAlt[TC[_], B](f: Cop[Id] => B)(implicit a: Alt[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], a2: TC[A3#Cop[Id]], a3: TC[A4#Cop[Id]], a4: TC[A5#Cop[Id]], dummy: DummyImplicit): TC[B] =
    mkAlt[TC, Id, B](f)
  def mkDivide[TC[_], B](
      f: B => Prod[Id]
  )(implicit d: Divide[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], a2: TC[A3#Prod[Id]], a3: TC[A4#Prod[Id]], a4: TC[A5#Prod[Id]], dummy: DummyImplicit): TC[B] = mkDivide[TC, Id, B](f)
  def mkApply[TC[_], B](f: Prod[Id] => B)(implicit a: Apply[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], a2: TC[A3#Prod[Id]], a3: TC[A4#Prod[Id]], a4: TC[A5#Prod[Id]], dummy: DummyImplicit): TC[B] =
    mkApply[TC, Id, B](f)

  def choose[TC[_], F[_]](implicit d: Decidable[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]], a2: TC[A3#Cop[F]], a3: TC[A4#Cop[F]], a4: TC[A5#Cop[F]]): TC[Cop[F]] = mkChoose[TC, F, Cop[F]](identity)
  def alt[TC[_], F[_]](implicit a: Alt[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]], a2: TC[A3#Cop[F]], a3: TC[A4#Cop[F]], a4: TC[A5#Cop[F]]): TC[Cop[F]] = mkAlt[TC, F, Cop[F]](identity)
  def divide[TC[_], F[_]](implicit d: Divide[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]], a2: TC[A3#Prod[F]], a3: TC[A4#Prod[F]], a4: TC[A5#Prod[F]]): TC[Prod[F]] = mkDivide[TC, F, Prod[F]](identity)
  def apply[TC[_], F[_]](implicit a: Apply[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]], a2: TC[A3#Prod[F]], a3: TC[A4#Prod[F]], a4: TC[A5#Prod[F]]): TC[Prod[F]] = mkApply[TC, F, Prod[F]](identity)

  def choose[TC[_]](implicit d: Decidable[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], a2: TC[A3#Cop[Id]], a3: TC[A4#Cop[Id]], a4: TC[A5#Cop[Id]], dummy: DummyImplicit): TC[Cop[Id]] =
    mkChoose[TC, Cop[Id]](identity)
  def alt[TC[_]](implicit a: Alt[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], a2: TC[A3#Cop[Id]], a3: TC[A4#Cop[Id]], a4: TC[A5#Cop[Id]], dummy: DummyImplicit): TC[Cop[Id]] =
    mkAlt[TC, Cop[Id]](identity)
  def divide[TC[_]](implicit d: Divide[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], a2: TC[A3#Prod[Id]], a3: TC[A4#Prod[Id]], a4: TC[A5#Prod[Id]], dummy: DummyImplicit): TC[Prod[Id]] =
    mkDivide[TC, Prod[Id]](identity)
  def apply[TC[_]](implicit a: Apply[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], a2: TC[A3#Prod[Id]], a3: TC[A4#Prod[Id]], a4: TC[A5#Prod[Id]], dummy: DummyImplicit): TC[Prod[Id]] =
    mkApply[TC, Prod[Id]](identity)

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = choose[Inj[Cop[F], ?], F]
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = divide[Inj[Prod[F], ?], F]
  }

  def transformP[F[_], G[_]](
      nt: (F ~> G)
  )(implicit trans0: Transform[A1#Prod], trans1: Transform[A2#Prod], trans2: Transform[A3#Prod], trans3: Transform[A4#Prod], trans4: Transform[A5#Prod]): Prod[F] => Prod[G] =
    Transform[Prod].transform(nt)

  def transformC[F[_], G[_]](
      nt: (F ~> G)
  )(implicit trans0: Transform[A1#Cop], trans1: Transform[A2#Cop], trans2: Transform[A3#Cop], trans3: Transform[A4#Cop], trans4: Transform[A5#Cop]): Cop[F] => Cop[G] =
    Transform[Cop].transform(nt)

  def sequenceP[F[_]](
      p: Prod[F]
  )(implicit F: Apply[F], seq0: Sequence[A1#Prod, Apply], seq1: Sequence[A2#Prod, Apply], seq2: Sequence[A3#Prod, Apply], seq3: Sequence[A4#Prod, Apply], seq4: Sequence[A5#Prod, Apply]): F[Prod[Id]] =
    Sequence[Prod, Apply].sequence(p)

  def sequenceC[F[_]](c: Cop[F])(
      implicit F: Functor[F],
      seq0: Sequence[A1#Cop, Functor],
      seq1: Sequence[A2#Cop, Functor],
      seq2: Sequence[A3#Cop, Functor],
      seq3: Sequence[A4#Cop, Functor],
      seq4: Sequence[A5#Cop, Functor]
  ): F[Cop[Id]] =
    Sequence[Cop, Functor].sequence(c)

  def extractC[F[_], B](c: Cop[F])(implicit inj: Inj[Option[B], Cop[F]]): Option[B] = inj(c)

  def extractP[F[_], B](p: Prod[F])(implicit inj: Inj[B, Prod[F]]): B = inj(p)

  // format: off
  def foldMap[F[_], C](p: Prod[F])(map: Cop[Id] => C)(
    implicit O: Ordering[Cop[Id]],
    M: Monoid[C],
    PE: PlusEmpty[F],
    U: Uncons[F],
    U0: Uncons0[A1#Prod, A1#Cop], U1: Uncons0[A2#Prod, A2#Cop], U2: Uncons0[A3#Prod, A3#Cop], U3: Uncons0[A4#Prod, A4#Cop], U4: Uncons0[A5#Prod, A5#Cop]
  ): C = {
    import scala.collection.mutable.{PriorityQueue => PQ}

    def uncons(p: Prod[F]): (List[Cop[Id]], Prod[F]) = {
      val (h1, t1) = U0(p.t1)
      val (h2, t2) = U1(p.t2)
      val (h3, t3) = U2(p.t3)
      val (h4, t4) = U3(p.t4)
      val (h5, t5) = U4(p.t5)
      (List(h1.map(inj(_: A1#Cop[Id])), h2.map(inj(_: A2#Cop[Id])), h3.map(inj(_: A3#Cop[Id])), h4.map(inj(_: A4#Cop[Id])), h5.map(inj(_: A5#Cop[Id]))).flatten,
        Prod[F]((t1, t2, t3, t4, t5)))
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
      (prod.run.==((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5]))) match {
        case true => appendAll(out, q)
        case false => q.isEmpty match {
          case true => {
            val (hs, ts) = uncons(prod)
            q ++= hs
            go(ts, q, out)
          }
          case false => q.dequeue.run match {
            case dj @ -\/(_) =>
              val (h, t) = U0(prod.t1)
              go(Prod[F]((t, prod.t2, prod.t3, prod.t4, prod.t5)),
                q ++= h.map(inj(_: A1#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(-\/(_)) =>
              val (h, t) = U1(prod.t2)
              go(Prod[F]((prod.t1, t, prod.t3, prod.t4, prod.t5)),
                q ++= h.map(inj(_: A2#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(-\/(_))) =>
              val (h, t) = U2(prod.t3)
              go(Prod[F]((prod.t1, prod.t2, t, prod.t4, prod.t5)),
                q ++= h.map(inj(_: A3#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(-\/(_)))) =>
              val (h, t) = U3(prod.t4)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, t, prod.t5)),
                q ++= h.map(inj(_: A4#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(_)))) =>
              val (h, t) = U4(prod.t5)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, t)),
                q ++= h.map(inj(_: A5#Cop[Id])), M.append(out, map(Cop[Id](dj))))

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

object AndXor5 {
  def apply[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor]: AndXor5[A1, A2, A3, A4, A5] =
    new AndXor5[A1, A2, A3, A4, A5] {}
}
