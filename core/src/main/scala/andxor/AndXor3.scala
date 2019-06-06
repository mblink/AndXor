package andxor

import andxor.types.{Cop3, Prod3}
import scala.annotation.tailrec
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXorK3[F[_], A1, A2, A3] extends AndXor {
  type Prod = Prod3[F, A1, A2, A3]
  object Prod { def apply(p: (F[A1], F[A2], F[A3])): Prod = Prod3[F, A1, A2, A3](p) }

  type Cop = Cop3[F, A1, A2, A3]
  object Cop { def apply(c: (F[A1] \/ (F[A2] \/ F[A3]))): Cop = Cop3[F, A1, A2, A3](c) }

  val AndXorF = AndXorF3[A1, A2, A3]
  type AndXor[G[_]] = AndXorF.Repr[G]

  def combineId[G[_]](implicit @scalaz.unused ev: F[_] =:= Id[_], a0: G[A1], a1: G[A2], a2: G[A3]): ComposeAndXor[G, Cop3[Id, A1, A2, A3], Prod3[Id, A1, A2, A3]] =
    AndXor3[A1, A2, A3].combine[G]

  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose3(a0, a1, a2)(f(_).run)

      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly3(a0, a1, a2)(x => f(Cop(x)))

      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide3(a0, a1, a2)(f(_).run)

      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply3(a0, a1, a2) {
          case (i0, i1, i2) =>
            f(Prod((i0, i1, i2)))
        }

    }

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit val injEv: Inj[Cop, Cop] = combine[Inj[Cop, ?]].choose
    implicit def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj[Prod, ?]].divide
    implicit def injCopToProdEv(implicit M: Monoid[Prod]): Inj[Prod, Cop] = combine[Inj[Prod, ?]].choose
    implicit val injProdToVecCopEv: Inj[Vector[Cop], Prod] = combine[Inj[Vector[Cop], ?]].divide
  }

  def transformP[G[_]](nt: (F ~> G)): AndXorK3[F, A1, A2, A3]#Prod => AndXorK3[G, A1, A2, A3]#Prod =
    (p: AndXorK3[F, A1, A2, A3]#Prod) => Prod3[G, A1, A2, A3]((nt(p.t1), nt(p.t2), nt(p.t3)))

  def transformC[G[_]](nt: (F ~> G)): AndXorK3[F, A1, A2, A3]#Cop => AndXorK3[G, A1, A2, A3]#Cop =
    (p: AndXorK3[F, A1, A2, A3]#Cop) =>
      Cop3[G, A1, A2, A3](
        p.run.bimap(nt(_), _.bimap(nt(_), nt(_)))
      )

  def subst1[G[_]]: AndXor3[G[A1], F[A2], F[A3]] = AndXor3[G[A1], F[A2], F[A3]]

  def subst2[G[_]]: AndXor3[F[A1], G[A2], F[A3]] = AndXor3[F[A1], G[A2], F[A3]]

  def subst3[G[_]]: AndXor3[F[A1], F[A2], G[A3]] = AndXor3[F[A1], F[A2], G[A3]]

  // format: off
  def sequenceP(p: Prod)(implicit A: Apply[F]): F[Prod3[Id, A1, A2, A3]] = {
    A.map(
    A.ap(p.t3)(
    A.ap(p.t2)(
    A.map(p.t1)((i0: A1) => (i1: A2) => (i2: A3) =>
      (i0, i1, i2)))))(Prod3[Id, A1, A2, A3](_))
  }

  def sequenceC(cop: Cop)(implicit FF: Functor[F]): F[Cop3[Id, A1, A2, A3]] =
    cop.run match {
      case -\/(x) => FF.map(x)(y => Cop3[Id, A1, A2, A3](-\/(y)))
      case \/-(-\/(x)) => FF.map(x)(y => Cop3[Id, A1, A2, A3](\/-(-\/(y))))
      case \/-(\/-(x)) => FF.map(x)(y => Cop3[Id, A1, A2, A3](\/-(\/-(y))))
    }

  def extractC[B](c: Cop)(implicit inj: Inj[Option[B], Cop]): Option[B] = inj(c)

  def extractP[B](p: Prod)(implicit inj: Inj[B, Prod]): B = inj(p)

  def foldMap[G[_], C](p: AndXor[G]#Prod)(map: AndXor[Id]#Cop => C)(
      implicit O: Ordering[Cop3[Id, A1, A2, A3]], M: Monoid[C], PE: PlusEmpty[G], U: Uncons[G]): C = {
    import scala.collection.mutable.{PriorityQueue => PQ}

    val TG = AndXorF[G]
    val TI = AndXorF[Id]

    def uncons(p: TG.Prod): (List[TI.Cop], TG.Prod) = {
      val ht1 = U(p.t1)
      val ht2 = U(p.t2)
      val ht3 = U(p.t3)
      (List(ht1._1.map(TI.inj(_: Id[A1])), ht2._1.map(TI.inj(_: Id[A2])), ht3._1.map(TI.inj(_: Id[A3]))).flatten,
        TG.Prod((ht1._2, ht2._2, ht3._2)))
    }

    @tailrec
    def appendAll(out: C, q: PQ[TI.Cop]): C =
      q.isEmpty match {
        case true => out
        case false =>
          val newOut = M.append(out, map(q.dequeue))
          appendAll(newOut, q)
      }

    @tailrec
    def go(prod: TG.Prod, q: PQ[TI.Cop], out: C): C =
      (prod.run.==((PE.empty[A1], PE.empty[A2], PE.empty[A3]))) match {
        case true => appendAll(out, q)
        case false => q.isEmpty match {
          case true => {
            val (hs, ts) = uncons(prod)
            q ++= hs
            go(ts, q, out)
          }
          case false => q.dequeue.run match {
            case dj @ -\/(_) =>
              val (h, t) = U(prod.t1)
              go(TG.Prod((t, prod.t2, prod.t3)),
                q ++= h.map(TI.inj(_: Id[A1])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(-\/(_)) =>
              val (h, t) = U(prod.t2)
              go(TG.Prod((prod.t1, t, prod.t3)),
                q ++= h.map(TI.inj(_: Id[A2])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(\/-(_)) =>
              val (h, t) = U(prod.t3)
              go(TG.Prod((prod.t1, prod.t2, t)),
                q ++= h.map(TI.inj(_: Id[A3])), M.append(out, map(TI.Cop(dj))))

          }
        }
      }
    val Q = new PQ[TI.Cop]()(O)
    val (hs, ts) = uncons(p)
    Q ++= hs
    go(ts, Q, M.zero)
  }
  // format: on
}

object AndXorK3 {

  def apply[F[_], A1, A2, A3]: AndXorK3[F, A1, A2, A3] =
    new AndXorK3[F, A1, A2, A3] {}
}

trait AndXorF3[A1, A2, A3] {
  type Repr[F[_]] = AndXorK3[F, A1, A2, A3]
  def apply[F[_]]: Repr[F] =
    new AndXorK3[F, A1, A2, A3] {}
}

object AndXorF3 {
  def apply[A1, A2, A3]: AndXorF3[A1, A2, A3] =
    new AndXorF3[A1, A2, A3] {}
}

trait AndXor3[A1, A2, A3] extends AndXorK3[Id, A1, A2, A3]

object AndXor3 {
  def apply[A1, A2, A3]: AndXor3[A1, A2, A3] =
    new AndXor3[A1, A2, A3] {}
}
