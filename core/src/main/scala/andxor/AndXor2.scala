package andxor

import andxor.types.{Cop2, Prod2}
import scala.annotation.tailrec
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.std.vector._

trait AndXorK2[F[_], A1, A2] extends AndXor {
  type Prod = Prod2[F, A1, A2]
  object Prod { def apply(p: (F[A1], F[A2])): Prod = Prod2[F, A1, A2](p) }

  type Cop = Cop2[F, A1, A2]
  object Cop { def apply(c: (F[A1] \/ F[A2])): Cop = Cop2[F, A1, A2](c) }

  val AndXorF = AndXorF2[A1, A2]
  type AndXor[G[_]] = AndXorF.Repr[G]

  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose2(a0, a1)(f(_).run)

      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly2(a0, a1)(x => f(Cop(x)))

      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide2(a0, a1)(f(_).run)

      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply2(a0, a1) {
          case (i0, i1) =>
            f(Prod((i0, i1)))
        }

    }

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit val injEv: Inj[Cop, Cop] = combine[Inj[Cop, ?]].choose
    implicit def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj[Prod, ?]].divide
    implicit def injCopToProdEv(implicit M: Monoid[Prod]): Inj[Prod, Cop] = combine[Inj[Prod, ?]].choose
    implicit val injProdToVecCopEv: Inj[Vector[Cop], Prod] = combine[Inj[Vector[Cop], ?]].divide
  }

  def transformP[G[_]](nt: (F ~> G)): AndXorK2[F, A1, A2]#Prod => AndXorK2[G, A1, A2]#Prod =
    (p: AndXorK2[F, A1, A2]#Prod) => {
      val pr = p.run
      Prod2[G, A1, A2]((nt(pr._1), nt(pr._2)))
    }

  def transformC[G[_]](nt: (F ~> G)): AndXorK2[F, A1, A2]#Cop => AndXorK2[G, A1, A2]#Cop =
    (p: AndXorK2[F, A1, A2]#Cop) =>
      Cop2[G, A1, A2](
        p.run.bimap(nt(_), nt(_))
      )

  def subst1[G[_]]: AndXor2[G[A1], F[A2]] = AndXor2[G[A1], F[A2]]

  def subst2[G[_]]: AndXor2[F[A1], G[A2]] = AndXor2[F[A1], G[A2]]

  // format: off
  def sequenceP(prod: Prod)(implicit A: Apply[F]): F[Prod2[Id, A1, A2]] = {
    val p = prod.run
    A.map(
    A.ap(p._2)(
    A.map(p._1)((i0: A1) => (i1: A2) =>
      (i0, i1))))(Prod2[Id, A1, A2](_))
  }

  def sequenceC(cop: Cop)(implicit FF: Functor[F]): F[Cop2[Id, A1, A2]] =
    cop.run match {
      case -\/(x) => FF.map(x)(y => Cop2[Id, A1, A2](-\/(y)))
      case \/-(x) => FF.map(x)(y => Cop2[Id, A1, A2](\/-(y)))
    }

  def extractC[B](c: Cop)(implicit inj: Inj[Option[B], Cop]): Option[B] = inj(c)

  def extractP[B](p: Prod)(implicit inj: Inj[B, Prod]): B = inj(p)

  def foldMap[G[_], C](p: AndXor[G]#Prod)(map: AndXor[Id]#Cop => C)(
      implicit O: Ordering[AndXor[Id]#Cop], M: Monoid[C], PE: PlusEmpty[G], U: Uncons[G]): C = {
    import scala.collection.mutable.{PriorityQueue => PQ}

    val TG = AndXorF[G]
    val TI = AndXorF[Id]

    def uncons(p: TG.Prod): (List[TI.Cop], TG.Prod) = {
      val pr = p.run
      val ht1 = U(pr._1)
      val ht2 = U(pr._2)
      (List(ht1._1.map(TI.inj(_: Id[A1])), ht2._1.map(TI.inj(_: Id[A2]))).flatten,
        TG.Prod((ht1._2, ht2._2)))
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
      (prod.run.==((PE.empty[A1], PE.empty[A2]))) match {
        case true => appendAll(out, q)
        case false => q.isEmpty match {
          case true => {
            val (hs, ts) = uncons(prod)
            q ++= hs
            go(ts, q, out)
          }
          case false => q.dequeue.run match {
            case dj @ -\/(_) =>
              val pr = prod.run
              val (h, t) = U(pr._1)
              go(TG.Prod((t, pr._2)),
                q ++= h.map(TI.inj(_: Id[A1])), M.append(out, map(TI.Cop(dj))))
            case dj @ \/-(_) =>
              val pr = prod.run
              val (h, t) = U(pr._2)
              go(TG.Prod((pr._1, t)),
                q ++= h.map(TI.inj(_: Id[A2])), M.append(out, map(TI.Cop(dj))))

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

object AndXorK2 {

  def apply[F[_], A1, A2]: AndXorK2[F, A1, A2] =
    new AndXorK2[F, A1, A2] {}
}

trait AndXorF2[A1, A2] {
  type Repr[F[_]] = AndXorK2[F, A1, A2]
  def apply[F[_]]: Repr[F] =
    new AndXorK2[F, A1, A2] {}
}

object AndXorF2 {
  def apply[A1, A2]: AndXorF2[A1, A2] =
    new AndXorF2[A1, A2] {}
}

trait AndXor2[A1, A2] extends AndXorK2[Id, A1, A2]

object AndXor2 {
  def apply[A1, A2]: AndXor2[A1, A2] =
    new AndXor2[A1, A2] {}
}
