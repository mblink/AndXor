package andxor

import andxor.types.{Cop1, Prod1}
import scalaz.{Apply, Functor, PlusEmpty, Monoid, ~>}
import scalaz.Id.Id

trait AndXorK1[F[_], A1] extends AndXor {
  type Prod = Prod1[F, A1]
  object Prod { def apply(p: F[A1]): Prod = Prod1[F, A1](p) }

  type Cop = Cop1[F, A1]
  object Cop { def apply(c: F[A1]): Cop = Cop1[F, A1](c) }

  val AndXorF = AndXorF1[A1]
  type AndXor[G[_]] = AndXorF.Repr[G]

  def combine[G[_]](implicit a0: G[F[A1]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        d.contramap(a0)(f(_).run)

      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        a.map(a0)(x => f(Cop(x)))

      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        d.contramap(a0)(f(_).run)

      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        a.map(a0)(x => f(Prod(x)))
    }

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK1[F, A1]#Prod => AndXorK1[G, A1]#Prod =
    (p: AndXorK1[F, A1]#Prod) => {
      val pr = p.run
      Prod1[G, A1](nt(pr))
    }

  def transformC[G[_]](nt: (F ~> G)): AndXorK1[F, A1]#Cop => AndXorK1[G, A1]#Cop =
    (p: AndXorK1[F, A1]#Cop) => Cop1[G, A1](nt(p.run))

  def subst1[G[_]]: AndXor1[G[A1]] = AndXor1[G[A1]]

  // format: off
  def sequenceP(prod: Prod)(implicit A: Apply[F]): F[Prod1[Id, A1]] = {
    val p = prod.run
    A.map(
    A.map(p)((i0: A1) =>
      i0))(Prod1[Id, A1](_))
  }

  def sequenceC(cop: Cop)(implicit FF: Functor[F]): F[Cop1[Id, A1]] =
    cop.run match {
      case x => FF.map(x)(y => Cop1[Id, A1](y))
    }

  def extractC[B](c: Cop)(implicit inj: Inj[Option[B], Cop]): Option[B] = inj(c)

  def extractP[B](p: Prod)(implicit inj: Inj[B, Prod]): B = inj(p)

  def foldMap[G[_], C](p: AndXor[G]#Prod)(map: AndXor[Id]#Cop => C)(
      implicit O: Ordering[Cop1[Id, A1]], M: Monoid[C], PE: PlusEmpty[G], U: Uncons[G]): C = {
    import scala.collection.mutable.{PriorityQueue => PQ}

    val TG = AndXorF[G]
    val TI = AndXorF[Id]

    def uncons(p: TG.Prod): (List[TI.Cop], TG.Prod) = {
      val pr = p.run
      val ht1 = U(pr)
      (List(ht1._1.map(TI.inj(_: Id[A1]))).flatten,
        TG.Prod(ht1._2))
    }
    @scala.annotation.tailrec
    def go(prod: TG.Prod, q: PQ[TI.Cop], out: C): C =
      (prod.run.==(PE.empty[A1])) match {
        case true =>
          q.foldLeft(out)((acc, el) => M.append(acc, map(el)))
        case false => q.isEmpty match {
          case true => {
            val (hs, ts) = uncons(prod)
            q ++= hs
            go(ts, q, out)
          }
          case false => q.dequeue.run match {
            case x => {
              val pr = prod.run
              val (h, t) = U(pr)
              go(TG.Prod(t),
                q ++= h.map(TI.inj(_: Id[A1])), M.append(out, map(TI.inj(x))))
          }

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

object AndXorK1 {

  def apply[F[_], A1]: AndXorK1[F, A1] =
    new AndXorK1[F, A1] {}
}

trait AndXorF1[A1] {
  type Repr[F[_]] = AndXorK1[F, A1]
  def apply[F[_]]: Repr[F] =
    new AndXorK1[F, A1] {}
}

object AndXorF1 {
  def apply[A1]: AndXorF1[A1] =
    new AndXorF1[A1] {}
}

trait AndXor1[A1] extends AndXorK1[Id, A1]

object AndXor1 {
  def apply[A1]: AndXor1[A1] =
    new AndXor1[A1] {}
}
