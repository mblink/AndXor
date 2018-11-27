package andxor
import scala.language.higherKinds
import scalaz.{Apply, Foldable, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.std.list._
import scalaz.syntax.either._

trait AndXorK2[F[_], A1, A2] extends AndXor {
  type Prod = (F[A1], F[A2])
  type Cop = (F[A1] \/ F[A2])
  val AndXorF = AndXorF2[A1, A2]
  type AndXor[G[_]] = AndXorF2[A1, A2]#Repr[G]
  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose2(a0, a1)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly2(a0, a1)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide2(a0, a1)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply2(a0, a1)((i0, i1) => f((i0, i1)))
    }

  object instances {

    implicit val inja0: Inj[Cop, F[A1]] =
      Inj.instance(_.left[F[A2]])

    implicit val inja0Inverse: Inj[Option[F[A1]], Cop] =
      Inj.instance(_ match {
        case -\/(x) => Some(x)
        case _      => None
      })

    implicit val inja1: Inj[Cop, F[A2]] =
      Inj.instance(_.right[F[A1]])

    implicit val inja1Inverse: Inj[Option[F[A2]], Cop] =
      Inj.instance(_ match {
        case \/-(x) => Some(x)
        case _      => None
      })

    implicit def lifta0(implicit M: Monoid[Prod]): Inj[Prod, F[A1]] = {
      val (_, a0) =
        M.zero
      Inj.instance((_, a0))
    }

    implicit val lifta0Inverse: Inj[F[A1], Prod] = Inj.instance(_._1)

    implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, F[A2]] = {
      val (a0, _) =
        M.zero
      Inj.instance((a0, _))
    }

    implicit val lifta1Inverse: Inj[F[A2], Prod] = Inj.instance(_._2)

  }

  import instances._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK2[F, A1, A2]#Prod => AndXorK2[G, A1, A2]#Prod =
    (p: AndXorK2[F, A1, A2]#Prod) => (nt(p._1), nt(p._2))

  def transformC[G[_]](nt: (F ~> G)): AndXorK2[F, A1, A2]#Cop => AndXorK2[G, A1, A2]#Cop =
    (p: AndXorK2[F, A1, A2]#Cop) => p.bimap(nt(_), nt(_))

  // format: off
  def sequenceP(prod: Prod)(implicit A: Apply[F]): F[AndXorK2[Id, A1, A2]#Prod] = {
    val (a0, a1) = prod
    A.ap(a1)(
     A.map(a0)(((i0: A1, i1: A2) =>
    (i0, i1)).curried))
  }
  

  def extractC[B](c: Cop)(implicit inj: Inj[Option[B], Cop]): Option[B] = inj(c)

  def extractP[B](p: Prod)(implicit inj: Inj[B, Prod]): B = inj(p)

  def toListP(p: Prod): List[Cop] = combine[Inj.Aux[List[Cop]]#Out].divide.apply(p)

  def foldMap[G[_], C](p: AndXor[G]#Prod)(
    map: AndXor[Id]#Cop => C)(
    implicit O: Ordering[AndXorK2[Id, A1, A2]#Cop], M: Monoid[C],
    PE: PlusEmpty[G], U: Uncons[G]): C = {
    val TG = AndXorF[G]
    val TI = AndXorF[Id]
    import scala.collection.mutable.{PriorityQueue => PQ}
    import TI.instances._
    def uncons(p: TG.Prod): (List[TI.Cop], TG.Prod) = {
     val hts = (U(p._1), U(p._2))
     (List(hts._1._1.map(TI.inj(_: A1)), hts._2._1.map(TI.inj(_: A2))).flatten,
      (hts._1._2, hts._2._2))
    }
    @scala.annotation.tailrec
    def go(prod: TG.Prod, q: PQ[TI.Cop], out: C): C =
     (prod.==((PE.empty[A1], PE.empty[A2]))) match {
       case true =>
         q.foldLeft(out)((acc, el) => M.append(acc, map(el)))
       case false => q.isEmpty match {
         case true => {
           val (hs, ts) = uncons(prod)
           q ++= hs
           go(ts, q, out)
         }
         case false => q.dequeue match {
                        case -\/(x) => {
               val (h, t) = U(prod._1)
               go((t, prod._2),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(x) => {
               val (h, t) = U(prod._2)
               go((prod._1, t),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }

         }
       }
     }
    val Q = new scala.collection.mutable.PriorityQueue[TI.Cop]()
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
