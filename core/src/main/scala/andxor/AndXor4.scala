package andxor
import andxor.MapN.syntax._
import scala.language.higherKinds
import scalaz.{Apply, Foldable, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.Isomorphism.{<=>, IsoSet}
import scalaz.std.list._

trait AndXorK4[F[_], A1, A2, A3, A4] extends AndXor {
  type Prod = (F[A1], F[A2], F[A3], F[A4])
  type Cop = (F[A1] \/ (F[A2] \/ (F[A3] \/ F[A4])))
  val AndXorF = AndXorF4[A1, A2, A3, A4]
  type AndXor[G[_]] = AndXorF4[A1, A2, A3, A4]#Repr[G]
  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]], a3: G[F[A4]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose4(a0, a1, a2, a3)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly4(a0, a1, a2, a3)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide4(a0, a1, a2, a3)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply4(a0, a1, a2, a3)((i0, i1, i2, i3) => f((i0, i1, i2, i3)))
    }

  object instances {

    implicit val inja0: Inj[Cop, F[A1]] =
      Inj.instance(x => -\/(x))

    implicit val inja0Inverse: Inj[Option[F[A1]], Cop] =
      Inj.instance(_ match {
        case -\/(x) => Some(x)
        case _      => None
      })

    implicit val inja1: Inj[Cop, F[A2]] =
      Inj.instance(x => \/-(-\/(x)))

    implicit val inja1Inverse: Inj[Option[F[A2]], Cop] =
      Inj.instance(_ match {
        case \/-(-\/(x)) => Some(x)
        case _           => None
      })

    implicit val inja2: Inj[Cop, F[A3]] =
      Inj.instance(x => \/-(\/-(-\/(x))))

    implicit val inja2Inverse: Inj[Option[F[A3]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _                => None
      })

    implicit val inja3: Inj[Cop, F[A4]] =
      Inj.instance(x => \/-(\/-(\/-(x))))

    implicit val inja3Inverse: Inj[Option[F[A4]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(x))) => Some(x)
        case _                => None
      })

    implicit def liftisoa0(implicit M: Monoid[Prod]): Prod <=> F[A1] =
      IsoSet(_._1, x => M.zero.map1(_ => x))

    implicit def liftisoa1(implicit M: Monoid[Prod]): Prod <=> F[A2] =
      IsoSet(_._2, x => M.zero.map2(_ => x))

    implicit def liftisoa2(implicit M: Monoid[Prod]): Prod <=> F[A3] =
      IsoSet(_._3, x => M.zero.map3(_ => x))

    implicit def liftisoa3(implicit M: Monoid[Prod]): Prod <=> F[A4] =
      IsoSet(_._4, x => M.zero.map4(_ => x))

  }

  import instances._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK4[F, A1, A2, A3, A4]#Prod => AndXorK4[G, A1, A2, A3, A4]#Prod =
    (p: AndXorK4[F, A1, A2, A3, A4]#Prod) => (nt(p._1), nt(p._2), nt(p._3), nt(p._4))

  def transformC[G[_]](nt: (F ~> G)): AndXorK4[F, A1, A2, A3, A4]#Cop => AndXorK4[G, A1, A2, A3, A4]#Cop =
    (p: AndXorK4[F, A1, A2, A3, A4]#Cop) => p.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_))))

  // format: off
  def sequenceP(prod: Prod)(implicit A: Apply[F]): F[AndXorK4[Id, A1, A2, A3, A4]#Prod] = {
    val (a0, a1, a2, a3) = prod
    A.ap(a3)(
    A.ap(a2)(
    A.ap(a1)(
     A.map(a0)(((i0: A1, i1: A2, i2: A3, i3: A4) =>
    (i0, i1, i2, i3)).curried))))
  }
  

  def extractC[B](c: Cop)(implicit inj: Inj[Option[B], Cop]): Option[B] = inj(c)

  def extractP[B](p: Prod)(implicit inj: Inj[B, Prod]): B = inj(p)

  def toListP(p: Prod): List[Cop] = combine[Inj.Aux[List[Cop]]#Out].divide.apply(p)

  def foldMap[G[_], C](p: AndXor[G]#Prod)(
    map: AndXor[Id]#Cop => C)(
    implicit O: Ordering[AndXorK4[Id, A1, A2, A3, A4]#Cop], M: Monoid[C],
    PE: PlusEmpty[G], U: Uncons[G]): C = {
    val TG = AndXorF[G]
    val TI = AndXorF[Id]
    import scala.collection.mutable.{PriorityQueue => PQ}
    import TI.instances._
    def uncons(p: TG.Prod): (List[TI.Cop], TG.Prod) = {
     val hts = (U(p._1), U(p._2), U(p._3), U(p._4))
     (List(hts._1._1.map(TI.inj(_: A1)), hts._2._1.map(TI.inj(_: A2)), hts._3._1.map(TI.inj(_: A3)), hts._4._1.map(TI.inj(_: A4))).flatten,
      (hts._1._2, hts._2._2, hts._3._2, hts._4._2))
    }
    @scala.annotation.tailrec
    def go(prod: TG.Prod, q: PQ[TI.Cop], out: C): C =
     (prod.==((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4]))) match {
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
               go((t, prod._2, prod._3, prod._4),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(-\/(x)) => {
               val (h, t) = U(prod._2)
               go((prod._1, t, prod._3, prod._4),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(-\/(x))) => {
               val (h, t) = U(prod._3)
               go((prod._1, prod._2, t, prod._4),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(x))) => {
               val (h, t) = U(prod._4)
               go((prod._1, prod._2, prod._3, t),
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

object AndXorK4 {

  def apply[F[_], A1, A2, A3, A4]: AndXorK4[F, A1, A2, A3, A4] =
    new AndXorK4[F, A1, A2, A3, A4] {}
}

trait AndXorF4[A1, A2, A3, A4] {
  type Repr[F[_]] = AndXorK4[F, A1, A2, A3, A4]
  def apply[F[_]]: Repr[F] =
    new AndXorK4[F, A1, A2, A3, A4] {}
}

object AndXorF4 {
  def apply[A1, A2, A3, A4]: AndXorF4[A1, A2, A3, A4] =
    new AndXorF4[A1, A2, A3, A4] {}
}

trait AndXor4[A1, A2, A3, A4] extends AndXorK4[Id, A1, A2, A3, A4]

object AndXor4 {
  def apply[A1, A2, A3, A4]: AndXor4[A1, A2, A3, A4] =
    new AndXor4[A1, A2, A3, A4] {}

}
