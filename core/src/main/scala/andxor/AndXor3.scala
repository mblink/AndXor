package andxor
import scala.language.higherKinds
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id

trait AndXorK3[F[_], A1, A2, A3] extends AndXor {
  type Prod = (F[A1], F[A2], F[A3])
  type Cop = (F[A1] \/ (F[A2] \/ F[A3]))
  val AndXorF = AndXorF3[A1, A2, A3]
  type AndXor[G[_]] = AndXorF3[A1, A2, A3]#Repr[G]
  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose3(a0, a1, a2)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly3(a0, a1, a2)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide3(a0, a1, a2)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply3(a0, a1, a2)((i0, i1, i2) => f((i0, i1, i2)))
    }

  object instances {

    implicit val prisma0: Prism[Cop, F[A1]] = new Prism[Cop, F[A1]] {
      def getOption(c: Cop): Option[F[A1]] = c match {
        case -\/(x) => Some(x)
        case _      => None
      }
      def reverseGet(x: F[A1]): Cop = -\/(x)
    }

    implicit val inja0: Inj[Cop, F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit val inja0Inverse: Inj[Option[F[A1]], Cop] = Inj.instance(prisma0.getOption(_))

    implicit val prisma1: Prism[Cop, F[A2]] = new Prism[Cop, F[A2]] {
      def getOption(c: Cop): Option[F[A2]] = c match {
        case \/-(-\/(x)) => Some(x)
        case _           => None
      }
      def reverseGet(x: F[A2]): Cop = \/-(-\/(x))
    }

    implicit val inja1: Inj[Cop, F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit val inja1Inverse: Inj[Option[F[A2]], Cop] = Inj.instance(prisma1.getOption(_))

    implicit val prisma2: Prism[Cop, F[A3]] = new Prism[Cop, F[A3]] {
      def getOption(c: Cop): Option[F[A3]] = c match {
        case \/-(\/-(x)) => Some(x)
        case _           => None
      }
      def reverseGet(x: F[A3]): Cop = \/-(\/-(x))
    }

    implicit val inja2: Inj[Cop, F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit val inja2Inverse: Inj[Option[F[A3]], Cop] = Inj.instance(prisma2.getOption(_))

    implicit def lifta0(implicit M: Monoid[Prod]): Inj[Prod, F[A1]] = {
      val (_, a0, a1) =
        M.zero
      Inj.instance((_, a0, a1))
    }

    implicit val lifta0Inverse: Inj[F[A1], Prod] = Inj.instance(_._1)

    implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, F[A2]] = {
      val (a0, _, a1) =
        M.zero
      Inj.instance((a0, _, a1))
    }

    implicit val lifta1Inverse: Inj[F[A2], Prod] = Inj.instance(_._2)

    implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, F[A3]] = {
      val (a0, a1, _) =
        M.zero
      Inj.instance((a0, a1, _))
    }

    implicit val lifta2Inverse: Inj[F[A3], Prod] = Inj.instance(_._3)

  }

  import instances._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK3[F, A1, A2, A3]#Prod => AndXorK3[G, A1, A2, A3]#Prod =
    (p: AndXorK3[F, A1, A2, A3]#Prod) => (nt(p._1), nt(p._2), nt(p._3))

  def transformC[G[_]](nt: (F ~> G)): AndXorK3[F, A1, A2, A3]#Cop => AndXorK3[G, A1, A2, A3]#Cop =
    (p: AndXorK3[F, A1, A2, A3]#Cop) => p.bimap(nt(_), _.bimap(nt(_), nt(_)))

  // format: off
  def sequenceP(prod: Prod)(implicit A: Apply[F]): F[AndXorK3[Id, A1, A2, A3]#Prod] = {
    val (a0, a1, a2) = prod
    A.ap(a2)(
    A.ap(a1)(
     A.map(a0)(((i0: A1, i1: A2, i2: A3) =>
    (i0, i1, i2)).curried)))
  }

  def sequenceC(cop: Cop)(implicit FF: Functor[F]): F[AndXorK3[Id, A1, A2, A3]#Cop] =
    cop match {
      case -\/(x) => FF.map(x)(y => -\/(y))
      case \/-(-\/(x)) => FF.map(x)(y => \/-(-\/(y)))
      case \/-(\/-(x)) => FF.map(x)(y => \/-(\/-(y)))
    }

  def extractC[B](c: Cop)(implicit inj: Inj[Option[B], Cop]): Option[B] = inj(c)

  def extractP[B](p: Prod)(implicit inj: Inj[B, Prod]): B = inj(p)

  def foldMap[G[_], C](p: AndXor[G]#Prod)(
    map: AndXor[Id]#Cop => C)(
    implicit O: Ordering[AndXorK3[Id, A1, A2, A3]#Cop], M: Monoid[C],
    PE: PlusEmpty[G], U: Uncons[G]): C = {
    val TG = AndXorF[G]
    val TI = AndXorF[Id]
    import scala.collection.mutable.{PriorityQueue => PQ}
    import TI.instances._
    def uncons(p: TG.Prod): (List[TI.Cop], TG.Prod) = {
     val hts = (U(p._1), U(p._2), U(p._3))
     (List(hts._1._1.map(TI.inj(_: A1)), hts._2._1.map(TI.inj(_: A2)), hts._3._1.map(TI.inj(_: A3))).flatten,
      (hts._1._2, hts._2._2, hts._3._2))
    }
    @scala.annotation.tailrec
    def go(prod: TG.Prod, q: PQ[TI.Cop], out: C): C =
     (prod.==((PE.empty[A1], PE.empty[A2], PE.empty[A3]))) match {
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
               go((t, prod._2, prod._3),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(-\/(x)) => {
               val (h, t) = U(prod._2)
               go((prod._1, t, prod._3),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(x)) => {
               val (h, t) = U(prod._3)
               go((prod._1, prod._2, t),
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
