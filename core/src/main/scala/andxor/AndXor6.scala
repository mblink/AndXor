package andxor
import scala.language.higherKinds
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id

trait AndXorK6[F[_], A1, A2, A3, A4, A5, A6] extends AndXor {
  type Prod = (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6])
  type Cop = (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ F[A6])))))
  val AndXorF = AndXorF6[A1, A2, A3, A4, A5, A6]
  type AndXor[G[_]] = AndXorF6[A1, A2, A3, A4, A5, A6]#Repr[G]
  def combine[G[_]](implicit a0: G[F[A1]], a1: G[F[A2]], a2: G[F[A3]], a3: G[F[A4]], a4: G[F[A5]], a5: G[F[A6]]): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose6(a0, a1, a2, a3, a4, a5)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly6(a0, a1, a2, a3, a4, a5)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide6(a0, a1, a2, a3, a4, a5)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply6(a0, a1, a2, a3, a4, a5)((i0, i1, i2, i3, i4, i5) => f((i0, i1, i2, i3, i4, i5)))
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
        case \/-(\/-(-\/(x))) => Some(x)
        case _                => None
      }
      def reverseGet(x: F[A3]): Cop = \/-(\/-(-\/(x)))
    }

    implicit val inja2: Inj[Cop, F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit val inja2Inverse: Inj[Option[F[A3]], Cop] = Inj.instance(prisma2.getOption(_))

    implicit val prisma3: Prism[Cop, F[A4]] = new Prism[Cop, F[A4]] {
      def getOption(c: Cop): Option[F[A4]] = c match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _                     => None
      }
      def reverseGet(x: F[A4]): Cop = \/-(\/-(\/-(-\/(x))))
    }

    implicit val inja3: Inj[Cop, F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit val inja3Inverse: Inj[Option[F[A4]], Cop] = Inj.instance(prisma3.getOption(_))

    implicit val prisma4: Prism[Cop, F[A5]] = new Prism[Cop, F[A5]] {
      def getOption(c: Cop): Option[F[A5]] = c match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _                          => None
      }
      def reverseGet(x: F[A5]): Cop = \/-(\/-(\/-(\/-(-\/(x)))))
    }

    implicit val inja4: Inj[Cop, F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit val inja4Inverse: Inj[Option[F[A5]], Cop] = Inj.instance(prisma4.getOption(_))

    implicit val prisma5: Prism[Cop, F[A6]] = new Prism[Cop, F[A6]] {
      def getOption(c: Cop): Option[F[A6]] = c match {
        case \/-(\/-(\/-(\/-(\/-(x))))) => Some(x)
        case _                          => None
      }
      def reverseGet(x: F[A6]): Cop = \/-(\/-(\/-(\/-(\/-(x)))))
    }

    implicit val inja5: Inj[Cop, F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit val inja5Inverse: Inj[Option[F[A6]], Cop] = Inj.instance(prisma5.getOption(_))

    implicit def lifta0(implicit M: Monoid[Prod]): Inj[Prod, F[A1]] = {
      val (_, a0, a1, a2, a3, a4) =
        M.zero
      Inj.instance((_, a0, a1, a2, a3, a4))
    }

    implicit val lifta0Inverse: Inj[F[A1], Prod] = Inj.instance(_._1)

    implicit def lifta1(implicit M: Monoid[Prod]): Inj[Prod, F[A2]] = {
      val (a0, _, a1, a2, a3, a4) =
        M.zero
      Inj.instance((a0, _, a1, a2, a3, a4))
    }

    implicit val lifta1Inverse: Inj[F[A2], Prod] = Inj.instance(_._2)

    implicit def lifta2(implicit M: Monoid[Prod]): Inj[Prod, F[A3]] = {
      val (a0, a1, _, a2, a3, a4) =
        M.zero
      Inj.instance((a0, a1, _, a2, a3, a4))
    }

    implicit val lifta2Inverse: Inj[F[A3], Prod] = Inj.instance(_._3)

    implicit def lifta3(implicit M: Monoid[Prod]): Inj[Prod, F[A4]] = {
      val (a0, a1, a2, _, a3, a4) =
        M.zero
      Inj.instance((a0, a1, a2, _, a3, a4))
    }

    implicit val lifta3Inverse: Inj[F[A4], Prod] = Inj.instance(_._4)

    implicit def lifta4(implicit M: Monoid[Prod]): Inj[Prod, F[A5]] = {
      val (a0, a1, a2, a3, _, a4) =
        M.zero
      Inj.instance((a0, a1, a2, a3, _, a4))
    }

    implicit val lifta4Inverse: Inj[F[A5], Prod] = Inj.instance(_._5)

    implicit def lifta5(implicit M: Monoid[Prod]): Inj[Prod, F[A6]] = {
      val (a0, a1, a2, a3, a4, _) =
        M.zero
      Inj.instance((a0, a1, a2, a3, a4, _))
    }

    implicit val lifta5Inverse: Inj[F[A6], Prod] = Inj.instance(_._6)

  }

  import instances._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK6[F, A1, A2, A3, A4, A5, A6]#Prod => AndXorK6[G, A1, A2, A3, A4, A5, A6]#Prod =
    (p: AndXorK6[F, A1, A2, A3, A4, A5, A6]#Prod) => (nt(p._1), nt(p._2), nt(p._3), nt(p._4), nt(p._5), nt(p._6))

  def transformC[G[_]](nt: (F ~> G)): AndXorK6[F, A1, A2, A3, A4, A5, A6]#Cop => AndXorK6[G, A1, A2, A3, A4, A5, A6]#Cop =
    (p: AndXorK6[F, A1, A2, A3, A4, A5, A6]#Cop) => p.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_))))))

  // format: off
  def sequenceP(prod: Prod)(implicit A: Apply[F]): F[AndXorK6[Id, A1, A2, A3, A4, A5, A6]#Prod] = {
    val (a0, a1, a2, a3, a4, a5) = prod
    A.ap(a5)(
    A.ap(a4)(
    A.ap(a3)(
    A.ap(a2)(
    A.ap(a1)(
     A.map(a0)(((i0: A1, i1: A2, i2: A3, i3: A4, i4: A5, i5: A6) =>
    (i0, i1, i2, i3, i4, i5)).curried))))))
  }

  def sequenceC(cop: Cop)(implicit FF: Functor[F]): F[AndXorK6[Id, A1, A2, A3, A4, A5, A6]#Cop] =
    cop match {
      case -\/(x) => FF.map(x)(y => -\/(y))
      case \/-(-\/(x)) => FF.map(x)(y => \/-(-\/(y)))
      case \/-(\/-(-\/(x))) => FF.map(x)(y => \/-(\/-(-\/(y))))
      case \/-(\/-(\/-(-\/(x)))) => FF.map(x)(y => \/-(\/-(\/-(-\/(y)))))
      case \/-(\/-(\/-(\/-(-\/(x))))) => FF.map(x)(y => \/-(\/-(\/-(\/-(-\/(y))))))
      case \/-(\/-(\/-(\/-(\/-(x))))) => FF.map(x)(y => \/-(\/-(\/-(\/-(\/-(y))))))
    }

  def extractC[B](c: Cop)(implicit inj: Inj[Option[B], Cop]): Option[B] = inj(c)

  def extractP[B](p: Prod)(implicit inj: Inj[B, Prod]): B = inj(p)

  def foldMap[G[_], C](p: AndXor[G]#Prod)(
    map: AndXor[Id]#Cop => C)(
    implicit O: Ordering[AndXorK6[Id, A1, A2, A3, A4, A5, A6]#Cop], M: Monoid[C],
    PE: PlusEmpty[G], U: Uncons[G]): C = {
    val TG = AndXorF[G]
    val TI = AndXorF[Id]
    import scala.collection.mutable.{PriorityQueue => PQ}
    import TI.instances._
    def uncons(p: TG.Prod): (List[TI.Cop], TG.Prod) = {
     val hts = (U(p._1), U(p._2), U(p._3), U(p._4), U(p._5), U(p._6))
     (List(hts._1._1.map(TI.inj(_: A1)), hts._2._1.map(TI.inj(_: A2)), hts._3._1.map(TI.inj(_: A3)), hts._4._1.map(TI.inj(_: A4)), hts._5._1.map(TI.inj(_: A5)), hts._6._1.map(TI.inj(_: A6))).flatten,
      (hts._1._2, hts._2._2, hts._3._2, hts._4._2, hts._5._2, hts._6._2))
    }
    @scala.annotation.tailrec
    def go(prod: TG.Prod, q: PQ[TI.Cop], out: C): C =
     (prod.==((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6]))) match {
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
               go((t, prod._2, prod._3, prod._4, prod._5, prod._6),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(-\/(x)) => {
               val (h, t) = U(prod._2)
               go((prod._1, t, prod._3, prod._4, prod._5, prod._6),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(-\/(x))) => {
               val (h, t) = U(prod._3)
               go((prod._1, prod._2, t, prod._4, prod._5, prod._6),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(-\/(x)))) => {
               val (h, t) = U(prod._4)
               go((prod._1, prod._2, prod._3, t, prod._5, prod._6),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(-\/(x))))) => {
               val (h, t) = U(prod._5)
               go((prod._1, prod._2, prod._3, prod._4, t, prod._6),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(x))))) => {
               val (h, t) = U(prod._6)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, t),
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

object AndXorK6 {

  def apply[F[_], A1, A2, A3, A4, A5, A6]: AndXorK6[F, A1, A2, A3, A4, A5, A6] =
    new AndXorK6[F, A1, A2, A3, A4, A5, A6] {}
}

trait AndXorF6[A1, A2, A3, A4, A5, A6] {
  type Repr[F[_]] = AndXorK6[F, A1, A2, A3, A4, A5, A6]
  def apply[F[_]]: Repr[F] =
    new AndXorK6[F, A1, A2, A3, A4, A5, A6] {}
}

object AndXorF6 {
  def apply[A1, A2, A3, A4, A5, A6]: AndXorF6[A1, A2, A3, A4, A5, A6] =
    new AndXorF6[A1, A2, A3, A4, A5, A6] {}
}

trait AndXor6[A1, A2, A3, A4, A5, A6] extends AndXorK6[Id, A1, A2, A3, A4, A5, A6]

object AndXor6 {
  def apply[A1, A2, A3, A4, A5, A6]: AndXor6[A1, A2, A3, A4, A5, A6] =
    new AndXor6[A1, A2, A3, A4, A5, A6] {}

}
