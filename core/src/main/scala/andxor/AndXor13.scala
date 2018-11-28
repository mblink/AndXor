package andxor
import andxor.MapN.syntax._
import scala.language.higherKinds
import scalaz.{Apply, Foldable, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id
import scalaz.Isomorphism.{<=>, IsoSet}
import scalaz.std.list._

trait AndXorK13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] extends AndXor {
  type Prod = (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13])
  type Cop = (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ F[A13]))))))))))))
  val AndXorF = AndXorF13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]
  type AndXor[G[_]] = AndXorF13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Repr[G]
  def combine[G[_]](
      implicit a0: G[F[A1]],
      a1: G[F[A2]],
      a2: G[F[A3]],
      a3: G[F[A4]],
      a4: G[F[A5]],
      a5: G[F[A6]],
      a6: G[F[A7]],
      a7: G[F[A8]],
      a8: G[F[A9]],
      a9: G[F[A10]],
      a10: G[F[A11]],
      a11: G[F[A12]],
      a12: G[F[A13]]
  ): ComposeAndXor[G, Cop, Prod] =
    new ComposeAndXor[G, Cop, Prod] {
      def mkChoose[B](f: B => Cop)(implicit d: Decidable[G]): G[B] =
        Combine.choose13(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(f)
      def mkAlt[B](f: Cop => B)(implicit a: Alt[G]): G[B] =
        Combine.altly13(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(f)
      def mkDivide[B](f: B => Prod)(implicit d: Divide[G]): G[B] =
        Combine.divide13(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(f)
      def mkApply[B](f: Prod => B)(implicit a: Apply[G]): G[B] =
        Combine.apply13(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12) => f((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12)))
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
      Inj.instance(x => \/-(\/-(\/-(-\/(x)))))

    implicit val inja3Inverse: Inj[Option[F[A4]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _                     => None
      })

    implicit val inja4: Inj[Cop, F[A5]] =
      Inj.instance(x => \/-(\/-(\/-(\/-(-\/(x))))))

    implicit val inja4Inverse: Inj[Option[F[A5]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _                          => None
      })

    implicit val inja5: Inj[Cop, F[A6]] =
      Inj.instance(x => \/-(\/-(\/-(\/-(\/-(-\/(x)))))))

    implicit val inja5Inverse: Inj[Option[F[A6]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _                               => None
      })

    implicit val inja6: Inj[Cop, F[A7]] =
      Inj.instance(x => \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))

    implicit val inja6Inverse: Inj[Option[F[A7]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _                                    => None
      })

    implicit val inja7: Inj[Cop, F[A8]] =
      Inj.instance(x => \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))

    implicit val inja7Inverse: Inj[Option[F[A8]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _                                         => None
      })

    implicit val inja8: Inj[Cop, F[A9]] =
      Inj.instance(x => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))

    implicit val inja8Inverse: Inj[Option[F[A9]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _                                              => None
      })

    implicit val inja9: Inj[Cop, F[A10]] =
      Inj.instance(x => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))

    implicit val inja9Inverse: Inj[Option[F[A10]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
        case _                                                   => None
      })

    implicit val inja10: Inj[Cop, F[A11]] =
      Inj.instance(x => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))

    implicit val inja10Inverse: Inj[Option[F[A11]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
        case _                                                        => None
      })

    implicit val inja11: Inj[Cop, F[A12]] =
      Inj.instance(x => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))

    implicit val inja11Inverse: Inj[Option[F[A12]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
        case _                                                             => None
      })

    implicit val inja12: Inj[Cop, F[A13]] =
      Inj.instance(x => \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))

    implicit val inja12Inverse: Inj[Option[F[A13]], Cop] =
      Inj.instance(_ match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))) => Some(x)
        case _                                                             => None
      })

    implicit def liftisoa0(implicit M: Monoid[Prod]): Prod <=> F[A1] =
      IsoSet(_._1, x => M.zero.map1(_ => x))

    implicit def liftisoa1(implicit M: Monoid[Prod]): Prod <=> F[A2] =
      IsoSet(_._2, x => M.zero.map2(_ => x))

    implicit def liftisoa2(implicit M: Monoid[Prod]): Prod <=> F[A3] =
      IsoSet(_._3, x => M.zero.map3(_ => x))

    implicit def liftisoa3(implicit M: Monoid[Prod]): Prod <=> F[A4] =
      IsoSet(_._4, x => M.zero.map4(_ => x))

    implicit def liftisoa4(implicit M: Monoid[Prod]): Prod <=> F[A5] =
      IsoSet(_._5, x => M.zero.map5(_ => x))

    implicit def liftisoa5(implicit M: Monoid[Prod]): Prod <=> F[A6] =
      IsoSet(_._6, x => M.zero.map6(_ => x))

    implicit def liftisoa6(implicit M: Monoid[Prod]): Prod <=> F[A7] =
      IsoSet(_._7, x => M.zero.map7(_ => x))

    implicit def liftisoa7(implicit M: Monoid[Prod]): Prod <=> F[A8] =
      IsoSet(_._8, x => M.zero.map8(_ => x))

    implicit def liftisoa8(implicit M: Monoid[Prod]): Prod <=> F[A9] =
      IsoSet(_._9, x => M.zero.map9(_ => x))

    implicit def liftisoa9(implicit M: Monoid[Prod]): Prod <=> F[A10] =
      IsoSet(_._10, x => M.zero.map10(_ => x))

    implicit def liftisoa10(implicit M: Monoid[Prod]): Prod <=> F[A11] =
      IsoSet(_._11, x => M.zero.map11(_ => x))

    implicit def liftisoa11(implicit M: Monoid[Prod]): Prod <=> F[A12] =
      IsoSet(_._12, x => M.zero.map12(_ => x))

    implicit def liftisoa12(implicit M: Monoid[Prod]): Prod <=> F[A13] =
      IsoSet(_._13, x => M.zero.map13(_ => x))

  }

  import instances._

  val injEv = combine[Inj.Aux[Cop]#Out].choose
  def liftEv(implicit M: Monoid[Prod]): Inj[Prod, Prod] = combine[Inj.Aux[Prod]#Out].divide

  def transformP[G[_]](nt: (F ~> G)): AndXorK13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Prod => AndXorK13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Prod =
    (p: AndXorK13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Prod) =>
      (nt(p._1), nt(p._2), nt(p._3), nt(p._4), nt(p._5), nt(p._6), nt(p._7), nt(p._8), nt(p._9), nt(p._10), nt(p._11), nt(p._12), nt(p._13))

  def transformC[G[_]](nt: (F ~> G)): AndXorK13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Cop => AndXorK13[G, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Cop =
    (p: AndXorK13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Cop) =>
      p.bimap(
        nt(_),
        _.bimap(
          nt(_),
          _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), _.bimap(nt(_), nt(_)))))))))))
        )
      )

  // format: off
  def sequenceP(prod: Prod)(implicit A: Apply[F]): F[AndXorK13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Prod] = {
    val (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = prod
    A.ap(a12)(
    A.ap(a11)(
    A.ap(a10)(
    A.ap(a9)(
    A.ap(a8)(
    A.ap(a7)(
    A.ap(a6)(
    A.ap(a5)(
    A.ap(a4)(
    A.ap(a3)(
    A.ap(a2)(
    A.ap(a1)(
     A.map(a0)(((i0: A1, i1: A2, i2: A3, i3: A4, i4: A5, i5: A6, i6: A7, i7: A8, i8: A9, i9: A10, i10: A11, i11: A12, i12: A13) =>
    (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12)).curried)))))))))))))
  }
  

  def extractC[B](c: Cop)(implicit inj: Inj[Option[B], Cop]): Option[B] = inj(c)

  def extractP[B](p: Prod)(implicit inj: Inj[B, Prod]): B = inj(p)

  def toListP(p: Prod): List[Cop] = combine[Inj.Aux[List[Cop]]#Out].divide.apply(p)

  def foldMap[G[_], C](p: AndXor[G]#Prod)(
    map: AndXor[Id]#Cop => C)(
    implicit O: Ordering[AndXorK13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]#Cop], M: Monoid[C],
    PE: PlusEmpty[G], U: Uncons[G]): C = {
    val TG = AndXorF[G]
    val TI = AndXorF[Id]
    import scala.collection.mutable.{PriorityQueue => PQ}
    import TI.instances._
    def uncons(p: TG.Prod): (List[TI.Cop], TG.Prod) = {
     val hts = (U(p._1), U(p._2), U(p._3), U(p._4), U(p._5), U(p._6), U(p._7), U(p._8), U(p._9), U(p._10), U(p._11), U(p._12), U(p._13))
     (List(hts._1._1.map(TI.inj(_: A1)), hts._2._1.map(TI.inj(_: A2)), hts._3._1.map(TI.inj(_: A3)), hts._4._1.map(TI.inj(_: A4)), hts._5._1.map(TI.inj(_: A5)), hts._6._1.map(TI.inj(_: A6)), hts._7._1.map(TI.inj(_: A7)), hts._8._1.map(TI.inj(_: A8)), hts._9._1.map(TI.inj(_: A9)), hts._10._1.map(TI.inj(_: A10)), hts._11._1.map(TI.inj(_: A11)), hts._12._1.map(TI.inj(_: A12)), hts._13._1.map(TI.inj(_: A13))).flatten,
      (hts._1._2, hts._2._2, hts._3._2, hts._4._2, hts._5._2, hts._6._2, hts._7._2, hts._8._2, hts._9._2, hts._10._2, hts._11._2, hts._12._2, hts._13._2))
    }
    @scala.annotation.tailrec
    def go(prod: TG.Prod, q: PQ[TI.Cop], out: C): C =
     (prod.==((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8], PE.empty[A9], PE.empty[A10], PE.empty[A11], PE.empty[A12], PE.empty[A13]))) match {
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
               go((t, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(-\/(x)) => {
               val (h, t) = U(prod._2)
               go((prod._1, t, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(-\/(x))) => {
               val (h, t) = U(prod._3)
               go((prod._1, prod._2, t, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(-\/(x)))) => {
               val (h, t) = U(prod._4)
               go((prod._1, prod._2, prod._3, t, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(-\/(x))))) => {
               val (h, t) = U(prod._5)
               go((prod._1, prod._2, prod._3, prod._4, t, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => {
               val (h, t) = U(prod._6)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, t, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => {
               val (h, t) = U(prod._7)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, t, prod._8, prod._9, prod._10, prod._11, prod._12, prod._13),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => {
               val (h, t) = U(prod._8)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, t, prod._9, prod._10, prod._11, prod._12, prod._13),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => {
               val (h, t) = U(prod._9)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, t, prod._10, prod._11, prod._12, prod._13),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => {
               val (h, t) = U(prod._10)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, t, prod._11, prod._12, prod._13),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => {
               val (h, t) = U(prod._11)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, t, prod._12, prod._13),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => {
               val (h, t) = U(prod._12)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, t, prod._13),
                 q ++= h.map(TI.inj(_)), M.append(out, map(TI.inj(x))))
             }
             case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))) => {
               val (h, t) = U(prod._13)
               go((prod._1, prod._2, prod._3, prod._4, prod._5, prod._6, prod._7, prod._8, prod._9, prod._10, prod._11, prod._12, t),
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

object AndXorK13 {

  def apply[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: AndXorK13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
    new AndXorK13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}
}

trait AndXorF13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {
  type Repr[F[_]] = AndXorK13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]
  def apply[F[_]]: Repr[F] =
    new AndXorK13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}
}

object AndXorF13 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: AndXorF13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
    new AndXorF13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}
}

trait AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] extends AndXorK13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

object AndXor13 {
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
    new AndXor13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}

}
