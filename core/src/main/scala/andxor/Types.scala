package andxor


import io.estatico.newtype.macros.newtype
import scalaz.{\/, -\/, \/-, InvariantFunctor, Monoid}

object types {
  private val MF = InvariantFunctor[Monoid]
  
  @newtype case class Prod1[F[_], A1](run: (F[A1]))
  object Prod1 {
    implicit def Prod1Monoid[F[_], A1](implicit M: Monoid[(F[A1])]): Monoid[Prod1[F, A1]] =
      MF.xmap(M, Prod1[F, A1](_), (_: Prod1[F, A1]).run)

    implicit def lifta0[F[_], A1]: Inj[Prod1[F, A1], F[A1]] = {
      
      Inj.instance(x => Prod1[F, A1]((x)))
    }

    implicit def lifta0Inverse[F[_], A1]: Inj[F[A1], Prod1[F, A1]] = Inj.instance(_.run)
  }

  @newtype case class Cop1[F[_], A1](run: F[A1])
  object Cop1 {
    implicit def prisma0[F[_], A1]: Prism[Cop1[F, A1], F[A1]] = new Prism[Cop1[F, A1], F[A1]] {
      def getOption(c: Cop1[F, A1]): Option[F[A1]] = Some(c.run)
      def reverseGet(x: F[A1]): Cop1[F, A1] = Cop1[F, A1](x)
    }

    implicit def inja0[F[_], A1]: Inj[Cop1[F, A1], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1]: Inj[Option[F[A1]], Cop1[F, A1]] = Inj.instance(prisma0.getOption(_))
  }
  
  @newtype case class Prod2[F[_], A1, A2](run: (F[A1], F[A2]))
  object Prod2 {
    implicit def Prod2Monoid[F[_], A1, A2](implicit M: Monoid[(F[A1], F[A2])]): Monoid[Prod2[F, A1, A2]] =
      MF.xmap(M, Prod2[F, A1, A2](_), (_: Prod2[F, A1, A2]).run)

    implicit def lifta0[F[_], A1, A2](implicit M: Monoid[Prod2[F, A1, A2]]): Inj[Prod2[F, A1, A2], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod2[F, A1, A2]((x, t._2)))
    }

    implicit def lifta0Inverse[F[_], A1, A2]: Inj[F[A1], Prod2[F, A1, A2]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2](implicit M: Monoid[Prod2[F, A1, A2]]): Inj[Prod2[F, A1, A2], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod2[F, A1, A2]((t._1, x)))
    }

    implicit def lifta1Inverse[F[_], A1, A2]: Inj[F[A2], Prod2[F, A1, A2]] = Inj.instance(_.run._2)
  }

  @newtype case class Cop2[F[_], A1, A2](run: (F[A1] \/ F[A2]))
  object Cop2 {
    implicit def prisma0[F[_], A1, A2]: Prism[Cop2[F, A1, A2], F[A1]] = new Prism[Cop2[F, A1, A2], F[A1]] {
      def getOption(c: Cop2[F, A1, A2]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop2[F, A1, A2] = Cop2[F, A1, A2](-\/(x))
    }

    implicit def inja0[F[_], A1, A2]: Inj[Cop2[F, A1, A2], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2]: Inj[Option[F[A1]], Cop2[F, A1, A2]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2]: Prism[Cop2[F, A1, A2], F[A2]] = new Prism[Cop2[F, A1, A2], F[A2]] {
      def getOption(c: Cop2[F, A1, A2]): Option[F[A2]] = c.run match {
        case \/-(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop2[F, A1, A2] = Cop2[F, A1, A2](\/-(x))
    }

    implicit def inja1[F[_], A1, A2]: Inj[Cop2[F, A1, A2], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2]: Inj[Option[F[A2]], Cop2[F, A1, A2]] = Inj.instance(prisma1.getOption(_))
  }
  
  @newtype case class Prod3[F[_], A1, A2, A3](run: (F[A1], F[A2], F[A3]))
  object Prod3 {
    implicit def Prod3Monoid[F[_], A1, A2, A3](implicit M: Monoid[(F[A1], F[A2], F[A3])]): Monoid[Prod3[F, A1, A2, A3]] =
      MF.xmap(M, Prod3[F, A1, A2, A3](_), (_: Prod3[F, A1, A2, A3]).run)

    implicit def lifta0[F[_], A1, A2, A3](implicit M: Monoid[Prod3[F, A1, A2, A3]]): Inj[Prod3[F, A1, A2, A3], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod3[F, A1, A2, A3]((x, t._2, t._3)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3]: Inj[F[A1], Prod3[F, A1, A2, A3]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3](implicit M: Monoid[Prod3[F, A1, A2, A3]]): Inj[Prod3[F, A1, A2, A3], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod3[F, A1, A2, A3]((t._1, x, t._3)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3]: Inj[F[A2], Prod3[F, A1, A2, A3]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3](implicit M: Monoid[Prod3[F, A1, A2, A3]]): Inj[Prod3[F, A1, A2, A3], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod3[F, A1, A2, A3]((t._1, t._2, x)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3]: Inj[F[A3], Prod3[F, A1, A2, A3]] = Inj.instance(_.run._3)
  }

  @newtype case class Cop3[F[_], A1, A2, A3](run: (F[A1] \/ (F[A2] \/ F[A3])))
  object Cop3 {
    implicit def prisma0[F[_], A1, A2, A3]: Prism[Cop3[F, A1, A2, A3], F[A1]] = new Prism[Cop3[F, A1, A2, A3], F[A1]] {
      def getOption(c: Cop3[F, A1, A2, A3]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop3[F, A1, A2, A3] = Cop3[F, A1, A2, A3](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3]: Inj[Cop3[F, A1, A2, A3], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3]: Inj[Option[F[A1]], Cop3[F, A1, A2, A3]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3]: Prism[Cop3[F, A1, A2, A3], F[A2]] = new Prism[Cop3[F, A1, A2, A3], F[A2]] {
      def getOption(c: Cop3[F, A1, A2, A3]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop3[F, A1, A2, A3] = Cop3[F, A1, A2, A3](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3]: Inj[Cop3[F, A1, A2, A3], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3]: Inj[Option[F[A2]], Cop3[F, A1, A2, A3]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3]: Prism[Cop3[F, A1, A2, A3], F[A3]] = new Prism[Cop3[F, A1, A2, A3], F[A3]] {
      def getOption(c: Cop3[F, A1, A2, A3]): Option[F[A3]] = c.run match {
        case \/-(\/-(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop3[F, A1, A2, A3] = Cop3[F, A1, A2, A3](\/-(\/-(x)))
    }

    implicit def inja2[F[_], A1, A2, A3]: Inj[Cop3[F, A1, A2, A3], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3]: Inj[Option[F[A3]], Cop3[F, A1, A2, A3]] = Inj.instance(prisma2.getOption(_))
  }
  
  @newtype case class Prod4[F[_], A1, A2, A3, A4](run: (F[A1], F[A2], F[A3], F[A4]))
  object Prod4 {
    implicit def Prod4Monoid[F[_], A1, A2, A3, A4](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4])]): Monoid[Prod4[F, A1, A2, A3, A4]] =
      MF.xmap(M, Prod4[F, A1, A2, A3, A4](_), (_: Prod4[F, A1, A2, A3, A4]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((x, t._2, t._3, t._4)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4]: Inj[F[A1], Prod4[F, A1, A2, A3, A4]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((t._1, x, t._3, t._4)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4]: Inj[F[A2], Prod4[F, A1, A2, A3, A4]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((t._1, t._2, x, t._4)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4]: Inj[F[A3], Prod4[F, A1, A2, A3, A4]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((t._1, t._2, t._3, x)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4]: Inj[F[A4], Prod4[F, A1, A2, A3, A4]] = Inj.instance(_.run._4)
  }

  @newtype case class Cop4[F[_], A1, A2, A3, A4](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ F[A4]))))
  object Cop4 {
    implicit def prisma0[F[_], A1, A2, A3, A4]: Prism[Cop4[F, A1, A2, A3, A4], F[A1]] = new Prism[Cop4[F, A1, A2, A3, A4], F[A1]] {
      def getOption(c: Cop4[F, A1, A2, A3, A4]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop4[F, A1, A2, A3, A4] = Cop4[F, A1, A2, A3, A4](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4]: Inj[Option[F[A1]], Cop4[F, A1, A2, A3, A4]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4]: Prism[Cop4[F, A1, A2, A3, A4], F[A2]] = new Prism[Cop4[F, A1, A2, A3, A4], F[A2]] {
      def getOption(c: Cop4[F, A1, A2, A3, A4]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop4[F, A1, A2, A3, A4] = Cop4[F, A1, A2, A3, A4](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4]: Inj[Option[F[A2]], Cop4[F, A1, A2, A3, A4]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4]: Prism[Cop4[F, A1, A2, A3, A4], F[A3]] = new Prism[Cop4[F, A1, A2, A3, A4], F[A3]] {
      def getOption(c: Cop4[F, A1, A2, A3, A4]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop4[F, A1, A2, A3, A4] = Cop4[F, A1, A2, A3, A4](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4]: Inj[Option[F[A3]], Cop4[F, A1, A2, A3, A4]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4]: Prism[Cop4[F, A1, A2, A3, A4], F[A4]] = new Prism[Cop4[F, A1, A2, A3, A4], F[A4]] {
      def getOption(c: Cop4[F, A1, A2, A3, A4]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop4[F, A1, A2, A3, A4] = Cop4[F, A1, A2, A3, A4](\/-(\/-(\/-(x))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4]: Inj[Option[F[A4]], Cop4[F, A1, A2, A3, A4]] = Inj.instance(prisma3.getOption(_))
  }
  
  @newtype case class Prod5[F[_], A1, A2, A3, A4, A5](run: (F[A1], F[A2], F[A3], F[A4], F[A5]))
  object Prod5 {
    implicit def Prod5Monoid[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5])]): Monoid[Prod5[F, A1, A2, A3, A4, A5]] =
      MF.xmap(M, Prod5[F, A1, A2, A3, A4, A5](_), (_: Prod5[F, A1, A2, A3, A4, A5]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[F, A1, A2, A3, A4, A5]]): Inj[Prod5[F, A1, A2, A3, A4, A5], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod5[F, A1, A2, A3, A4, A5]((x, t._2, t._3, t._4, t._5)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5]: Inj[F[A1], Prod5[F, A1, A2, A3, A4, A5]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[F, A1, A2, A3, A4, A5]]): Inj[Prod5[F, A1, A2, A3, A4, A5], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod5[F, A1, A2, A3, A4, A5]((t._1, x, t._3, t._4, t._5)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5]: Inj[F[A2], Prod5[F, A1, A2, A3, A4, A5]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[F, A1, A2, A3, A4, A5]]): Inj[Prod5[F, A1, A2, A3, A4, A5], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod5[F, A1, A2, A3, A4, A5]((t._1, t._2, x, t._4, t._5)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5]: Inj[F[A3], Prod5[F, A1, A2, A3, A4, A5]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[F, A1, A2, A3, A4, A5]]): Inj[Prod5[F, A1, A2, A3, A4, A5], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod5[F, A1, A2, A3, A4, A5]((t._1, t._2, t._3, x, t._5)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5]: Inj[F[A4], Prod5[F, A1, A2, A3, A4, A5]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[F, A1, A2, A3, A4, A5]]): Inj[Prod5[F, A1, A2, A3, A4, A5], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod5[F, A1, A2, A3, A4, A5]((t._1, t._2, t._3, t._4, x)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5]: Inj[F[A5], Prod5[F, A1, A2, A3, A4, A5]] = Inj.instance(_.run._5)
  }

  @newtype case class Cop5[F[_], A1, A2, A3, A4, A5](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ F[A5])))))
  object Cop5 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5]: Prism[Cop5[F, A1, A2, A3, A4, A5], F[A1]] = new Prism[Cop5[F, A1, A2, A3, A4, A5], F[A1]] {
      def getOption(c: Cop5[F, A1, A2, A3, A4, A5]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop5[F, A1, A2, A3, A4, A5] = Cop5[F, A1, A2, A3, A4, A5](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5]: Inj[Cop5[F, A1, A2, A3, A4, A5], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5]: Inj[Option[F[A1]], Cop5[F, A1, A2, A3, A4, A5]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5]: Prism[Cop5[F, A1, A2, A3, A4, A5], F[A2]] = new Prism[Cop5[F, A1, A2, A3, A4, A5], F[A2]] {
      def getOption(c: Cop5[F, A1, A2, A3, A4, A5]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop5[F, A1, A2, A3, A4, A5] = Cop5[F, A1, A2, A3, A4, A5](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5]: Inj[Cop5[F, A1, A2, A3, A4, A5], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5]: Inj[Option[F[A2]], Cop5[F, A1, A2, A3, A4, A5]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5]: Prism[Cop5[F, A1, A2, A3, A4, A5], F[A3]] = new Prism[Cop5[F, A1, A2, A3, A4, A5], F[A3]] {
      def getOption(c: Cop5[F, A1, A2, A3, A4, A5]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop5[F, A1, A2, A3, A4, A5] = Cop5[F, A1, A2, A3, A4, A5](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5]: Inj[Cop5[F, A1, A2, A3, A4, A5], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5]: Inj[Option[F[A3]], Cop5[F, A1, A2, A3, A4, A5]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5]: Prism[Cop5[F, A1, A2, A3, A4, A5], F[A4]] = new Prism[Cop5[F, A1, A2, A3, A4, A5], F[A4]] {
      def getOption(c: Cop5[F, A1, A2, A3, A4, A5]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop5[F, A1, A2, A3, A4, A5] = Cop5[F, A1, A2, A3, A4, A5](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5]: Inj[Cop5[F, A1, A2, A3, A4, A5], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5]: Inj[Option[F[A4]], Cop5[F, A1, A2, A3, A4, A5]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5]: Prism[Cop5[F, A1, A2, A3, A4, A5], F[A5]] = new Prism[Cop5[F, A1, A2, A3, A4, A5], F[A5]] {
      def getOption(c: Cop5[F, A1, A2, A3, A4, A5]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop5[F, A1, A2, A3, A4, A5] = Cop5[F, A1, A2, A3, A4, A5](\/-(\/-(\/-(\/-(x)))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5]: Inj[Cop5[F, A1, A2, A3, A4, A5], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5]: Inj[Option[F[A5]], Cop5[F, A1, A2, A3, A4, A5]] = Inj.instance(prisma4.getOption(_))
  }
  
  @newtype case class Prod6[F[_], A1, A2, A3, A4, A5, A6](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6]))
  object Prod6 {
    implicit def Prod6Monoid[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6])]): Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]] =
      MF.xmap(M, Prod6[F, A1, A2, A3, A4, A5, A6](_), (_: Prod6[F, A1, A2, A3, A4, A5, A6]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((x, t._2, t._3, t._4, t._5, t._6)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[F[A1], Prod6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((t._1, x, t._3, t._4, t._5, t._6)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[F[A2], Prod6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((t._1, t._2, x, t._4, t._5, t._6)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[F[A3], Prod6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((t._1, t._2, t._3, x, t._5, t._6)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[F[A4], Prod6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((t._1, t._2, t._3, t._4, x, t._6)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[F[A5], Prod6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((t._1, t._2, t._3, t._4, t._5, x)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[F[A6], Prod6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(_.run._6)
  }

  @newtype case class Cop6[F[_], A1, A2, A3, A4, A5, A6](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ F[A6]))))))
  object Cop6 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6]: Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A1]] = new Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A1]] {
      def getOption(c: Cop6[F, A1, A2, A3, A4, A5, A6]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop6[F, A1, A2, A3, A4, A5, A6] = Cop6[F, A1, A2, A3, A4, A5, A6](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[F[A1]], Cop6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6]: Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A2]] = new Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A2]] {
      def getOption(c: Cop6[F, A1, A2, A3, A4, A5, A6]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop6[F, A1, A2, A3, A4, A5, A6] = Cop6[F, A1, A2, A3, A4, A5, A6](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[F[A2]], Cop6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6]: Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A3]] = new Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A3]] {
      def getOption(c: Cop6[F, A1, A2, A3, A4, A5, A6]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop6[F, A1, A2, A3, A4, A5, A6] = Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[F[A3]], Cop6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6]: Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A4]] = new Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A4]] {
      def getOption(c: Cop6[F, A1, A2, A3, A4, A5, A6]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop6[F, A1, A2, A3, A4, A5, A6] = Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[F[A4]], Cop6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6]: Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A5]] = new Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A5]] {
      def getOption(c: Cop6[F, A1, A2, A3, A4, A5, A6]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop6[F, A1, A2, A3, A4, A5, A6] = Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[F[A5]], Cop6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6]: Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A6]] = new Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A6]] {
      def getOption(c: Cop6[F, A1, A2, A3, A4, A5, A6]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop6[F, A1, A2, A3, A4, A5, A6] = Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(\/-(\/-(x))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[F[A6]], Cop6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(prisma5.getOption(_))
  }
  
  @newtype case class Prod7[F[_], A1, A2, A3, A4, A5, A6, A7](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7]))
  object Prod7 {
    implicit def Prod7Monoid[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7])]): Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]] =
      MF.xmap(M, Prod7[F, A1, A2, A3, A4, A5, A6, A7](_), (_: Prod7[F, A1, A2, A3, A4, A5, A6, A7]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((x, t._2, t._3, t._4, t._5, t._6, t._7)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[F[A1], Prod7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t._1, x, t._3, t._4, t._5, t._6, t._7)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[F[A2], Prod7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t._1, t._2, x, t._4, t._5, t._6, t._7)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[F[A3], Prod7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t._1, t._2, t._3, x, t._5, t._6, t._7)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[F[A4], Prod7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t._1, t._2, t._3, t._4, x, t._6, t._7)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[F[A5], Prod7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t._1, t._2, t._3, t._4, t._5, x, t._7)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[F[A6], Prod7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t._1, t._2, t._3, t._4, t._5, t._6, x)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[F[A7], Prod7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(_.run._7)
  }

  @newtype case class Cop7[F[_], A1, A2, A3, A4, A5, A6, A7](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ F[A7])))))))
  object Cop7 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A1]] = new Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A1]] {
      def getOption(c: Cop7[F, A1, A2, A3, A4, A5, A6, A7]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop7[F, A1, A2, A3, A4, A5, A6, A7] = Cop7[F, A1, A2, A3, A4, A5, A6, A7](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[F[A1]], Cop7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A2]] = new Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A2]] {
      def getOption(c: Cop7[F, A1, A2, A3, A4, A5, A6, A7]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop7[F, A1, A2, A3, A4, A5, A6, A7] = Cop7[F, A1, A2, A3, A4, A5, A6, A7](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[F[A2]], Cop7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A3]] = new Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A3]] {
      def getOption(c: Cop7[F, A1, A2, A3, A4, A5, A6, A7]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop7[F, A1, A2, A3, A4, A5, A6, A7] = Cop7[F, A1, A2, A3, A4, A5, A6, A7](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[F[A3]], Cop7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A4]] = new Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A4]] {
      def getOption(c: Cop7[F, A1, A2, A3, A4, A5, A6, A7]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop7[F, A1, A2, A3, A4, A5, A6, A7] = Cop7[F, A1, A2, A3, A4, A5, A6, A7](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[F[A4]], Cop7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A5]] = new Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A5]] {
      def getOption(c: Cop7[F, A1, A2, A3, A4, A5, A6, A7]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop7[F, A1, A2, A3, A4, A5, A6, A7] = Cop7[F, A1, A2, A3, A4, A5, A6, A7](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[F[A5]], Cop7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A6]] = new Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A6]] {
      def getOption(c: Cop7[F, A1, A2, A3, A4, A5, A6, A7]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop7[F, A1, A2, A3, A4, A5, A6, A7] = Cop7[F, A1, A2, A3, A4, A5, A6, A7](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[F[A6]], Cop7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A7]] = new Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A7]] {
      def getOption(c: Cop7[F, A1, A2, A3, A4, A5, A6, A7]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop7[F, A1, A2, A3, A4, A5, A6, A7] = Cop7[F, A1, A2, A3, A4, A5, A6, A7](\/-(\/-(\/-(\/-(\/-(\/-(x)))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[F[A7]], Cop7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(prisma6.getOption(_))
  }
  
  @newtype case class Prod8[F[_], A1, A2, A3, A4, A5, A6, A7, A8](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8]))
  object Prod8 {
    implicit def Prod8Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8])]): Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] =
      MF.xmap(M, Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8](_), (_: Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A1], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A2], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A3], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A4], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A5], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A6], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A7], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._7)

    implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x)))
    }

    implicit def lifta7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A8], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._8)
  }

  @newtype case class Cop8[F[_], A1, A2, A3, A4, A5, A6, A7, A8](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ F[A8]))))))))
  object Cop8 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A1]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A1]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A1]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A2]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A2]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A2]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A3]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A3]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A3]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A4]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A4]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A4]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A5]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A5]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A5]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A6]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A6]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A6]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A7]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A7]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A7]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A8]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A8]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A8]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))
    }

    implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A8]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma7.getOption(_))
  }
  
  @newtype case class Prod9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9]))
  object Prod9 {
    implicit def Prod9Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9])]): Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
      MF.xmap(M, Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](_), (_: Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A1], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A2], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A3], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A4], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A5], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A6], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A7], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._7)

    implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9)))
    }

    implicit def lifta7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A8], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._8)

    implicit def lifta8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x)))
    }

    implicit def lifta8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A9], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._9)
  }

  @newtype case class Cop9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ F[A9])))))))))
  object Cop9 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A1]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A2]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A3]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A4]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A5]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A6]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A7]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A8]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A8]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma7.getOption(_))

    implicit def prisma8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A9]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))
    }

    implicit def inja8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] = Inj.instance(prisma8.reverseGet(_))
    implicit def inja8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A9]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma8.getOption(_))
  }
  
  @newtype case class Prod10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10]))
  object Prod10 {
    implicit def Prod10Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10])]): Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
      MF.xmap(M, Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](_), (_: Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A1], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A2], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A3], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A4], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A5], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A6], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A7], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._7)

    implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10)))
    }

    implicit def lifta7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A8], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._8)

    implicit def lifta8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10)))
    }

    implicit def lifta8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A9], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._9)

    implicit def lifta9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x)))
    }

    implicit def lifta9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A10], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._10)
  }

  @newtype case class Cop10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ F[A10]))))))))))
  object Cop10 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A1]] = new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A1]] {
      def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A1]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A2]] = new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A2]] {
      def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A2]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A3]] = new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A3]] {
      def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A3]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A4]] = new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A4]] {
      def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A4]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A5]] = new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A5]] {
      def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A5]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A6]] = new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A6]] {
      def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A6]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A7]] = new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A7]] {
      def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A7]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A8]] = new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A8]] {
      def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A8]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A8]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma7.getOption(_))

    implicit def prisma8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A9]] = new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A9]] {
      def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A9]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
    }

    implicit def inja8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A9]] = Inj.instance(prisma8.reverseGet(_))
    implicit def inja8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A9]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma8.getOption(_))

    implicit def prisma9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A10]] = new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A10]] {
      def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A10]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A10]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))
    }

    implicit def inja9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A10]] = Inj.instance(prisma9.reverseGet(_))
    implicit def inja9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A10]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma9.getOption(_))
  }
  
  @newtype case class Prod11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]))
  object Prod11 {
    implicit def Prod11Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11])]): Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      MF.xmap(M, Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](_), (_: Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A1], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A2], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A3], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A4], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A5], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A6], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A7], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._7)

    implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11)))
    }

    implicit def lifta7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A8], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._8)

    implicit def lifta8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11)))
    }

    implicit def lifta8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A9], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._9)

    implicit def lifta9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11)))
    }

    implicit def lifta9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A10], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._10)

    implicit def lifta10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x)))
    }

    implicit def lifta10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A11], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._11)
  }

  @newtype case class Cop11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ F[A11])))))))))))
  object Cop11 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A1]] = new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A1]] {
      def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A1]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A2]] = new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A2]] {
      def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A2]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A3]] = new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A3]] {
      def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A3]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A4]] = new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A4]] {
      def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A4]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A5]] = new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A5]] {
      def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A5]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A6]] = new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A6]] {
      def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A6]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A7]] = new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A7]] {
      def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A7]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A8]] = new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A8]] {
      def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A8]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A8]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma7.getOption(_))

    implicit def prisma8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A9]] = new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A9]] {
      def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A9]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
    }

    implicit def inja8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A9]] = Inj.instance(prisma8.reverseGet(_))
    implicit def inja8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A9]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma8.getOption(_))

    implicit def prisma9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A10]] = new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A10]] {
      def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A10]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A10]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
    }

    implicit def inja9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A10]] = Inj.instance(prisma9.reverseGet(_))
    implicit def inja9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A10]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma9.getOption(_))

    implicit def prisma10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A11]] = new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A11]] {
      def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A11]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A11]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))
    }

    implicit def inja10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A11]] = Inj.instance(prisma10.reverseGet(_))
    implicit def inja10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A11]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma10.getOption(_))
  }
  
  @newtype case class Prod12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]))
  object Prod12 {
    implicit def Prod12Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12])]): Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      MF.xmap(M, Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](_), (_: Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A1], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A2], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A3], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A4], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A5], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A6], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A7], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._7)

    implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A8], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._8)

    implicit def lifta8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12)))
    }

    implicit def lifta8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A9], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._9)

    implicit def lifta9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12)))
    }

    implicit def lifta9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A10], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._10)

    implicit def lifta10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12)))
    }

    implicit def lifta10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A11], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._11)

    implicit def lifta11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A12]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x)))
    }

    implicit def lifta11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A12], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._12)
  }

  @newtype case class Cop12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ F[A12]))))))))))))
  object Cop12 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A1]] = new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A1]] {
      def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A1]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A2]] = new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A2]] {
      def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A2]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A3]] = new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A3]] {
      def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A3]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A4]] = new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A4]] {
      def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A4]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A5]] = new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A5]] {
      def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A5]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A6]] = new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A6]] {
      def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A6]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A7]] = new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A7]] {
      def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A7]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A8]] = new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A8]] {
      def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A8]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A8]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(prisma7.getOption(_))

    implicit def prisma8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A9]] = new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A9]] {
      def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A9]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
    }

    implicit def inja8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A9]] = Inj.instance(prisma8.reverseGet(_))
    implicit def inja8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A9]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(prisma8.getOption(_))

    implicit def prisma9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A10]] = new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A10]] {
      def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A10]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A10]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
    }

    implicit def inja9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A10]] = Inj.instance(prisma9.reverseGet(_))
    implicit def inja9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A10]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(prisma9.getOption(_))

    implicit def prisma10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A11]] = new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A11]] {
      def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A11]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A11]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
    }

    implicit def inja10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A11]] = Inj.instance(prisma10.reverseGet(_))
    implicit def inja10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A11]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(prisma10.getOption(_))

    implicit def prisma11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A12]] = new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A12]] {
      def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A12]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A12]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))
    }

    implicit def inja11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A12]] = Inj.instance(prisma11.reverseGet(_))
    implicit def inja11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A12]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(prisma11.getOption(_))
  }
  
  @newtype case class Prod13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13]))
  object Prod13 {
    implicit def Prod13Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13])]): Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      MF.xmap(M, Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](_), (_: Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A1], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A2], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A3], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A4], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A5], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A6], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A7], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._7)

    implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A8], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._8)

    implicit def lifta8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A9], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._9)

    implicit def lifta9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13)))
    }

    implicit def lifta9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A10], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._10)

    implicit def lifta10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13)))
    }

    implicit def lifta10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A11], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._11)

    implicit def lifta11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A12]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13)))
    }

    implicit def lifta11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A12], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._12)

    implicit def lifta12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A13]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x)))
    }

    implicit def lifta12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A13], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._13)
  }

  @newtype case class Cop13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ F[A13])))))))))))))
  object Cop13 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A1]] = new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A1]] {
      def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A1]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A2]] = new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A2]] {
      def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A2]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A3]] = new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A3]] {
      def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A3]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A4]] = new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A4]] {
      def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A4]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A5]] = new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A5]] {
      def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A5]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A6]] = new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A6]] {
      def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A6]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A7]] = new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A7]] {
      def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A7]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A8]] = new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A8]] {
      def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A8]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A8]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(prisma7.getOption(_))

    implicit def prisma8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A9]] = new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A9]] {
      def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A9]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
    }

    implicit def inja8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A9]] = Inj.instance(prisma8.reverseGet(_))
    implicit def inja8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A9]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(prisma8.getOption(_))

    implicit def prisma9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A10]] = new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A10]] {
      def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A10]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A10]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
    }

    implicit def inja9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A10]] = Inj.instance(prisma9.reverseGet(_))
    implicit def inja9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A10]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(prisma9.getOption(_))

    implicit def prisma10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A11]] = new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A11]] {
      def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A11]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A11]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
    }

    implicit def inja10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A11]] = Inj.instance(prisma10.reverseGet(_))
    implicit def inja10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A11]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(prisma10.getOption(_))

    implicit def prisma11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A12]] = new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A12]] {
      def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A12]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A12]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
    }

    implicit def inja11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A12]] = Inj.instance(prisma11.reverseGet(_))
    implicit def inja11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A12]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(prisma11.getOption(_))

    implicit def prisma12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A13]] = new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A13]] {
      def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A13]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A13]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))
    }

    implicit def inja12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A13]] = Inj.instance(prisma12.reverseGet(_))
    implicit def inja12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A13]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(prisma12.getOption(_))
  }
  
  @newtype case class Prod14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14]))
  object Prod14 {
    implicit def Prod14Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14])]): Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      MF.xmap(M, Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](_), (_: Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A1], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A2], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A3], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A4], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A5], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A6], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A7], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(_.run._7)

    implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A8], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(_.run._8)

    implicit def lifta8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A9], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(_.run._9)

    implicit def lifta9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A10], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(_.run._10)

    implicit def lifta10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14)))
    }

    implicit def lifta10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A11], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(_.run._11)

    implicit def lifta11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A12]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14)))
    }

    implicit def lifta11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A12], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(_.run._12)

    implicit def lifta12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A13]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14)))
    }

    implicit def lifta12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A13], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(_.run._13)

    implicit def lifta13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A14]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x)))
    }

    implicit def lifta13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A14], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(_.run._14)
  }

  @newtype case class Cop14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14]))))))))))))))
  object Cop14 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A1]] = new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A1]] {
      def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A1]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A2]] = new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A2]] {
      def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A2]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A3]] = new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A3]] {
      def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A3]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A4]] = new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A4]] {
      def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A4]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A5]] = new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A5]] {
      def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A5]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A6]] = new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A6]] {
      def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A6]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A7]] = new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A7]] {
      def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A7]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A8]] = new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A8]] {
      def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A8]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A8]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(prisma7.getOption(_))

    implicit def prisma8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A9]] = new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A9]] {
      def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A9]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
    }

    implicit def inja8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A9]] = Inj.instance(prisma8.reverseGet(_))
    implicit def inja8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A9]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(prisma8.getOption(_))

    implicit def prisma9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A10]] = new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A10]] {
      def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A10]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A10]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
    }

    implicit def inja9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A10]] = Inj.instance(prisma9.reverseGet(_))
    implicit def inja9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A10]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(prisma9.getOption(_))

    implicit def prisma10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A11]] = new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A11]] {
      def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A11]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A11]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
    }

    implicit def inja10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A11]] = Inj.instance(prisma10.reverseGet(_))
    implicit def inja10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A11]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(prisma10.getOption(_))

    implicit def prisma11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A12]] = new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A12]] {
      def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A12]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A12]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
    }

    implicit def inja11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A12]] = Inj.instance(prisma11.reverseGet(_))
    implicit def inja11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A12]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(prisma11.getOption(_))

    implicit def prisma12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A13]] = new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A13]] {
      def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A13]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A13]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
    }

    implicit def inja12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A13]] = Inj.instance(prisma12.reverseGet(_))
    implicit def inja12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A13]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(prisma12.getOption(_))

    implicit def prisma13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A14]] = new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A14]] {
      def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A14]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A14]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))
    }

    implicit def inja13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A14]] = Inj.instance(prisma13.reverseGet(_))
    implicit def inja13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A14]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] = Inj.instance(prisma13.getOption(_))
  }
  
  @newtype case class Prod15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15]))
  object Prod15 {
    implicit def Prod15Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15])]): Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      MF.xmap(M, Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](_), (_: Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A1], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A2], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A3], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A4], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A5], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A6], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A7], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(_.run._7)

    implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A8], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(_.run._8)

    implicit def lifta8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A9], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(_.run._9)

    implicit def lifta9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A10], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(_.run._10)

    implicit def lifta10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A11], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(_.run._11)

    implicit def lifta11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A12]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15)))
    }

    implicit def lifta11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A12], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(_.run._12)

    implicit def lifta12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A13]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15)))
    }

    implicit def lifta12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A13], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(_.run._13)

    implicit def lifta13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A14]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15)))
    }

    implicit def lifta13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A14], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(_.run._14)

    implicit def lifta14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A15]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x)))
    }

    implicit def lifta14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A15], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(_.run._15)
  }

  @newtype case class Cop15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ F[A15])))))))))))))))
  object Cop15 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A1]] = new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A1]] {
      def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A1]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A2]] = new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A2]] {
      def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A2]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A3]] = new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A3]] {
      def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A3]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A4]] = new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A4]] {
      def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A4]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A5]] = new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A5]] {
      def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A5]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A6]] = new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A6]] {
      def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A6]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A7]] = new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A7]] {
      def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A7]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A8]] = new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A8]] {
      def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A8]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A8]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma7.getOption(_))

    implicit def prisma8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A9]] = new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A9]] {
      def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A9]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
    }

    implicit def inja8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A9]] = Inj.instance(prisma8.reverseGet(_))
    implicit def inja8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A9]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma8.getOption(_))

    implicit def prisma9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A10]] = new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A10]] {
      def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A10]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A10]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
    }

    implicit def inja9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A10]] = Inj.instance(prisma9.reverseGet(_))
    implicit def inja9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A10]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma9.getOption(_))

    implicit def prisma10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A11]] = new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A11]] {
      def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A11]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A11]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
    }

    implicit def inja10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A11]] = Inj.instance(prisma10.reverseGet(_))
    implicit def inja10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A11]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma10.getOption(_))

    implicit def prisma11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A12]] = new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A12]] {
      def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A12]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A12]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
    }

    implicit def inja11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A12]] = Inj.instance(prisma11.reverseGet(_))
    implicit def inja11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A12]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma11.getOption(_))

    implicit def prisma12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A13]] = new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A13]] {
      def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A13]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A13]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
    }

    implicit def inja12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A13]] = Inj.instance(prisma12.reverseGet(_))
    implicit def inja12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A13]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma12.getOption(_))

    implicit def prisma13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A14]] = new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A14]] {
      def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A14]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A14]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
    }

    implicit def inja13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A14]] = Inj.instance(prisma13.reverseGet(_))
    implicit def inja13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A14]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma13.getOption(_))

    implicit def prisma14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A15]] = new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A15]] {
      def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A15]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A15]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))
    }

    implicit def inja14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A15]] = Inj.instance(prisma14.reverseGet(_))
    implicit def inja14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A15]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma14.getOption(_))
  }
  
  @newtype case class Prod16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16]))
  object Prod16 {
    implicit def Prod16Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16])]): Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      MF.xmap(M, Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](_), (_: Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A1], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A2], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A3], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A4], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A5], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A6], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A7], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._7)

    implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    }

    implicit def lifta7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A8], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._8)

    implicit def lifta8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    }

    implicit def lifta8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A9], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._9)

    implicit def lifta9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15, t._16)))
    }

    implicit def lifta9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A10], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._10)

    implicit def lifta10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15, t._16)))
    }

    implicit def lifta10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A11], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._11)

    implicit def lifta11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A12]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15, t._16)))
    }

    implicit def lifta11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A12], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._12)

    implicit def lifta12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A13]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15, t._16)))
    }

    implicit def lifta12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A13], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._13)

    implicit def lifta13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A14]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15, t._16)))
    }

    implicit def lifta13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A14], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._14)

    implicit def lifta14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A15]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x, t._16)))
    }

    implicit def lifta14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A15], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._15)

    implicit def lifta15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A16]] = {
      val t = M.zero.run
      Inj.instance(x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, x)))
    }

    implicit def lifta15Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[F[A16], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._16)
  }

  @newtype case class Cop16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ F[A16]))))))))))))))))
  object Cop16 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A1]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A1]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A1]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A2]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A2]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A2]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A3]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A3]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A3]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A4]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A4]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A4]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A5]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A5]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A5]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A6]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A6]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A6]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A7]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A7]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A7]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A8]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A8]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A8]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A8]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma7.getOption(_))

    implicit def prisma8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A9]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A9]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A9]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
    }

    implicit def inja8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A9]] = Inj.instance(prisma8.reverseGet(_))
    implicit def inja8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A9]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma8.getOption(_))

    implicit def prisma9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A10]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A10]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A10]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A10]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
    }

    implicit def inja9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A10]] = Inj.instance(prisma9.reverseGet(_))
    implicit def inja9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A10]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma9.getOption(_))

    implicit def prisma10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A11]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A11]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A11]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A11]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
    }

    implicit def inja10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A11]] = Inj.instance(prisma10.reverseGet(_))
    implicit def inja10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A11]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma10.getOption(_))

    implicit def prisma11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A12]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A12]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A12]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A12]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
    }

    implicit def inja11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A12]] = Inj.instance(prisma11.reverseGet(_))
    implicit def inja11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A12]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma11.getOption(_))

    implicit def prisma12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A13]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A13]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A13]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A13]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
    }

    implicit def inja12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A13]] = Inj.instance(prisma12.reverseGet(_))
    implicit def inja12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A13]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma12.getOption(_))

    implicit def prisma13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A14]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A14]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A14]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A14]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
    }

    implicit def inja13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A14]] = Inj.instance(prisma13.reverseGet(_))
    implicit def inja13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A14]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma13.getOption(_))

    implicit def prisma14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A15]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A15]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A15]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A15]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
    }

    implicit def inja14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A15]] = Inj.instance(prisma14.reverseGet(_))
    implicit def inja14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A15]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma14.getOption(_))

    implicit def prisma15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A16]] = new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A16]] {
      def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A16]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A16]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))
    }

    implicit def inja15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A16]] = Inj.instance(prisma15.reverseGet(_))
    implicit def inja15Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Option[F[A16]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma15.getOption(_))
  }
  
  @newtype case class Prod17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17]))
  object Prod17 {
    implicit def Prod17Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17])]): Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      MF.xmap(M, Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](_), (_: Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A1], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A2], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A3], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A4], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A5], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A6], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A7], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._7)

    implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    }

    implicit def lifta7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A8], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._8)

    implicit def lifta8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    }

    implicit def lifta8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A9], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._9)

    implicit def lifta9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    }

    implicit def lifta9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A10], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._10)

    implicit def lifta10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15, t._16, t._17)))
    }

    implicit def lifta10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A11], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._11)

    implicit def lifta11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A12]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15, t._16, t._17)))
    }

    implicit def lifta11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A12], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._12)

    implicit def lifta12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A13]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15, t._16, t._17)))
    }

    implicit def lifta12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A13], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._13)

    implicit def lifta13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A14]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15, t._16, t._17)))
    }

    implicit def lifta13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A14], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._14)

    implicit def lifta14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A15]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x, t._16, t._17)))
    }

    implicit def lifta14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A15], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._15)

    implicit def lifta15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A16]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, x, t._17)))
    }

    implicit def lifta15Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A16], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._16)

    implicit def lifta16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A17]] = {
      val t = M.zero.run
      Inj.instance(x => Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, x)))
    }

    implicit def lifta16Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[F[A17], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._17)
  }

  @newtype case class Cop17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ F[A17])))))))))))))))))
  object Cop17 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A1]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A1]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A1]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A2]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A2]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A2]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A3]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A3]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A3]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A4]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A4]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A4]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A5]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A5]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A5]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A6]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A6]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A6]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A7]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A7]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A7]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A8]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A8]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A8]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A8]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma7.getOption(_))

    implicit def prisma8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A9]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A9]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A9]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
    }

    implicit def inja8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A9]] = Inj.instance(prisma8.reverseGet(_))
    implicit def inja8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A9]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma8.getOption(_))

    implicit def prisma9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A10]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A10]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A10]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A10]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
    }

    implicit def inja9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A10]] = Inj.instance(prisma9.reverseGet(_))
    implicit def inja9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A10]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma9.getOption(_))

    implicit def prisma10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A11]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A11]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A11]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A11]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
    }

    implicit def inja10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A11]] = Inj.instance(prisma10.reverseGet(_))
    implicit def inja10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A11]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma10.getOption(_))

    implicit def prisma11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A12]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A12]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A12]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A12]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
    }

    implicit def inja11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A12]] = Inj.instance(prisma11.reverseGet(_))
    implicit def inja11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A12]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma11.getOption(_))

    implicit def prisma12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A13]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A13]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A13]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A13]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
    }

    implicit def inja12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A13]] = Inj.instance(prisma12.reverseGet(_))
    implicit def inja12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A13]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma12.getOption(_))

    implicit def prisma13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A14]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A14]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A14]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A14]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
    }

    implicit def inja13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A14]] = Inj.instance(prisma13.reverseGet(_))
    implicit def inja13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A14]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma13.getOption(_))

    implicit def prisma14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A15]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A15]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A15]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A15]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
    }

    implicit def inja14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A15]] = Inj.instance(prisma14.reverseGet(_))
    implicit def inja14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A15]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma14.getOption(_))

    implicit def prisma15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A16]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A16]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A16]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A16]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))
    }

    implicit def inja15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A16]] = Inj.instance(prisma15.reverseGet(_))
    implicit def inja15Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A16]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma15.getOption(_))

    implicit def prisma16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A17]] = new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A17]] {
      def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A17]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A17]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))
    }

    implicit def inja16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A17]] = Inj.instance(prisma16.reverseGet(_))
    implicit def inja16Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Option[F[A17]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma16.getOption(_))
  }
  
  @newtype case class Prod18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18]))
  object Prod18 {
    implicit def Prod18Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18])]): Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      MF.xmap(M, Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](_), (_: Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A1], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A2], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A3], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A4], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A5], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A6], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A7], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._7)

    implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    }

    implicit def lifta7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A8], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._8)

    implicit def lifta8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    }

    implicit def lifta8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A9], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._9)

    implicit def lifta9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    }

    implicit def lifta9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A10], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._10)

    implicit def lifta10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    }

    implicit def lifta10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A11], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._11)

    implicit def lifta11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A12]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15, t._16, t._17, t._18)))
    }

    implicit def lifta11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A12], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._12)

    implicit def lifta12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A13]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15, t._16, t._17, t._18)))
    }

    implicit def lifta12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A13], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._13)

    implicit def lifta13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A14]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15, t._16, t._17, t._18)))
    }

    implicit def lifta13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A14], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._14)

    implicit def lifta14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A15]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x, t._16, t._17, t._18)))
    }

    implicit def lifta14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A15], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._15)

    implicit def lifta15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A16]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, x, t._17, t._18)))
    }

    implicit def lifta15Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A16], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._16)

    implicit def lifta16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A17]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, x, t._18)))
    }

    implicit def lifta16Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A17], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._17)

    implicit def lifta17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A18]] = {
      val t = M.zero.run
      Inj.instance(x => Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, x)))
    }

    implicit def lifta17Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[F[A18], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._18)
  }

  @newtype case class Cop18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ F[A18]))))))))))))))))))
  object Cop18 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A1]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A1]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A1]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A2]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A2]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A2]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A3]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A3]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A3]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A4]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A4]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A4]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A5]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A5]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A5]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A6]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A6]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A6]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A7]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A7]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A7]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A8]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A8]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A8]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A8]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma7.getOption(_))

    implicit def prisma8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A9]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A9]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A9]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
    }

    implicit def inja8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A9]] = Inj.instance(prisma8.reverseGet(_))
    implicit def inja8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A9]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma8.getOption(_))

    implicit def prisma9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A10]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A10]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A10]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A10]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
    }

    implicit def inja9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A10]] = Inj.instance(prisma9.reverseGet(_))
    implicit def inja9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A10]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma9.getOption(_))

    implicit def prisma10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A11]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A11]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A11]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A11]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
    }

    implicit def inja10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A11]] = Inj.instance(prisma10.reverseGet(_))
    implicit def inja10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A11]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma10.getOption(_))

    implicit def prisma11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A12]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A12]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A12]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A12]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
    }

    implicit def inja11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A12]] = Inj.instance(prisma11.reverseGet(_))
    implicit def inja11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A12]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma11.getOption(_))

    implicit def prisma12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A13]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A13]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A13]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A13]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
    }

    implicit def inja12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A13]] = Inj.instance(prisma12.reverseGet(_))
    implicit def inja12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A13]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma12.getOption(_))

    implicit def prisma13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A14]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A14]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A14]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A14]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
    }

    implicit def inja13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A14]] = Inj.instance(prisma13.reverseGet(_))
    implicit def inja13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A14]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma13.getOption(_))

    implicit def prisma14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A15]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A15]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A15]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A15]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
    }

    implicit def inja14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A15]] = Inj.instance(prisma14.reverseGet(_))
    implicit def inja14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A15]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma14.getOption(_))

    implicit def prisma15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A16]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A16]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A16]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A16]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))
    }

    implicit def inja15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A16]] = Inj.instance(prisma15.reverseGet(_))
    implicit def inja15Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A16]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma15.getOption(_))

    implicit def prisma16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A17]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A17]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A17]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A17]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))
    }

    implicit def inja16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A17]] = Inj.instance(prisma16.reverseGet(_))
    implicit def inja16Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A17]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma16.getOption(_))

    implicit def prisma17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A18]] = new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A18]] {
      def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A18]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A18]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))))
    }

    implicit def inja17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A18]] = Inj.instance(prisma17.reverseGet(_))
    implicit def inja17Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: Inj[Option[F[A18]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma17.getOption(_))
  }
  
  @newtype case class Prod19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19]))
  object Prod19 {
    implicit def Prod19Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19])]): Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      MF.xmap(M, Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](_), (_: Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A1], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A2], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A3], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A4], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A5], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A6], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A7], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._7)

    implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    }

    implicit def lifta7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A8], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._8)

    implicit def lifta8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    }

    implicit def lifta8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A9], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._9)

    implicit def lifta9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    }

    implicit def lifta9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A10], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._10)

    implicit def lifta10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    }

    implicit def lifta10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A11], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._11)

    implicit def lifta11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A12]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    }

    implicit def lifta11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A12], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._12)

    implicit def lifta12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A13]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15, t._16, t._17, t._18, t._19)))
    }

    implicit def lifta12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A13], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._13)

    implicit def lifta13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A14]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15, t._16, t._17, t._18, t._19)))
    }

    implicit def lifta13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A14], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._14)

    implicit def lifta14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A15]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x, t._16, t._17, t._18, t._19)))
    }

    implicit def lifta14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A15], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._15)

    implicit def lifta15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A16]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, x, t._17, t._18, t._19)))
    }

    implicit def lifta15Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A16], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._16)

    implicit def lifta16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A17]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, x, t._18, t._19)))
    }

    implicit def lifta16Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A17], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._17)

    implicit def lifta17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A18]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, x, t._19)))
    }

    implicit def lifta17Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A18], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._18)

    implicit def lifta18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A19]] = {
      val t = M.zero.run
      Inj.instance(x => Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, x)))
    }

    implicit def lifta18Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[F[A19], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._19)
  }

  @newtype case class Cop19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ F[A19])))))))))))))))))))
  object Cop19 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A1]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A1]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A1]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A2]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A2]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A2]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A3]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A3]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A3]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A4]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A4]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A4]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A5]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A5]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A5]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A6]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A6]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A6]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A7]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A7]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A7]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A8]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A8]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A8]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A8]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma7.getOption(_))

    implicit def prisma8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A9]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A9]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A9]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
    }

    implicit def inja8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A9]] = Inj.instance(prisma8.reverseGet(_))
    implicit def inja8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A9]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma8.getOption(_))

    implicit def prisma9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A10]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A10]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A10]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A10]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
    }

    implicit def inja9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A10]] = Inj.instance(prisma9.reverseGet(_))
    implicit def inja9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A10]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma9.getOption(_))

    implicit def prisma10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A11]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A11]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A11]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A11]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
    }

    implicit def inja10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A11]] = Inj.instance(prisma10.reverseGet(_))
    implicit def inja10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A11]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma10.getOption(_))

    implicit def prisma11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A12]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A12]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A12]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A12]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
    }

    implicit def inja11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A12]] = Inj.instance(prisma11.reverseGet(_))
    implicit def inja11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A12]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma11.getOption(_))

    implicit def prisma12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A13]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A13]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A13]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A13]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
    }

    implicit def inja12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A13]] = Inj.instance(prisma12.reverseGet(_))
    implicit def inja12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A13]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma12.getOption(_))

    implicit def prisma13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A14]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A14]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A14]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A14]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
    }

    implicit def inja13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A14]] = Inj.instance(prisma13.reverseGet(_))
    implicit def inja13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A14]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma13.getOption(_))

    implicit def prisma14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A15]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A15]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A15]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A15]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
    }

    implicit def inja14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A15]] = Inj.instance(prisma14.reverseGet(_))
    implicit def inja14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A15]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma14.getOption(_))

    implicit def prisma15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A16]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A16]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A16]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A16]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))
    }

    implicit def inja15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A16]] = Inj.instance(prisma15.reverseGet(_))
    implicit def inja15Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A16]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma15.getOption(_))

    implicit def prisma16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A17]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A17]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A17]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A17]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))
    }

    implicit def inja16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A17]] = Inj.instance(prisma16.reverseGet(_))
    implicit def inja16Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A17]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma16.getOption(_))

    implicit def prisma17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A18]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A18]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A18]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A18]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))
    }

    implicit def inja17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A18]] = Inj.instance(prisma17.reverseGet(_))
    implicit def inja17Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A18]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma17.getOption(_))

    implicit def prisma18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A19]] = new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A19]] {
      def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A19]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A19]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))))
    }

    implicit def inja18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A19]] = Inj.instance(prisma18.reverseGet(_))
    implicit def inja18Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: Inj[Option[F[A19]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma18.getOption(_))
  }
  
  @newtype case class Prod20[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20]))
  object Prod20 {
    implicit def Prod20Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20])]): Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      MF.xmap(M, Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](_), (_: Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A1], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A2], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A3], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A4], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A5], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A6], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A7], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._7)

    implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A8], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._8)

    implicit def lifta8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A9], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._9)

    implicit def lifta9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A10], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._10)

    implicit def lifta10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A11], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._11)

    implicit def lifta11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A12]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A12], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._12)

    implicit def lifta12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A13]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A13], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._13)

    implicit def lifta13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A14]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15, t._16, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A14], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._14)

    implicit def lifta14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A15]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x, t._16, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A15], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._15)

    implicit def lifta15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A16]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, x, t._17, t._18, t._19, t._20)))
    }

    implicit def lifta15Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A16], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._16)

    implicit def lifta16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A17]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, x, t._18, t._19, t._20)))
    }

    implicit def lifta16Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A17], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._17)

    implicit def lifta17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A18]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, x, t._19, t._20)))
    }

    implicit def lifta17Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A18], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._18)

    implicit def lifta18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A19]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, x, t._20)))
    }

    implicit def lifta18Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A19], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._19)

    implicit def lifta19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A20]] = {
      val t = M.zero.run
      Inj.instance(x => Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, x)))
    }

    implicit def lifta19Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[F[A20], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._20)
  }

  @newtype case class Cop20[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ F[A20]))))))))))))))))))))
  object Cop20 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A1]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A1]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A1]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A2]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A2]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A2]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A3]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A3]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A3]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A4]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A4]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A4]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A5]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A5]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A5]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A6]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A6]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A6]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A7]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A7]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A7]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A8]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A8]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A8]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A8]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma7.getOption(_))

    implicit def prisma8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A9]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A9]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A9]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
    }

    implicit def inja8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A9]] = Inj.instance(prisma8.reverseGet(_))
    implicit def inja8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A9]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma8.getOption(_))

    implicit def prisma9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A10]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A10]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A10]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A10]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
    }

    implicit def inja9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A10]] = Inj.instance(prisma9.reverseGet(_))
    implicit def inja9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A10]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma9.getOption(_))

    implicit def prisma10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A11]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A11]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A11]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A11]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
    }

    implicit def inja10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A11]] = Inj.instance(prisma10.reverseGet(_))
    implicit def inja10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A11]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma10.getOption(_))

    implicit def prisma11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A12]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A12]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A12]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A12]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
    }

    implicit def inja11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A12]] = Inj.instance(prisma11.reverseGet(_))
    implicit def inja11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A12]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma11.getOption(_))

    implicit def prisma12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A13]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A13]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A13]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A13]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
    }

    implicit def inja12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A13]] = Inj.instance(prisma12.reverseGet(_))
    implicit def inja12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A13]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma12.getOption(_))

    implicit def prisma13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A14]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A14]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A14]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A14]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
    }

    implicit def inja13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A14]] = Inj.instance(prisma13.reverseGet(_))
    implicit def inja13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A14]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma13.getOption(_))

    implicit def prisma14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A15]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A15]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A15]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A15]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
    }

    implicit def inja14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A15]] = Inj.instance(prisma14.reverseGet(_))
    implicit def inja14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A15]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma14.getOption(_))

    implicit def prisma15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A16]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A16]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A16]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A16]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))
    }

    implicit def inja15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A16]] = Inj.instance(prisma15.reverseGet(_))
    implicit def inja15Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A16]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma15.getOption(_))

    implicit def prisma16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A17]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A17]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A17]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A17]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))
    }

    implicit def inja16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A17]] = Inj.instance(prisma16.reverseGet(_))
    implicit def inja16Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A17]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma16.getOption(_))

    implicit def prisma17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A18]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A18]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A18]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A18]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))
    }

    implicit def inja17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A18]] = Inj.instance(prisma17.reverseGet(_))
    implicit def inja17Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A18]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma17.getOption(_))

    implicit def prisma18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A19]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A19]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A19]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A19]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))))
    }

    implicit def inja18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A19]] = Inj.instance(prisma18.reverseGet(_))
    implicit def inja18Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A19]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma18.getOption(_))

    implicit def prisma19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A20]] = new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A20]] {
      def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A20]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A20]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))))))
    }

    implicit def inja19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A20]] = Inj.instance(prisma19.reverseGet(_))
    implicit def inja19Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: Inj[Option[F[A20]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma19.getOption(_))
  }
  
  @newtype case class Prod21[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20], F[A21]))
  object Prod21 {
    implicit def Prod21Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20], F[A21])]): Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      MF.xmap(M, Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](_), (_: Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A1], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A2], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A3], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A4], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A5], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A6], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A7], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._7)

    implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A8], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._8)

    implicit def lifta8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A9], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._9)

    implicit def lifta9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A10], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._10)

    implicit def lifta10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A11], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._11)

    implicit def lifta11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A12]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A12], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._12)

    implicit def lifta12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A13]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A13], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._13)

    implicit def lifta13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A14]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A14], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._14)

    implicit def lifta14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A15]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x, t._16, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A15], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._15)

    implicit def lifta15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A16]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, x, t._17, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta15Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A16], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._16)

    implicit def lifta16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A17]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, x, t._18, t._19, t._20, t._21)))
    }

    implicit def lifta16Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A17], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._17)

    implicit def lifta17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A18]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, x, t._19, t._20, t._21)))
    }

    implicit def lifta17Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A18], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._18)

    implicit def lifta18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A19]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, x, t._20, t._21)))
    }

    implicit def lifta18Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A19], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._19)

    implicit def lifta19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A20]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, x, t._21)))
    }

    implicit def lifta19Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A20], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._20)

    implicit def lifta20[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A21]] = {
      val t = M.zero.run
      Inj.instance(x => Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, x)))
    }

    implicit def lifta20Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[F[A21], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._21)
  }

  @newtype case class Cop21[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ F[A21])))))))))))))))))))))
  object Cop21 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A1]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A1]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A1]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A2]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A2]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A2]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A3]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A3]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A3]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A4]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A4]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A4]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A5]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A5]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A5]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A6]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A6]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A6]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A7]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A7]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A7]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A8]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A8]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A8]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A8]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma7.getOption(_))

    implicit def prisma8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A9]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A9]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A9]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
    }

    implicit def inja8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A9]] = Inj.instance(prisma8.reverseGet(_))
    implicit def inja8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A9]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma8.getOption(_))

    implicit def prisma9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A10]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A10]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A10]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A10]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
    }

    implicit def inja9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A10]] = Inj.instance(prisma9.reverseGet(_))
    implicit def inja9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A10]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma9.getOption(_))

    implicit def prisma10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A11]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A11]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A11]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A11]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
    }

    implicit def inja10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A11]] = Inj.instance(prisma10.reverseGet(_))
    implicit def inja10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A11]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma10.getOption(_))

    implicit def prisma11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A12]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A12]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A12]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A12]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
    }

    implicit def inja11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A12]] = Inj.instance(prisma11.reverseGet(_))
    implicit def inja11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A12]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma11.getOption(_))

    implicit def prisma12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A13]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A13]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A13]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A13]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
    }

    implicit def inja12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A13]] = Inj.instance(prisma12.reverseGet(_))
    implicit def inja12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A13]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma12.getOption(_))

    implicit def prisma13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A14]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A14]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A14]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A14]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
    }

    implicit def inja13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A14]] = Inj.instance(prisma13.reverseGet(_))
    implicit def inja13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A14]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma13.getOption(_))

    implicit def prisma14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A15]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A15]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A15]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A15]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
    }

    implicit def inja14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A15]] = Inj.instance(prisma14.reverseGet(_))
    implicit def inja14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A15]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma14.getOption(_))

    implicit def prisma15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A16]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A16]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A16]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A16]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))
    }

    implicit def inja15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A16]] = Inj.instance(prisma15.reverseGet(_))
    implicit def inja15Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A16]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma15.getOption(_))

    implicit def prisma16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A17]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A17]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A17]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A17]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))
    }

    implicit def inja16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A17]] = Inj.instance(prisma16.reverseGet(_))
    implicit def inja16Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A17]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma16.getOption(_))

    implicit def prisma17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A18]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A18]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A18]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A18]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))
    }

    implicit def inja17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A18]] = Inj.instance(prisma17.reverseGet(_))
    implicit def inja17Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A18]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma17.getOption(_))

    implicit def prisma18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A19]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A19]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A19]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A19]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))))
    }

    implicit def inja18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A19]] = Inj.instance(prisma18.reverseGet(_))
    implicit def inja18Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A19]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma18.getOption(_))

    implicit def prisma19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A20]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A20]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A20]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A20]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))))
    }

    implicit def inja19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A20]] = Inj.instance(prisma19.reverseGet(_))
    implicit def inja19Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A20]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma19.getOption(_))

    implicit def prisma20[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A21]] = new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A21]] {
      def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A21]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A21]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))))))
    }

    implicit def inja20[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A21]] = Inj.instance(prisma20.reverseGet(_))
    implicit def inja20Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: Inj[Option[F[A21]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma20.getOption(_))
  }
  
  @newtype case class Prod22[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20], F[A21], F[A22]))
  object Prod22 {
    implicit def Prod22Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20], F[A21], F[A22])]): Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      MF.xmap(M, Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](_), (_: Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]).run)

    implicit def lifta0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A1], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._1)

    implicit def lifta1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A2], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._2)

    implicit def lifta2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A3], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._3)

    implicit def lifta3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A4], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._4)

    implicit def lifta4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A5], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._5)

    implicit def lifta5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A6], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._6)

    implicit def lifta6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A7], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._7)

    implicit def lifta7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A8], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._8)

    implicit def lifta8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A9], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._9)

    implicit def lifta9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A10], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._10)

    implicit def lifta10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A11], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._11)

    implicit def lifta11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A12]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A12], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._12)

    implicit def lifta12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A13]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A13], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._13)

    implicit def lifta13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A14]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A14], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._14)

    implicit def lifta14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A15]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A15], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._15)

    implicit def lifta15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A16]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, x, t._17, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta15Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A16], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._16)

    implicit def lifta16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A17]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, x, t._18, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta16Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A17], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._17)

    implicit def lifta17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A18]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, x, t._19, t._20, t._21, t._22)))
    }

    implicit def lifta17Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A18], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._18)

    implicit def lifta18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A19]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, x, t._20, t._21, t._22)))
    }

    implicit def lifta18Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A19], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._19)

    implicit def lifta19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A20]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, x, t._21, t._22)))
    }

    implicit def lifta19Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A20], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._20)

    implicit def lifta20[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A21]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, x, t._22)))
    }

    implicit def lifta20Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A21], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._21)

    implicit def lifta21[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A22]] = {
      val t = M.zero.run
      Inj.instance(x => Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, x)))
    }

    implicit def lifta21Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[F[A22], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._22)
  }

  @newtype case class Cop22[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[A19] \/ (F[A20] \/ (F[A21] \/ F[A22]))))))))))))))))))))))
  object Cop22 {
    implicit def prisma0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A1]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A1]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A1]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](-\/(x))
    }

    implicit def inja0[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A1]] = Inj.instance(prisma0.reverseGet(_))
    implicit def inja0Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A1]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma0.getOption(_))

    implicit def prisma1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A2]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A2]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A2]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(-\/(x)))
    }

    implicit def inja1[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A2]] = Inj.instance(prisma1.reverseGet(_))
    implicit def inja1Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A2]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma1.getOption(_))

    implicit def prisma2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A3]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A3]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A3]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(-\/(x))))
    }

    implicit def inja2[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A3]] = Inj.instance(prisma2.reverseGet(_))
    implicit def inja2Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A3]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma2.getOption(_))

    implicit def prisma3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A4]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A4]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A4]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A4]] = Inj.instance(prisma3.reverseGet(_))
    implicit def inja3Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A4]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma3.getOption(_))

    implicit def prisma4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A5]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A5]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A5]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A5]] = Inj.instance(prisma4.reverseGet(_))
    implicit def inja4Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A5]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma4.getOption(_))

    implicit def prisma5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A6]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A6]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A6]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A6]] = Inj.instance(prisma5.reverseGet(_))
    implicit def inja5Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A6]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma5.getOption(_))

    implicit def prisma6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A7]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A7]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A7]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A7]] = Inj.instance(prisma6.reverseGet(_))
    implicit def inja6Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A7]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma6.getOption(_))

    implicit def prisma7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A8]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A8]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A8]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A8]] = Inj.instance(prisma7.reverseGet(_))
    implicit def inja7Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A8]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma7.getOption(_))

    implicit def prisma8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A9]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A9]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A9]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
    }

    implicit def inja8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A9]] = Inj.instance(prisma8.reverseGet(_))
    implicit def inja8Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A9]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma8.getOption(_))

    implicit def prisma9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A10]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A10]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A10]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A10]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
    }

    implicit def inja9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A10]] = Inj.instance(prisma9.reverseGet(_))
    implicit def inja9Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A10]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma9.getOption(_))

    implicit def prisma10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A11]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A11]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A11]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A11]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
    }

    implicit def inja10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A11]] = Inj.instance(prisma10.reverseGet(_))
    implicit def inja10Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A11]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma10.getOption(_))

    implicit def prisma11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A12]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A12]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A12]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A12]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
    }

    implicit def inja11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A12]] = Inj.instance(prisma11.reverseGet(_))
    implicit def inja11Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A12]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma11.getOption(_))

    implicit def prisma12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A13]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A13]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A13]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A13]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
    }

    implicit def inja12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A13]] = Inj.instance(prisma12.reverseGet(_))
    implicit def inja12Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A13]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma12.getOption(_))

    implicit def prisma13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A14]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A14]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A14]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A14]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
    }

    implicit def inja13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A14]] = Inj.instance(prisma13.reverseGet(_))
    implicit def inja13Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A14]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma13.getOption(_))

    implicit def prisma14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A15]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A15]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A15]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A15]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
    }

    implicit def inja14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A15]] = Inj.instance(prisma14.reverseGet(_))
    implicit def inja14Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A15]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma14.getOption(_))

    implicit def prisma15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A16]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A16]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A16]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A16]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))
    }

    implicit def inja15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A16]] = Inj.instance(prisma15.reverseGet(_))
    implicit def inja15Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A16]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma15.getOption(_))

    implicit def prisma16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A17]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A17]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A17]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A17]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))
    }

    implicit def inja16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A17]] = Inj.instance(prisma16.reverseGet(_))
    implicit def inja16Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A17]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma16.getOption(_))

    implicit def prisma17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A18]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A18]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A18]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A18]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))
    }

    implicit def inja17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A18]] = Inj.instance(prisma17.reverseGet(_))
    implicit def inja17Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A18]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma17.getOption(_))

    implicit def prisma18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A19]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A19]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A19]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A19]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))))
    }

    implicit def inja18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A19]] = Inj.instance(prisma18.reverseGet(_))
    implicit def inja18Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A19]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma18.getOption(_))

    implicit def prisma19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A20]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A20]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A20]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A20]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))))
    }

    implicit def inja19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A20]] = Inj.instance(prisma19.reverseGet(_))
    implicit def inja19Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A20]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma19.getOption(_))

    implicit def prisma20[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A21]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A21]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A21]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A21]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))))))
    }

    implicit def inja20[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A21]] = Inj.instance(prisma20.reverseGet(_))
    implicit def inja20Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A21]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma20.getOption(_))

    implicit def prisma21[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A22]] = new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A22]] {
      def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A22]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))))))) => Some(x)
        case _ => None
      }
      def reverseGet(x: F[A22]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))))))))
    }

    implicit def inja21[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A22]] = Inj.instance(prisma21.reverseGet(_))
    implicit def inja21Inverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: Inj[Option[F[A22]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma21.getOption(_))
  }
  
}
