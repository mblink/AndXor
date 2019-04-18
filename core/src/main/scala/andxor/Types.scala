package andxor

import io.estatico.newtype.macros.newtype
import scalaz.{\/, -\/, \/-, InvariantFunctor, Monoid}
import scalaz.Id.Id

object types {
  private val MF = InvariantFunctor[Monoid]

  @newtype case class Prod1[F[_], A1](run: (F[A1]))
  trait Prod1LP {
    implicit def Prod1Monoid[F[_], A1](implicit M: Monoid[(F[A1])]): Monoid[Prod1[F, A1]] =
      MF.xmap(M, Prod1[F, A1](_), (_: Prod1[F, A1]).run)

    implicit def lifta0F[F[_], A1]: Inj[Prod1[F, A1], F[A1]] = {
      Inj.instance(x => Prod1[F, A1]((x)))
    }

    implicit def lifta0FInverse[F[_], A1]: Inj[F[A1], Prod1[F, A1]] = Inj.instance(_.run)
  }
  object Prod1 extends Prod1LP {
    implicit def lifta0Id[A1]: Inj[Prod1[Id, A1], A1] = lifta0F[Id, A1]

    implicit def lifta0IdInverse[A1]: Inj[A1, Prod1[Id, A1]] = lifta0FInverse[Id, A1]
  }

  @newtype case class Cop1[F[_], A1](run: F[A1])
  trait Cop1LP {
    implicit def prisma0F[F[_], A1]: Prism[Cop1[F, A1], F[A1]] = new Prism[Cop1[F, A1], F[A1]] {
      def getOption(c: Cop1[F, A1]): Option[F[A1]] = Some(c.run)
      def reverseGet(x: F[A1]): Cop1[F, A1] = Cop1[F, A1](x)
    }

    implicit def inja0F[F[_], A1]: Inj[Cop1[F, A1], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1]: Inj[Option[F[A1]], Cop1[F, A1]] = Inj.instance(prisma0F.getOption(_))
  }
  object Cop1 extends Cop1LP {
    implicit def prisma0Id[A1]: Prism[Cop1[Id, A1], A1] = prisma0F[Id, A1]

    implicit def inja0Id[A1]: Inj[Cop1[Id, A1], A1] = inja0F[Id, A1]

    implicit def inja0IdInverse[F[_], A1]: Inj[Option[A1], Cop1[Id, A1]] = inja0FInverse[Id, A1]
  }

  @newtype case class Prod2[F[_], A1, A2](run: (F[A1], F[A2]))
  trait Prod2LP {
    implicit def Prod2Monoid[F[_], A1, A2](implicit M: Monoid[(F[A1], F[A2])]): Monoid[Prod2[F, A1, A2]] =
      MF.xmap(M, Prod2[F, A1, A2](_), (_: Prod2[F, A1, A2]).run)

    implicit def lifta0F[F[_], A1, A2](implicit M: Monoid[Prod2[F, A1, A2]]): Inj[Prod2[F, A1, A2], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod2[F, A1, A2]((x, t._2)))
    }

    implicit def lifta0FInverse[F[_], A1, A2]: Inj[F[A1], Prod2[F, A1, A2]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2](implicit M: Monoid[Prod2[F, A1, A2]]): Inj[Prod2[F, A1, A2], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod2[F, A1, A2]((t._1, x)))
    }

    implicit def lifta1FInverse[F[_], A1, A2]: Inj[F[A2], Prod2[F, A1, A2]] = Inj.instance(_.run._2)
  }
  object Prod2 extends Prod2LP {
    implicit def lifta0Id[A1, A2](implicit M: Monoid[Prod2[Id, A1, A2]]): Inj[Prod2[Id, A1, A2], A1] = lifta0F[Id, A1, A2]

    implicit def lifta0IdInverse[A1, A2]: Inj[A1, Prod2[Id, A1, A2]] = lifta0FInverse[Id, A1, A2]

    implicit def lifta1Id[A1, A2](implicit M: Monoid[Prod2[Id, A1, A2]]): Inj[Prod2[Id, A1, A2], A2] = lifta1F[Id, A1, A2]

    implicit def lifta1IdInverse[A1, A2]: Inj[A2, Prod2[Id, A1, A2]] = lifta1FInverse[Id, A1, A2]
  }

  @newtype case class Cop2[F[_], A1, A2](run: (F[A1] \/ F[A2]))
  trait Cop2LP {
    implicit def prisma0F[F[_], A1, A2]: Prism[Cop2[F, A1, A2], F[A1]] = new Prism[Cop2[F, A1, A2], F[A1]] {
      def getOption(c: Cop2[F, A1, A2]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _      => None
      }
      def reverseGet(x: F[A1]): Cop2[F, A1, A2] = Cop2[F, A1, A2](-\/(x))
    }

    implicit def inja0F[F[_], A1, A2]: Inj[Cop2[F, A1, A2], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2]: Inj[Option[F[A1]], Cop2[F, A1, A2]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2]: Prism[Cop2[F, A1, A2], F[A2]] = new Prism[Cop2[F, A1, A2], F[A2]] {
      def getOption(c: Cop2[F, A1, A2]): Option[F[A2]] = c.run match {
        case \/-(x) => Some(x)
        case _      => None
      }
      def reverseGet(x: F[A2]): Cop2[F, A1, A2] = Cop2[F, A1, A2](\/-(x))
    }

    implicit def inja1F[F[_], A1, A2]: Inj[Cop2[F, A1, A2], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2]: Inj[Option[F[A2]], Cop2[F, A1, A2]] = Inj.instance(prisma1F.getOption(_))
  }
  object Cop2 extends Cop2LP {
    implicit def prisma0Id[A1, A2]: Prism[Cop2[Id, A1, A2], A1] = prisma0F[Id, A1, A2]

    implicit def inja0Id[A1, A2]: Inj[Cop2[Id, A1, A2], A1] = inja0F[Id, A1, A2]

    implicit def inja0IdInverse[F[_], A1, A2]: Inj[Option[A1], Cop2[Id, A1, A2]] = inja0FInverse[Id, A1, A2]

    implicit def prisma1Id[A1, A2]: Prism[Cop2[Id, A1, A2], A2] = prisma1F[Id, A1, A2]

    implicit def inja1Id[A1, A2]: Inj[Cop2[Id, A1, A2], A2] = inja1F[Id, A1, A2]

    implicit def inja1IdInverse[F[_], A1, A2]: Inj[Option[A2], Cop2[Id, A1, A2]] = inja1FInverse[Id, A1, A2]
  }

  @newtype case class Prod3[F[_], A1, A2, A3](run: (F[A1], F[A2], F[A3]))
  trait Prod3LP {
    implicit def Prod3Monoid[F[_], A1, A2, A3](implicit M: Monoid[(F[A1], F[A2], F[A3])]): Monoid[Prod3[F, A1, A2, A3]] =
      MF.xmap(M, Prod3[F, A1, A2, A3](_), (_: Prod3[F, A1, A2, A3]).run)

    implicit def lifta0F[F[_], A1, A2, A3](implicit M: Monoid[Prod3[F, A1, A2, A3]]): Inj[Prod3[F, A1, A2, A3], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod3[F, A1, A2, A3]((x, t._2, t._3)))
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3]: Inj[F[A1], Prod3[F, A1, A2, A3]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3](implicit M: Monoid[Prod3[F, A1, A2, A3]]): Inj[Prod3[F, A1, A2, A3], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod3[F, A1, A2, A3]((t._1, x, t._3)))
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3]: Inj[F[A2], Prod3[F, A1, A2, A3]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3](implicit M: Monoid[Prod3[F, A1, A2, A3]]): Inj[Prod3[F, A1, A2, A3], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod3[F, A1, A2, A3]((t._1, t._2, x)))
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3]: Inj[F[A3], Prod3[F, A1, A2, A3]] = Inj.instance(_.run._3)
  }
  object Prod3 extends Prod3LP {
    implicit def lifta0Id[A1, A2, A3](implicit M: Monoid[Prod3[Id, A1, A2, A3]]): Inj[Prod3[Id, A1, A2, A3], A1] = lifta0F[Id, A1, A2, A3]

    implicit def lifta0IdInverse[A1, A2, A3]: Inj[A1, Prod3[Id, A1, A2, A3]] = lifta0FInverse[Id, A1, A2, A3]

    implicit def lifta1Id[A1, A2, A3](implicit M: Monoid[Prod3[Id, A1, A2, A3]]): Inj[Prod3[Id, A1, A2, A3], A2] = lifta1F[Id, A1, A2, A3]

    implicit def lifta1IdInverse[A1, A2, A3]: Inj[A2, Prod3[Id, A1, A2, A3]] = lifta1FInverse[Id, A1, A2, A3]

    implicit def lifta2Id[A1, A2, A3](implicit M: Monoid[Prod3[Id, A1, A2, A3]]): Inj[Prod3[Id, A1, A2, A3], A3] = lifta2F[Id, A1, A2, A3]

    implicit def lifta2IdInverse[A1, A2, A3]: Inj[A3, Prod3[Id, A1, A2, A3]] = lifta2FInverse[Id, A1, A2, A3]
  }

  @newtype case class Cop3[F[_], A1, A2, A3](run: (F[A1] \/ (F[A2] \/ F[A3])))
  trait Cop3LP {
    implicit def prisma0F[F[_], A1, A2, A3]: Prism[Cop3[F, A1, A2, A3], F[A1]] = new Prism[Cop3[F, A1, A2, A3], F[A1]] {
      def getOption(c: Cop3[F, A1, A2, A3]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _      => None
      }
      def reverseGet(x: F[A1]): Cop3[F, A1, A2, A3] = Cop3[F, A1, A2, A3](-\/(x))
    }

    implicit def inja0F[F[_], A1, A2, A3]: Inj[Cop3[F, A1, A2, A3], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3]: Inj[Option[F[A1]], Cop3[F, A1, A2, A3]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3]: Prism[Cop3[F, A1, A2, A3], F[A2]] = new Prism[Cop3[F, A1, A2, A3], F[A2]] {
      def getOption(c: Cop3[F, A1, A2, A3]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _           => None
      }
      def reverseGet(x: F[A2]): Cop3[F, A1, A2, A3] = Cop3[F, A1, A2, A3](\/-(-\/(x)))
    }

    implicit def inja1F[F[_], A1, A2, A3]: Inj[Cop3[F, A1, A2, A3], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3]: Inj[Option[F[A2]], Cop3[F, A1, A2, A3]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3]: Prism[Cop3[F, A1, A2, A3], F[A3]] = new Prism[Cop3[F, A1, A2, A3], F[A3]] {
      def getOption(c: Cop3[F, A1, A2, A3]): Option[F[A3]] = c.run match {
        case \/-(\/-(x)) => Some(x)
        case _           => None
      }
      def reverseGet(x: F[A3]): Cop3[F, A1, A2, A3] = Cop3[F, A1, A2, A3](\/-(\/-(x)))
    }

    implicit def inja2F[F[_], A1, A2, A3]: Inj[Cop3[F, A1, A2, A3], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3]: Inj[Option[F[A3]], Cop3[F, A1, A2, A3]] = Inj.instance(prisma2F.getOption(_))
  }
  object Cop3 extends Cop3LP {
    implicit def prisma0Id[A1, A2, A3]: Prism[Cop3[Id, A1, A2, A3], A1] = prisma0F[Id, A1, A2, A3]

    implicit def inja0Id[A1, A2, A3]: Inj[Cop3[Id, A1, A2, A3], A1] = inja0F[Id, A1, A2, A3]

    implicit def inja0IdInverse[F[_], A1, A2, A3]: Inj[Option[A1], Cop3[Id, A1, A2, A3]] = inja0FInverse[Id, A1, A2, A3]

    implicit def prisma1Id[A1, A2, A3]: Prism[Cop3[Id, A1, A2, A3], A2] = prisma1F[Id, A1, A2, A3]

    implicit def inja1Id[A1, A2, A3]: Inj[Cop3[Id, A1, A2, A3], A2] = inja1F[Id, A1, A2, A3]

    implicit def inja1IdInverse[F[_], A1, A2, A3]: Inj[Option[A2], Cop3[Id, A1, A2, A3]] = inja1FInverse[Id, A1, A2, A3]

    implicit def prisma2Id[A1, A2, A3]: Prism[Cop3[Id, A1, A2, A3], A3] = prisma2F[Id, A1, A2, A3]

    implicit def inja2Id[A1, A2, A3]: Inj[Cop3[Id, A1, A2, A3], A3] = inja2F[Id, A1, A2, A3]

    implicit def inja2IdInverse[F[_], A1, A2, A3]: Inj[Option[A3], Cop3[Id, A1, A2, A3]] = inja2FInverse[Id, A1, A2, A3]
  }

  @newtype case class Prod4[F[_], A1, A2, A3, A4](run: (F[A1], F[A2], F[A3], F[A4]))
  trait Prod4LP {
    implicit def Prod4Monoid[F[_], A1, A2, A3, A4](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4])]): Monoid[Prod4[F, A1, A2, A3, A4]] =
      MF.xmap(M, Prod4[F, A1, A2, A3, A4](_), (_: Prod4[F, A1, A2, A3, A4]).run)

    implicit def lifta0F[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((x, t._2, t._3, t._4)))
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4]: Inj[F[A1], Prod4[F, A1, A2, A3, A4]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((t._1, x, t._3, t._4)))
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4]: Inj[F[A2], Prod4[F, A1, A2, A3, A4]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((t._1, t._2, x, t._4)))
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4]: Inj[F[A3], Prod4[F, A1, A2, A3, A4]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4](implicit M: Monoid[Prod4[F, A1, A2, A3, A4]]): Inj[Prod4[F, A1, A2, A3, A4], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod4[F, A1, A2, A3, A4]((t._1, t._2, t._3, x)))
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4]: Inj[F[A4], Prod4[F, A1, A2, A3, A4]] = Inj.instance(_.run._4)
  }
  object Prod4 extends Prod4LP {
    implicit def lifta0Id[A1, A2, A3, A4](implicit M: Monoid[Prod4[Id, A1, A2, A3, A4]]): Inj[Prod4[Id, A1, A2, A3, A4], A1] = lifta0F[Id, A1, A2, A3, A4]

    implicit def lifta0IdInverse[A1, A2, A3, A4]: Inj[A1, Prod4[Id, A1, A2, A3, A4]] = lifta0FInverse[Id, A1, A2, A3, A4]

    implicit def lifta1Id[A1, A2, A3, A4](implicit M: Monoid[Prod4[Id, A1, A2, A3, A4]]): Inj[Prod4[Id, A1, A2, A3, A4], A2] = lifta1F[Id, A1, A2, A3, A4]

    implicit def lifta1IdInverse[A1, A2, A3, A4]: Inj[A2, Prod4[Id, A1, A2, A3, A4]] = lifta1FInverse[Id, A1, A2, A3, A4]

    implicit def lifta2Id[A1, A2, A3, A4](implicit M: Monoid[Prod4[Id, A1, A2, A3, A4]]): Inj[Prod4[Id, A1, A2, A3, A4], A3] = lifta2F[Id, A1, A2, A3, A4]

    implicit def lifta2IdInverse[A1, A2, A3, A4]: Inj[A3, Prod4[Id, A1, A2, A3, A4]] = lifta2FInverse[Id, A1, A2, A3, A4]

    implicit def lifta3Id[A1, A2, A3, A4](implicit M: Monoid[Prod4[Id, A1, A2, A3, A4]]): Inj[Prod4[Id, A1, A2, A3, A4], A4] = lifta3F[Id, A1, A2, A3, A4]

    implicit def lifta3IdInverse[A1, A2, A3, A4]: Inj[A4, Prod4[Id, A1, A2, A3, A4]] = lifta3FInverse[Id, A1, A2, A3, A4]
  }

  @newtype case class Cop4[F[_], A1, A2, A3, A4](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ F[A4]))))
  trait Cop4LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4]: Prism[Cop4[F, A1, A2, A3, A4], F[A1]] = new Prism[Cop4[F, A1, A2, A3, A4], F[A1]] {
      def getOption(c: Cop4[F, A1, A2, A3, A4]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _      => None
      }
      def reverseGet(x: F[A1]): Cop4[F, A1, A2, A3, A4] = Cop4[F, A1, A2, A3, A4](-\/(x))
    }

    implicit def inja0F[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4]: Inj[Option[F[A1]], Cop4[F, A1, A2, A3, A4]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4]: Prism[Cop4[F, A1, A2, A3, A4], F[A2]] = new Prism[Cop4[F, A1, A2, A3, A4], F[A2]] {
      def getOption(c: Cop4[F, A1, A2, A3, A4]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _           => None
      }
      def reverseGet(x: F[A2]): Cop4[F, A1, A2, A3, A4] = Cop4[F, A1, A2, A3, A4](\/-(-\/(x)))
    }

    implicit def inja1F[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4]: Inj[Option[F[A2]], Cop4[F, A1, A2, A3, A4]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4]: Prism[Cop4[F, A1, A2, A3, A4], F[A3]] = new Prism[Cop4[F, A1, A2, A3, A4], F[A3]] {
      def getOption(c: Cop4[F, A1, A2, A3, A4]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _                => None
      }
      def reverseGet(x: F[A3]): Cop4[F, A1, A2, A3, A4] = Cop4[F, A1, A2, A3, A4](\/-(\/-(-\/(x))))
    }

    implicit def inja2F[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4]: Inj[Option[F[A3]], Cop4[F, A1, A2, A3, A4]] = Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4]: Prism[Cop4[F, A1, A2, A3, A4], F[A4]] = new Prism[Cop4[F, A1, A2, A3, A4], F[A4]] {
      def getOption(c: Cop4[F, A1, A2, A3, A4]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(x))) => Some(x)
        case _                => None
      }
      def reverseGet(x: F[A4]): Cop4[F, A1, A2, A3, A4] = Cop4[F, A1, A2, A3, A4](\/-(\/-(\/-(x))))
    }

    implicit def inja3F[F[_], A1, A2, A3, A4]: Inj[Cop4[F, A1, A2, A3, A4], F[A4]] = Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4]: Inj[Option[F[A4]], Cop4[F, A1, A2, A3, A4]] = Inj.instance(prisma3F.getOption(_))
  }
  object Cop4 extends Cop4LP {
    implicit def prisma0Id[A1, A2, A3, A4]: Prism[Cop4[Id, A1, A2, A3, A4], A1] = prisma0F[Id, A1, A2, A3, A4]

    implicit def inja0Id[A1, A2, A3, A4]: Inj[Cop4[Id, A1, A2, A3, A4], A1] = inja0F[Id, A1, A2, A3, A4]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4]: Inj[Option[A1], Cop4[Id, A1, A2, A3, A4]] = inja0FInverse[Id, A1, A2, A3, A4]

    implicit def prisma1Id[A1, A2, A3, A4]: Prism[Cop4[Id, A1, A2, A3, A4], A2] = prisma1F[Id, A1, A2, A3, A4]

    implicit def inja1Id[A1, A2, A3, A4]: Inj[Cop4[Id, A1, A2, A3, A4], A2] = inja1F[Id, A1, A2, A3, A4]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4]: Inj[Option[A2], Cop4[Id, A1, A2, A3, A4]] = inja1FInverse[Id, A1, A2, A3, A4]

    implicit def prisma2Id[A1, A2, A3, A4]: Prism[Cop4[Id, A1, A2, A3, A4], A3] = prisma2F[Id, A1, A2, A3, A4]

    implicit def inja2Id[A1, A2, A3, A4]: Inj[Cop4[Id, A1, A2, A3, A4], A3] = inja2F[Id, A1, A2, A3, A4]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4]: Inj[Option[A3], Cop4[Id, A1, A2, A3, A4]] = inja2FInverse[Id, A1, A2, A3, A4]

    implicit def prisma3Id[A1, A2, A3, A4]: Prism[Cop4[Id, A1, A2, A3, A4], A4] = prisma3F[Id, A1, A2, A3, A4]

    implicit def inja3Id[A1, A2, A3, A4]: Inj[Cop4[Id, A1, A2, A3, A4], A4] = inja3F[Id, A1, A2, A3, A4]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4]: Inj[Option[A4], Cop4[Id, A1, A2, A3, A4]] = inja3FInverse[Id, A1, A2, A3, A4]
  }

  @newtype case class Prod5[F[_], A1, A2, A3, A4, A5](run: (F[A1], F[A2], F[A3], F[A4], F[A5]))
  trait Prod5LP {
    implicit def Prod5Monoid[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5])]): Monoid[Prod5[F, A1, A2, A3, A4, A5]] =
      MF.xmap(M, Prod5[F, A1, A2, A3, A4, A5](_), (_: Prod5[F, A1, A2, A3, A4, A5]).run)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[F, A1, A2, A3, A4, A5]]): Inj[Prod5[F, A1, A2, A3, A4, A5], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod5[F, A1, A2, A3, A4, A5]((x, t._2, t._3, t._4, t._5)))
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5]: Inj[F[A1], Prod5[F, A1, A2, A3, A4, A5]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[F, A1, A2, A3, A4, A5]]): Inj[Prod5[F, A1, A2, A3, A4, A5], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod5[F, A1, A2, A3, A4, A5]((t._1, x, t._3, t._4, t._5)))
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5]: Inj[F[A2], Prod5[F, A1, A2, A3, A4, A5]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[F, A1, A2, A3, A4, A5]]): Inj[Prod5[F, A1, A2, A3, A4, A5], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod5[F, A1, A2, A3, A4, A5]((t._1, t._2, x, t._4, t._5)))
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5]: Inj[F[A3], Prod5[F, A1, A2, A3, A4, A5]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[F, A1, A2, A3, A4, A5]]): Inj[Prod5[F, A1, A2, A3, A4, A5], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod5[F, A1, A2, A3, A4, A5]((t._1, t._2, t._3, x, t._5)))
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5]: Inj[F[A4], Prod5[F, A1, A2, A3, A4, A5]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[F, A1, A2, A3, A4, A5]]): Inj[Prod5[F, A1, A2, A3, A4, A5], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod5[F, A1, A2, A3, A4, A5]((t._1, t._2, t._3, t._4, x)))
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5]: Inj[F[A5], Prod5[F, A1, A2, A3, A4, A5]] = Inj.instance(_.run._5)
  }
  object Prod5 extends Prod5LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[Id, A1, A2, A3, A4, A5]]): Inj[Prod5[Id, A1, A2, A3, A4, A5], A1] = lifta0F[Id, A1, A2, A3, A4, A5]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5]: Inj[A1, Prod5[Id, A1, A2, A3, A4, A5]] = lifta0FInverse[Id, A1, A2, A3, A4, A5]

    implicit def lifta1Id[A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[Id, A1, A2, A3, A4, A5]]): Inj[Prod5[Id, A1, A2, A3, A4, A5], A2] = lifta1F[Id, A1, A2, A3, A4, A5]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5]: Inj[A2, Prod5[Id, A1, A2, A3, A4, A5]] = lifta1FInverse[Id, A1, A2, A3, A4, A5]

    implicit def lifta2Id[A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[Id, A1, A2, A3, A4, A5]]): Inj[Prod5[Id, A1, A2, A3, A4, A5], A3] = lifta2F[Id, A1, A2, A3, A4, A5]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5]: Inj[A3, Prod5[Id, A1, A2, A3, A4, A5]] = lifta2FInverse[Id, A1, A2, A3, A4, A5]

    implicit def lifta3Id[A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[Id, A1, A2, A3, A4, A5]]): Inj[Prod5[Id, A1, A2, A3, A4, A5], A4] = lifta3F[Id, A1, A2, A3, A4, A5]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5]: Inj[A4, Prod5[Id, A1, A2, A3, A4, A5]] = lifta3FInverse[Id, A1, A2, A3, A4, A5]

    implicit def lifta4Id[A1, A2, A3, A4, A5](implicit M: Monoid[Prod5[Id, A1, A2, A3, A4, A5]]): Inj[Prod5[Id, A1, A2, A3, A4, A5], A5] = lifta4F[Id, A1, A2, A3, A4, A5]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5]: Inj[A5, Prod5[Id, A1, A2, A3, A4, A5]] = lifta4FInverse[Id, A1, A2, A3, A4, A5]
  }

  @newtype case class Cop5[F[_], A1, A2, A3, A4, A5](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ F[A5])))))
  trait Cop5LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5]: Prism[Cop5[F, A1, A2, A3, A4, A5], F[A1]] = new Prism[Cop5[F, A1, A2, A3, A4, A5], F[A1]] {
      def getOption(c: Cop5[F, A1, A2, A3, A4, A5]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _      => None
      }
      def reverseGet(x: F[A1]): Cop5[F, A1, A2, A3, A4, A5] = Cop5[F, A1, A2, A3, A4, A5](-\/(x))
    }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5]: Inj[Cop5[F, A1, A2, A3, A4, A5], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5]: Inj[Option[F[A1]], Cop5[F, A1, A2, A3, A4, A5]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5]: Prism[Cop5[F, A1, A2, A3, A4, A5], F[A2]] = new Prism[Cop5[F, A1, A2, A3, A4, A5], F[A2]] {
      def getOption(c: Cop5[F, A1, A2, A3, A4, A5]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _           => None
      }
      def reverseGet(x: F[A2]): Cop5[F, A1, A2, A3, A4, A5] = Cop5[F, A1, A2, A3, A4, A5](\/-(-\/(x)))
    }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5]: Inj[Cop5[F, A1, A2, A3, A4, A5], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5]: Inj[Option[F[A2]], Cop5[F, A1, A2, A3, A4, A5]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5]: Prism[Cop5[F, A1, A2, A3, A4, A5], F[A3]] = new Prism[Cop5[F, A1, A2, A3, A4, A5], F[A3]] {
      def getOption(c: Cop5[F, A1, A2, A3, A4, A5]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _                => None
      }
      def reverseGet(x: F[A3]): Cop5[F, A1, A2, A3, A4, A5] = Cop5[F, A1, A2, A3, A4, A5](\/-(\/-(-\/(x))))
    }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5]: Inj[Cop5[F, A1, A2, A3, A4, A5], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5]: Inj[Option[F[A3]], Cop5[F, A1, A2, A3, A4, A5]] = Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5]: Prism[Cop5[F, A1, A2, A3, A4, A5], F[A4]] = new Prism[Cop5[F, A1, A2, A3, A4, A5], F[A4]] {
      def getOption(c: Cop5[F, A1, A2, A3, A4, A5]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _                     => None
      }
      def reverseGet(x: F[A4]): Cop5[F, A1, A2, A3, A4, A5] = Cop5[F, A1, A2, A3, A4, A5](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5]: Inj[Cop5[F, A1, A2, A3, A4, A5], F[A4]] = Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5]: Inj[Option[F[A4]], Cop5[F, A1, A2, A3, A4, A5]] = Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5]: Prism[Cop5[F, A1, A2, A3, A4, A5], F[A5]] = new Prism[Cop5[F, A1, A2, A3, A4, A5], F[A5]] {
      def getOption(c: Cop5[F, A1, A2, A3, A4, A5]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(x)))) => Some(x)
        case _                     => None
      }
      def reverseGet(x: F[A5]): Cop5[F, A1, A2, A3, A4, A5] = Cop5[F, A1, A2, A3, A4, A5](\/-(\/-(\/-(\/-(x)))))
    }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5]: Inj[Cop5[F, A1, A2, A3, A4, A5], F[A5]] = Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5]: Inj[Option[F[A5]], Cop5[F, A1, A2, A3, A4, A5]] = Inj.instance(prisma4F.getOption(_))
  }
  object Cop5 extends Cop5LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5]: Prism[Cop5[Id, A1, A2, A3, A4, A5], A1] = prisma0F[Id, A1, A2, A3, A4, A5]

    implicit def inja0Id[A1, A2, A3, A4, A5]: Inj[Cop5[Id, A1, A2, A3, A4, A5], A1] = inja0F[Id, A1, A2, A3, A4, A5]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5]: Inj[Option[A1], Cop5[Id, A1, A2, A3, A4, A5]] = inja0FInverse[Id, A1, A2, A3, A4, A5]

    implicit def prisma1Id[A1, A2, A3, A4, A5]: Prism[Cop5[Id, A1, A2, A3, A4, A5], A2] = prisma1F[Id, A1, A2, A3, A4, A5]

    implicit def inja1Id[A1, A2, A3, A4, A5]: Inj[Cop5[Id, A1, A2, A3, A4, A5], A2] = inja1F[Id, A1, A2, A3, A4, A5]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5]: Inj[Option[A2], Cop5[Id, A1, A2, A3, A4, A5]] = inja1FInverse[Id, A1, A2, A3, A4, A5]

    implicit def prisma2Id[A1, A2, A3, A4, A5]: Prism[Cop5[Id, A1, A2, A3, A4, A5], A3] = prisma2F[Id, A1, A2, A3, A4, A5]

    implicit def inja2Id[A1, A2, A3, A4, A5]: Inj[Cop5[Id, A1, A2, A3, A4, A5], A3] = inja2F[Id, A1, A2, A3, A4, A5]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5]: Inj[Option[A3], Cop5[Id, A1, A2, A3, A4, A5]] = inja2FInverse[Id, A1, A2, A3, A4, A5]

    implicit def prisma3Id[A1, A2, A3, A4, A5]: Prism[Cop5[Id, A1, A2, A3, A4, A5], A4] = prisma3F[Id, A1, A2, A3, A4, A5]

    implicit def inja3Id[A1, A2, A3, A4, A5]: Inj[Cop5[Id, A1, A2, A3, A4, A5], A4] = inja3F[Id, A1, A2, A3, A4, A5]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5]: Inj[Option[A4], Cop5[Id, A1, A2, A3, A4, A5]] = inja3FInverse[Id, A1, A2, A3, A4, A5]

    implicit def prisma4Id[A1, A2, A3, A4, A5]: Prism[Cop5[Id, A1, A2, A3, A4, A5], A5] = prisma4F[Id, A1, A2, A3, A4, A5]

    implicit def inja4Id[A1, A2, A3, A4, A5]: Inj[Cop5[Id, A1, A2, A3, A4, A5], A5] = inja4F[Id, A1, A2, A3, A4, A5]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5]: Inj[Option[A5], Cop5[Id, A1, A2, A3, A4, A5]] = inja4FInverse[Id, A1, A2, A3, A4, A5]
  }

  @newtype case class Prod6[F[_], A1, A2, A3, A4, A5, A6](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6]))
  trait Prod6LP {
    implicit def Prod6Monoid[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6])]): Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]] =
      MF.xmap(M, Prod6[F, A1, A2, A3, A4, A5, A6](_), (_: Prod6[F, A1, A2, A3, A4, A5, A6]).run)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((x, t._2, t._3, t._4, t._5, t._6)))
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[F[A1], Prod6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((t._1, x, t._3, t._4, t._5, t._6)))
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[F[A2], Prod6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((t._1, t._2, x, t._4, t._5, t._6)))
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[F[A3], Prod6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((t._1, t._2, t._3, x, t._5, t._6)))
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[F[A4], Prod6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((t._1, t._2, t._3, t._4, x, t._6)))
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[F[A5], Prod6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[F, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[F, A1, A2, A3, A4, A5, A6], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod6[F, A1, A2, A3, A4, A5, A6]((t._1, t._2, t._3, t._4, t._5, x)))
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[F[A6], Prod6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(_.run._6)
  }
  object Prod6 extends Prod6LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[Id, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[Id, A1, A2, A3, A4, A5, A6], A1] = lifta0F[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6]: Inj[A1, Prod6[Id, A1, A2, A3, A4, A5, A6]] = lifta0FInverse[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[Id, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[Id, A1, A2, A3, A4, A5, A6], A2] = lifta1F[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6]: Inj[A2, Prod6[Id, A1, A2, A3, A4, A5, A6]] = lifta1FInverse[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[Id, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[Id, A1, A2, A3, A4, A5, A6], A3] = lifta2F[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6]: Inj[A3, Prod6[Id, A1, A2, A3, A4, A5, A6]] = lifta2FInverse[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[Id, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[Id, A1, A2, A3, A4, A5, A6], A4] = lifta3F[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6]: Inj[A4, Prod6[Id, A1, A2, A3, A4, A5, A6]] = lifta3FInverse[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[Id, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[Id, A1, A2, A3, A4, A5, A6], A5] = lifta4F[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6]: Inj[A5, Prod6[Id, A1, A2, A3, A4, A5, A6]] = lifta4FInverse[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6](implicit M: Monoid[Prod6[Id, A1, A2, A3, A4, A5, A6]]): Inj[Prod6[Id, A1, A2, A3, A4, A5, A6], A6] = lifta5F[Id, A1, A2, A3, A4, A5, A6]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6]: Inj[A6, Prod6[Id, A1, A2, A3, A4, A5, A6]] = lifta5FInverse[Id, A1, A2, A3, A4, A5, A6]
  }

  @newtype case class Cop6[F[_], A1, A2, A3, A4, A5, A6](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ F[A6]))))))
  trait Cop6LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6]: Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A1]] = new Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A1]] {
      def getOption(c: Cop6[F, A1, A2, A3, A4, A5, A6]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _      => None
      }
      def reverseGet(x: F[A1]): Cop6[F, A1, A2, A3, A4, A5, A6] = Cop6[F, A1, A2, A3, A4, A5, A6](-\/(x))
    }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[F[A1]], Cop6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6]: Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A2]] = new Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A2]] {
      def getOption(c: Cop6[F, A1, A2, A3, A4, A5, A6]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _           => None
      }
      def reverseGet(x: F[A2]): Cop6[F, A1, A2, A3, A4, A5, A6] = Cop6[F, A1, A2, A3, A4, A5, A6](\/-(-\/(x)))
    }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[F[A2]], Cop6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6]: Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A3]] = new Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A3]] {
      def getOption(c: Cop6[F, A1, A2, A3, A4, A5, A6]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _                => None
      }
      def reverseGet(x: F[A3]): Cop6[F, A1, A2, A3, A4, A5, A6] = Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(-\/(x))))
    }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[F[A3]], Cop6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6]: Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A4]] = new Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A4]] {
      def getOption(c: Cop6[F, A1, A2, A3, A4, A5, A6]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _                     => None
      }
      def reverseGet(x: F[A4]): Cop6[F, A1, A2, A3, A4, A5, A6] = Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A4]] = Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[F[A4]], Cop6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6]: Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A5]] = new Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A5]] {
      def getOption(c: Cop6[F, A1, A2, A3, A4, A5, A6]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _                          => None
      }
      def reverseGet(x: F[A5]): Cop6[F, A1, A2, A3, A4, A5, A6] = Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A5]] = Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[F[A5]], Cop6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6]: Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A6]] = new Prism[Cop6[F, A1, A2, A3, A4, A5, A6], F[A6]] {
      def getOption(c: Cop6[F, A1, A2, A3, A4, A5, A6]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(x))))) => Some(x)
        case _                          => None
      }
      def reverseGet(x: F[A6]): Cop6[F, A1, A2, A3, A4, A5, A6] = Cop6[F, A1, A2, A3, A4, A5, A6](\/-(\/-(\/-(\/-(\/-(x))))))
    }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6]: Inj[Cop6[F, A1, A2, A3, A4, A5, A6], F[A6]] = Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[F[A6]], Cop6[F, A1, A2, A3, A4, A5, A6]] = Inj.instance(prisma5F.getOption(_))
  }
  object Cop6 extends Cop6LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6]: Prism[Cop6[Id, A1, A2, A3, A4, A5, A6], A1] = prisma0F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6]: Inj[Cop6[Id, A1, A2, A3, A4, A5, A6], A1] = inja0F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[A1], Cop6[Id, A1, A2, A3, A4, A5, A6]] = inja0FInverse[Id, A1, A2, A3, A4, A5, A6]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6]: Prism[Cop6[Id, A1, A2, A3, A4, A5, A6], A2] = prisma1F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6]: Inj[Cop6[Id, A1, A2, A3, A4, A5, A6], A2] = inja1F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[A2], Cop6[Id, A1, A2, A3, A4, A5, A6]] = inja1FInverse[Id, A1, A2, A3, A4, A5, A6]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6]: Prism[Cop6[Id, A1, A2, A3, A4, A5, A6], A3] = prisma2F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6]: Inj[Cop6[Id, A1, A2, A3, A4, A5, A6], A3] = inja2F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[A3], Cop6[Id, A1, A2, A3, A4, A5, A6]] = inja2FInverse[Id, A1, A2, A3, A4, A5, A6]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6]: Prism[Cop6[Id, A1, A2, A3, A4, A5, A6], A4] = prisma3F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6]: Inj[Cop6[Id, A1, A2, A3, A4, A5, A6], A4] = inja3F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[A4], Cop6[Id, A1, A2, A3, A4, A5, A6]] = inja3FInverse[Id, A1, A2, A3, A4, A5, A6]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6]: Prism[Cop6[Id, A1, A2, A3, A4, A5, A6], A5] = prisma4F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6]: Inj[Cop6[Id, A1, A2, A3, A4, A5, A6], A5] = inja4F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[A5], Cop6[Id, A1, A2, A3, A4, A5, A6]] = inja4FInverse[Id, A1, A2, A3, A4, A5, A6]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6]: Prism[Cop6[Id, A1, A2, A3, A4, A5, A6], A6] = prisma5F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6]: Inj[Cop6[Id, A1, A2, A3, A4, A5, A6], A6] = inja5F[Id, A1, A2, A3, A4, A5, A6]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6]: Inj[Option[A6], Cop6[Id, A1, A2, A3, A4, A5, A6]] = inja5FInverse[Id, A1, A2, A3, A4, A5, A6]
  }

  @newtype case class Prod7[F[_], A1, A2, A3, A4, A5, A6, A7](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7]))
  trait Prod7LP {
    implicit def Prod7Monoid[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7])]): Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]] =
      MF.xmap(M, Prod7[F, A1, A2, A3, A4, A5, A6, A7](_), (_: Prod7[F, A1, A2, A3, A4, A5, A6, A7]).run)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((x, t._2, t._3, t._4, t._5, t._6, t._7)))
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[F[A1], Prod7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t._1, x, t._3, t._4, t._5, t._6, t._7)))
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[F[A2], Prod7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t._1, t._2, x, t._4, t._5, t._6, t._7)))
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[F[A3], Prod7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t._1, t._2, t._3, x, t._5, t._6, t._7)))
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[F[A4], Prod7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t._1, t._2, t._3, t._4, x, t._6, t._7)))
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[F[A5], Prod7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t._1, t._2, t._3, t._4, t._5, x, t._7)))
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[F[A6], Prod7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[F, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[F, A1, A2, A3, A4, A5, A6, A7], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod7[F, A1, A2, A3, A4, A5, A6, A7]((t._1, t._2, t._3, t._4, t._5, t._6, x)))
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[F[A7], Prod7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(_.run._7)
  }
  object Prod7 extends Prod7LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[Id, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7]: Inj[A1, Prod7[Id, A1, A2, A3, A4, A5, A6, A7]] = lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[Id, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7]: Inj[A2, Prod7[Id, A1, A2, A3, A4, A5, A6, A7]] = lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[Id, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7]: Inj[A3, Prod7[Id, A1, A2, A3, A4, A5, A6, A7]] = lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[Id, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7]: Inj[A4, Prod7[Id, A1, A2, A3, A4, A5, A6, A7]] = lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[Id, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7]: Inj[A5, Prod7[Id, A1, A2, A3, A4, A5, A6, A7]] = lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[Id, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7]: Inj[A6, Prod7[Id, A1, A2, A3, A4, A5, A6, A7]] = lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7](implicit M: Monoid[Prod7[Id, A1, A2, A3, A4, A5, A6, A7]]): Inj[Prod7[Id, A1, A2, A3, A4, A5, A6, A7], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7]: Inj[A7, Prod7[Id, A1, A2, A3, A4, A5, A6, A7]] = lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7]
  }

  @newtype case class Cop7[F[_], A1, A2, A3, A4, A5, A6, A7](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ F[A7])))))))
  trait Cop7LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A1]] = new Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A1]] {
      def getOption(c: Cop7[F, A1, A2, A3, A4, A5, A6, A7]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _      => None
      }
      def reverseGet(x: F[A1]): Cop7[F, A1, A2, A3, A4, A5, A6, A7] = Cop7[F, A1, A2, A3, A4, A5, A6, A7](-\/(x))
    }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[F[A1]], Cop7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A2]] = new Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A2]] {
      def getOption(c: Cop7[F, A1, A2, A3, A4, A5, A6, A7]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _           => None
      }
      def reverseGet(x: F[A2]): Cop7[F, A1, A2, A3, A4, A5, A6, A7] = Cop7[F, A1, A2, A3, A4, A5, A6, A7](\/-(-\/(x)))
    }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[F[A2]], Cop7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A3]] = new Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A3]] {
      def getOption(c: Cop7[F, A1, A2, A3, A4, A5, A6, A7]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _                => None
      }
      def reverseGet(x: F[A3]): Cop7[F, A1, A2, A3, A4, A5, A6, A7] = Cop7[F, A1, A2, A3, A4, A5, A6, A7](\/-(\/-(-\/(x))))
    }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[F[A3]], Cop7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A4]] = new Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A4]] {
      def getOption(c: Cop7[F, A1, A2, A3, A4, A5, A6, A7]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _                     => None
      }
      def reverseGet(x: F[A4]): Cop7[F, A1, A2, A3, A4, A5, A6, A7] = Cop7[F, A1, A2, A3, A4, A5, A6, A7](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A4]] = Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[F[A4]], Cop7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A5]] = new Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A5]] {
      def getOption(c: Cop7[F, A1, A2, A3, A4, A5, A6, A7]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _                          => None
      }
      def reverseGet(x: F[A5]): Cop7[F, A1, A2, A3, A4, A5, A6, A7] = Cop7[F, A1, A2, A3, A4, A5, A6, A7](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A5]] = Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[F[A5]], Cop7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A6]] = new Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A6]] {
      def getOption(c: Cop7[F, A1, A2, A3, A4, A5, A6, A7]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _                               => None
      }
      def reverseGet(x: F[A6]): Cop7[F, A1, A2, A3, A4, A5, A6, A7] = Cop7[F, A1, A2, A3, A4, A5, A6, A7](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A6]] = Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[F[A6]], Cop7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A7]] = new Prism[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A7]] {
      def getOption(c: Cop7[F, A1, A2, A3, A4, A5, A6, A7]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(x)))))) => Some(x)
        case _                               => None
      }
      def reverseGet(x: F[A7]): Cop7[F, A1, A2, A3, A4, A5, A6, A7] = Cop7[F, A1, A2, A3, A4, A5, A6, A7](\/-(\/-(\/-(\/-(\/-(\/-(x)))))))
    }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[F, A1, A2, A3, A4, A5, A6, A7], F[A7]] = Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[F[A7]], Cop7[F, A1, A2, A3, A4, A5, A6, A7]] = Inj.instance(prisma6F.getOption(_))
  }
  object Cop7 extends Cop7LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A1] = prisma0F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A1] = inja0F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[A1], Cop7[Id, A1, A2, A3, A4, A5, A6, A7]] = inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A2] = prisma1F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A2] = inja1F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[A2], Cop7[Id, A1, A2, A3, A4, A5, A6, A7]] = inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A3] = prisma2F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A3] = inja2F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[A3], Cop7[Id, A1, A2, A3, A4, A5, A6, A7]] = inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A4] = prisma3F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A4] = inja3F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[A4], Cop7[Id, A1, A2, A3, A4, A5, A6, A7]] = inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A5] = prisma4F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A5] = inja4F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[A5], Cop7[Id, A1, A2, A3, A4, A5, A6, A7]] = inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A6] = prisma5F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A6] = inja5F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[A6], Cop7[Id, A1, A2, A3, A4, A5, A6, A7]] = inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7]: Prism[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A7] = prisma6F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7]: Inj[Cop7[Id, A1, A2, A3, A4, A5, A6, A7], A7] = inja6F[Id, A1, A2, A3, A4, A5, A6, A7]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7]: Inj[Option[A7], Cop7[Id, A1, A2, A3, A4, A5, A6, A7]] = inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7]
  }

  @newtype case class Prod8[F[_], A1, A2, A3, A4, A5, A6, A7, A8](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8]))
  trait Prod8LP {
    implicit def Prod8Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8])]): Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] =
      MF.xmap(M, Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8](_), (_: Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]).run)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8)))
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A1], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8)))
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A2], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8)))
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A3], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8)))
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A4], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8)))
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A5], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8)))
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A6], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8)))
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A7], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._7)

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x)))
    }

    implicit def lifta7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[F[A8], Prod8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(_.run._8)
  }
  object Prod8 extends Prod8LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[A1, Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[A2, Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[A3, Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[A4, Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[A5, Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[A6, Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[A7, Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8](implicit M: Monoid[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]]): Inj[Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A8] =
      lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def lifta7IdInverse[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[A8, Prod8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = lifta7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]
  }

  @newtype case class Cop8[F[_], A1, A2, A3, A4, A5, A6, A7, A8](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ F[A8]))))))))
  trait Cop8LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A1]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A1]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _      => None
      }
      def reverseGet(x: F[A1]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](-\/(x))
    }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A1]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A2]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A2]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _           => None
      }
      def reverseGet(x: F[A2]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](\/-(-\/(x)))
    }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A2]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A3]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A3]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _                => None
      }
      def reverseGet(x: F[A3]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(-\/(x))))
    }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A3]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A4]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A4]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _                     => None
      }
      def reverseGet(x: F[A4]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A4]] = Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A4]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A5]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A5]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _                          => None
      }
      def reverseGet(x: F[A5]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A5]] = Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A5]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A6]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A6]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _                               => None
      }
      def reverseGet(x: F[A6]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A6]] = Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A6]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A7]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A7]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _                                    => None
      }
      def reverseGet(x: F[A7]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A7]] = Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A7]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma6F.getOption(_))

    implicit def prisma7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A8]] = new Prism[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A8]] {
      def getOption(c: Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))) => Some(x)
        case _                                    => None
      }
      def reverseGet(x: F[A8]): Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8] = Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8](\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))
    }

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8], F[A8]] = Inj.instance(prisma7F.reverseGet(_))

    implicit def inja7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[F[A8]], Cop8[F, A1, A2, A3, A4, A5, A6, A7, A8]] = Inj.instance(prisma7F.getOption(_))
  }
  object Cop8 extends Cop8LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A1] = prisma0F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A1] = inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[A1], Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A2] = prisma1F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A2] = inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[A2], Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A3] = prisma2F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A3] = inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[A3], Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A4] = prisma3F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A4] = inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[A4], Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A5] = prisma4F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A5] = inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[A5], Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A6] = prisma5F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A6] = inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[A6], Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A7] = prisma6F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A7] = inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[A7], Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def prisma7Id[A1, A2, A3, A4, A5, A6, A7, A8]: Prism[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A8] = prisma7F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8], A8] = inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8]

    implicit def inja7IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8]: Inj[Option[A8], Cop8[Id, A1, A2, A3, A4, A5, A6, A7, A8]] = inja7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8]
  }

  @newtype case class Prod9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9]))
  trait Prod9LP {
    implicit def Prod9Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](
        implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9])]
    ): Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
      MF.xmap(M, Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](_), (_: Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]).run)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)))
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A1], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9)))
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A2], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9)))
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A3], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9)))
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A4], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9)))
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A5], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9)))
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A6], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9)))
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A7], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._7)

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9)))
    }

    implicit def lifta7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A8], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._8)

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x)))
    }

    implicit def lifta8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[F[A9], Prod9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(_.run._9)
  }
  object Prod9 extends Prod9LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[A1, Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[A2, Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[A3, Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[A4, Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[A5, Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[A6, Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[A7, Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A8] =
      lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta7IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[A8, Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = lifta7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit M: Monoid[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]]): Inj[Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A9] =
      lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def lifta8IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[A9, Prod9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = lifta8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]
  }

  @newtype case class Cop9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ F[A9])))))))))
  trait Cop9LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A1]] = c.run match {
        case -\/(x) => Some(x)
        case _      => None
      }
      def reverseGet(x: F[A1]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](-\/(x))
    }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A1]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A2]] = c.run match {
        case \/-(-\/(x)) => Some(x)
        case _           => None
      }
      def reverseGet(x: F[A2]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(-\/(x)))
    }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A2]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A3]] = c.run match {
        case \/-(\/-(-\/(x))) => Some(x)
        case _                => None
      }
      def reverseGet(x: F[A3]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(-\/(x))))
    }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A3]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A4]] = c.run match {
        case \/-(\/-(\/-(-\/(x)))) => Some(x)
        case _                     => None
      }
      def reverseGet(x: F[A4]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(-\/(x)))))
    }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A4]] = Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A4]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A5]] = c.run match {
        case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
        case _                          => None
      }
      def reverseGet(x: F[A5]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(-\/(x))))))
    }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A5]] = Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A5]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A6]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
        case _                               => None
      }
      def reverseGet(x: F[A6]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
    }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A6]] = Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A6]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A7]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
        case _                                    => None
      }
      def reverseGet(x: F[A7]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
    }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A7]] = Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A7]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma6F.getOption(_))

    implicit def prisma7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A8]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
        case _                                         => None
      }
      def reverseGet(x: F[A8]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
    }

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A8]] = Inj.instance(prisma7F.reverseGet(_))

    implicit def inja7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A8]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma7F.getOption(_))

    implicit def prisma8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] = new Prism[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] {
      def getOption(c: Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]): Option[F[A9]] = c.run match {
        case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))) => Some(x)
        case _                                         => None
      }
      def reverseGet(x: F[A9]): Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9] = Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))
    }

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9], F[A9]] = Inj.instance(prisma8F.reverseGet(_))

    implicit def inja8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[F[A9]], Cop9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = Inj.instance(prisma8F.getOption(_))
  }
  object Cop9 extends Cop9LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A1] = prisma0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A1] = inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[A1], Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A2] = prisma1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A2] = inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[A2], Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A3] = prisma2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A3] = inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[A3], Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A4] = prisma3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A4] = inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[A4], Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A5] = prisma4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A5] = inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[A5], Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A6] = prisma5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A6] = inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[A6], Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A7] = prisma6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A7] = inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[A7], Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def prisma7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A8] = prisma7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A8] = inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja7IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[A8], Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = inja7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def prisma8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Prism[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A9] = prisma8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9], A9] = inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]

    implicit def inja8IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9]: Inj[Option[A9], Cop9[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]] = inja8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9]
  }

  @newtype case class Prod10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10]))
  trait Prod10LP {
    implicit def Prod10Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10])]
    ): Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
      MF.xmap(M, Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](_), (_: Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]).run)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)))
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A1], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)))
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A2], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10)))
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A3], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10)))
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A4], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10)))
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A5], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10)))
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A6], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10)))
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A7], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._7)

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10)))
    }

    implicit def lifta7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A8], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._8)

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10)))
    }

    implicit def lifta8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A9], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._9)

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x)))
    }

    implicit def lifta9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[F[A10], Prod10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(_.run._10)
  }
  object Prod10 extends Prod10LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A1] = lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[A1, Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A2] = lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[A2, Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A3] = lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[A3, Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A4] = lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[A4, Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A5] = lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[A5, Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A6] = lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[A6, Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A7] = lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[A7, Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A8] = lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta7IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[A8, Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = lifta7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A9] = lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta8IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[A9, Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = lifta8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
        implicit M: Monoid[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]]
    ): Inj[Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A10] = lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def lifta9IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[A10, Prod10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = lifta9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  }

  @newtype case class Cop10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ F[A10]))))))))))
  trait Cop10LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A1]] =
      new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A1]] {
        def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A1]] = c.run match {
          case -\/(x) => Some(x)
          case _      => None
        }
        def reverseGet(x: F[A1]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](-\/(x))
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A1]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A2]] =
      new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A2]] {
        def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A2]] = c.run match {
          case \/-(-\/(x)) => Some(x)
          case _           => None
        }
        def reverseGet(x: F[A2]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(-\/(x)))
      }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A2]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A3]] =
      new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A3]] {
        def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A3]] = c.run match {
          case \/-(\/-(-\/(x))) => Some(x)
          case _                => None
        }
        def reverseGet(x: F[A3]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(-\/(x))))
      }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A3]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A4]] =
      new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A4]] {
        def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A4]] = c.run match {
          case \/-(\/-(\/-(-\/(x)))) => Some(x)
          case _                     => None
        }
        def reverseGet(x: F[A4]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(\/-(-\/(x)))))
      }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A4]] = Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A4]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A5]] =
      new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A5]] {
        def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A5]] = c.run match {
          case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
          case _                          => None
        }
        def reverseGet(x: F[A5]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(\/-(\/-(-\/(x))))))
      }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A5]] = Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A5]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A6]] =
      new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A6]] {
        def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A6]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
          case _                               => None
        }
        def reverseGet(x: F[A6]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
      }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A6]] = Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A6]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A7]] =
      new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A7]] {
        def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A7]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
          case _                                    => None
        }
        def reverseGet(x: F[A7]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
      }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A7]] = Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A7]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma6F.getOption(_))

    implicit def prisma7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A8]] =
      new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A8]] {
        def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A8]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
          case _                                         => None
        }
        def reverseGet(x: F[A8]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
      }

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A8]] = Inj.instance(prisma7F.reverseGet(_))

    implicit def inja7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A8]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma7F.getOption(_))

    implicit def prisma8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A9]] =
      new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A9]] {
        def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A9]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
          case _                                              => None
        }
        def reverseGet(x: F[A9]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
      }

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A9]] = Inj.instance(prisma8F.reverseGet(_))

    implicit def inja8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A9]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma8F.getOption(_))

    implicit def prisma9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A10]] =
      new Prism[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A10]] {
        def getOption(c: Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]): Option[F[A10]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))) => Some(x)
          case _                                              => None
        }
        def reverseGet(x: F[A10]): Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))
      }

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], F[A10]] = Inj.instance(prisma9F.reverseGet(_))

    implicit def inja9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[F[A10]], Cop10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] = Inj.instance(prisma9F.getOption(_))
  }
  object Cop10 extends Cop10LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A1] = prisma0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A1] = inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[A1], Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
      inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A2] = prisma1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A2] = inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[A2], Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
      inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A3] = prisma2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A3] = inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[A3], Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
      inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A4] = prisma3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A4] = inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[A4], Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
      inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A5] = prisma4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A5] = inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[A5], Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
      inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A6] = prisma5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A6] = inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[A6], Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
      inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A7] = prisma6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A7] = inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[A7], Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
      inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def prisma7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A8] = prisma7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A8] = inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja7IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[A8], Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
      inja7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def prisma8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A9] = prisma8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A9] = inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja8IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[A9], Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
      inja8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def prisma9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Prism[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A10] = prisma9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], A10] = inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]

    implicit def inja9IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: Inj[Option[A10], Cop10[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
      inja9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]
  }

  @newtype case class Prod11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11]))
  trait Prod11LP {
    implicit def Prod11Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11])]
    ): Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      MF.xmap(M, Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](_), (_: Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]).run)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)))
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A1], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)))
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A2], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)))
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A3], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11)))
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A4], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11)))
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A5], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11)))
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A6], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11)))
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A7], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._7)

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11)))
    }

    implicit def lifta7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A8], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._8)

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11)))
    }

    implicit def lifta8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A9], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._9)

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11)))
    }

    implicit def lifta9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A10], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._10)

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x)))
    }

    implicit def lifta10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[F[A11], Prod11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(_.run._11)
  }
  object Prod11 extends Prod11LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A1] = lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[A1, Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A2] = lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[A2, Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A3] = lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[A3, Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A4] = lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[A4, Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A5] = lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[A5, Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A6] = lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[A6, Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A7] = lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[A7, Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A8] = lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta7IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[A8, Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      lifta7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A9] = lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta8IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[A9, Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      lifta8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A10] = lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta9IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[A10, Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      lifta9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
        implicit M: Monoid[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]]
    ): Inj[Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A11] = lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def lifta10IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[A11, Prod11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      lifta10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
  }

  @newtype case class Cop11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
      run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ F[A11]))))))))))
  )
  trait Cop11LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A1]] =
      new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A1]] {
        def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A1]] = c.run match {
          case -\/(x) => Some(x)
          case _      => None
        }
        def reverseGet(x: F[A1]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](-\/(x))
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A1]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A2]] =
      new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A2]] {
        def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A2]] = c.run match {
          case \/-(-\/(x)) => Some(x)
          case _           => None
        }
        def reverseGet(x: F[A2]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(-\/(x)))
      }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A2]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A3]] =
      new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A3]] {
        def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A3]] = c.run match {
          case \/-(\/-(-\/(x))) => Some(x)
          case _                => None
        }
        def reverseGet(x: F[A3]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(-\/(x))))
      }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A3]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A4]] =
      new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A4]] {
        def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A4]] = c.run match {
          case \/-(\/-(\/-(-\/(x)))) => Some(x)
          case _                     => None
        }
        def reverseGet(x: F[A4]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(-\/(x)))))
      }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A4]] = Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A4]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A5]] =
      new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A5]] {
        def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A5]] = c.run match {
          case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
          case _                          => None
        }
        def reverseGet(x: F[A5]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(-\/(x))))))
      }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A5]] = Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A5]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A6]] =
      new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A6]] {
        def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A6]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
          case _                               => None
        }
        def reverseGet(x: F[A6]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
      }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A6]] = Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A6]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A7]] =
      new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A7]] {
        def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A7]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
          case _                                    => None
        }
        def reverseGet(x: F[A7]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
      }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A7]] = Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A7]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma6F.getOption(_))

    implicit def prisma7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A8]] =
      new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A8]] {
        def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A8]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
          case _                                         => None
        }
        def reverseGet(x: F[A8]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
      }

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A8]] = Inj.instance(prisma7F.reverseGet(_))

    implicit def inja7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A8]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma7F.getOption(_))

    implicit def prisma8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A9]] =
      new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A9]] {
        def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A9]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
          case _                                              => None
        }
        def reverseGet(x: F[A9]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
      }

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A9]] = Inj.instance(prisma8F.reverseGet(_))

    implicit def inja8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A9]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma8F.getOption(_))

    implicit def prisma9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A10]] =
      new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A10]] {
        def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A10]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
          case _                                                   => None
        }
        def reverseGet(x: F[A10]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
      }

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A10]] = Inj.instance(prisma9F.reverseGet(_))

    implicit def inja9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A10]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma9F.getOption(_))

    implicit def prisma10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A11]] =
      new Prism[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A11]] {
        def getOption(c: Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]): Option[F[A11]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))) => Some(x)
          case _                                                   => None
        }
        def reverseGet(x: F[A11]): Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))
      }

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], F[A11]] = Inj.instance(prisma10F.reverseGet(_))

    implicit def inja10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[F[A11]], Cop11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] = Inj.instance(prisma10F.getOption(_))
  }
  object Cop11 extends Cop11LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A1] =
      prisma0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A1] = inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[A1], Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A2] =
      prisma1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A2] = inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[A2], Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A3] =
      prisma2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A3] = inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[A3], Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A4] =
      prisma3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A4] = inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[A4], Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A5] =
      prisma4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A5] = inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[A5], Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A6] =
      prisma5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A6] = inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[A6], Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A7] =
      prisma6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A7] = inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[A7], Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def prisma7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A8] =
      prisma7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A8] = inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja7IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[A8], Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      inja7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def prisma8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A9] =
      prisma8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A9] = inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja8IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[A9], Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      inja8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def prisma9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A10] =
      prisma9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A10] = inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja9IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[A10], Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      inja9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def prisma10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Prism[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A11] =
      prisma10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], A11] = inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]

    implicit def inja10IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: Inj[Option[A11], Cop11[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
      inja10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]
  }

  @newtype case class Prod12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12]))
  trait Prod12LP {
    implicit def Prod12Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12])]
    ): Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      MF.xmap(M, Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](_), (_: Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]).run)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A1], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A2], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A3], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A4], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A5], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A6], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A7], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._7)

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12)))
    }

    implicit def lifta7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A8], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._8)

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12)))
    }

    implicit def lifta8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A9], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._9)

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12)))
    }

    implicit def lifta9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A10], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._10)

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12)))
    }

    implicit def lifta10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A11], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._11)

    implicit def lifta11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A12]] = {
      val t = M.zero.run
      Inj.instance(x => Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x)))
    }

    implicit def lifta11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[F[A12], Prod12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] = Inj.instance(_.run._12)
  }
  object Prod12 extends Prod12LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A1] = lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[A1, Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A2] = lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[A2, Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A3] = lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[A3, Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A4] = lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[A4, Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A5] = lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[A5, Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A6] = lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[A6, Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A7] = lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[A7, Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A8] = lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta7IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[A8, Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      lifta7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A9] = lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta8IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[A9, Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      lifta8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A10] = lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta9IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[A10, Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      lifta9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A11] = lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta10IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[A11, Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      lifta10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
        implicit M: Monoid[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]]
    ): Inj[Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A12] = lifta11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def lifta11IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[A12, Prod12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      lifta11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  }

  @newtype case class Cop12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ F[A12])))))))))))
  )
  trait Cop12LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A1]] =
      new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A1]] {
        def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A1]] = c.run match {
          case -\/(x) => Some(x)
          case _      => None
        }
        def reverseGet(x: F[A1]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](-\/(x))
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A1]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A2]] =
      new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A2]] {
        def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A2]] = c.run match {
          case \/-(-\/(x)) => Some(x)
          case _           => None
        }
        def reverseGet(x: F[A2]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(-\/(x)))
      }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A2]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A3]] =
      new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A3]] {
        def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A3]] = c.run match {
          case \/-(\/-(-\/(x))) => Some(x)
          case _                => None
        }
        def reverseGet(x: F[A3]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(-\/(x))))
      }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A3]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A4]] =
      new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A4]] {
        def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A4]] = c.run match {
          case \/-(\/-(\/-(-\/(x)))) => Some(x)
          case _                     => None
        }
        def reverseGet(x: F[A4]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(-\/(x)))))
      }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A4]] = Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A4]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A5]] =
      new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A5]] {
        def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A5]] = c.run match {
          case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
          case _                          => None
        }
        def reverseGet(x: F[A5]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(-\/(x))))))
      }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A5]] = Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A5]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A6]] =
      new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A6]] {
        def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A6]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
          case _                               => None
        }
        def reverseGet(x: F[A6]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
      }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A6]] = Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A6]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A7]] =
      new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A7]] {
        def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A7]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
          case _                                    => None
        }
        def reverseGet(x: F[A7]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
      }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A7]] = Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A7]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      Inj.instance(prisma6F.getOption(_))

    implicit def prisma7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A8]] =
      new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A8]] {
        def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A8]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
          case _                                         => None
        }
        def reverseGet(x: F[A8]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
      }

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A8]] = Inj.instance(prisma7F.reverseGet(_))

    implicit def inja7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A8]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      Inj.instance(prisma7F.getOption(_))

    implicit def prisma8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A9]] =
      new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A9]] {
        def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A9]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
          case _                                              => None
        }
        def reverseGet(x: F[A9]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
          Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
      }

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A9]] = Inj.instance(prisma8F.reverseGet(_))

    implicit def inja8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A9]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      Inj.instance(prisma8F.getOption(_))

    implicit def prisma9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A10]] =
      new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A10]] {
        def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A10]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
          case _                                                   => None
        }
        def reverseGet(x: F[A10]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
          Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
      }

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A10]] = Inj.instance(prisma9F.reverseGet(_))

    implicit def inja9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A10]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      Inj.instance(prisma9F.getOption(_))

    implicit def prisma10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A11]] =
      new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A11]] {
        def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A11]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
          case _                                                        => None
        }
        def reverseGet(x: F[A11]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
          Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
      }

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A11]] = Inj.instance(prisma10F.reverseGet(_))

    implicit def inja10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A11]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      Inj.instance(prisma10F.getOption(_))

    implicit def prisma11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A12]] =
      new Prism[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A12]] {
        def getOption(c: Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]): Option[F[A12]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))) => Some(x)
          case _                                                        => None
        }
        def reverseGet(x: F[A12]): Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] =
          Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))
      }

    implicit def inja11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], F[A12]] = Inj.instance(prisma11F.reverseGet(_))

    implicit def inja11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[F[A12]], Cop12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      Inj.instance(prisma11F.getOption(_))
  }
  object Cop12 extends Cop12LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A1] =
      prisma0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[A1], Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A2] =
      prisma1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[A2], Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A3] =
      prisma2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[A3], Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A4] =
      prisma3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[A4], Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A5] =
      prisma4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[A5], Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A6] =
      prisma5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[A6], Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A7] =
      prisma6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[A7], Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def prisma7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A8] =
      prisma7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja7IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[A8], Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      inja7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def prisma8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A9] =
      prisma8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja8IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[A9], Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      inja8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def prisma9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A10] =
      prisma9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja9IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[A10], Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      inja9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def prisma10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A11] =
      prisma10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja10IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[A11], Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      inja10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def prisma11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Prism[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A12] =
      prisma11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], A12] =
      inja11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]

    implicit def inja11IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: Inj[Option[A12], Cop12[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
      inja11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]
  }

  @newtype case class Prod13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13]))
  trait Prod13LP {
    implicit def Prod13Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13])]
    ): Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      MF.xmap(M, Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](_), (_: Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]).run)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A1], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A2], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A3], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A4], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A5], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A6], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A7], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._7)

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A8], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._8)

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13)))
    }

    implicit def lifta8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A9], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._9)

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13)))
    }

    implicit def lifta9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A10], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._10)

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13)))
    }

    implicit def lifta10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A11], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._11)

    implicit def lifta11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A12]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13)))
    }

    implicit def lifta11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A12], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._12)

    implicit def lifta12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A13]] = {
      val t = M.zero.run
      Inj.instance(x => Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x)))
    }

    implicit def lifta12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[F[A13], Prod13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] = Inj.instance(_.run._13)
  }
  object Prod13 extends Prod13LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A1] = lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[A1, Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A2] = lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[A2, Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A3] = lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[A3, Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A4] = lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[A4, Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A5] = lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[A5, Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A6] = lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[A6, Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A7] = lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[A7, Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A8] = lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta7IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[A8, Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      lifta7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A9] = lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta8IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[A9, Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      lifta8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A10] = lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta9IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[A10, Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      lifta9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A11] = lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta10IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[A11, Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      lifta10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A12] = lifta11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta11IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[A12, Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      lifta11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
        implicit M: Monoid[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]]
    ): Inj[Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A13] = lifta12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def lifta12IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[A13, Prod13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      lifta12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]
  }

  @newtype case class Cop13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
      run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ F[A13]))))))))))))
  )
  trait Cop13LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A1]] =
      new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A1]] {
        def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A1]] = c.run match {
          case -\/(x) => Some(x)
          case _      => None
        }
        def reverseGet(x: F[A1]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](-\/(x))
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A1]] =
      Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A1]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A2]] =
      new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A2]] {
        def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A2]] = c.run match {
          case \/-(-\/(x)) => Some(x)
          case _           => None
        }
        def reverseGet(x: F[A2]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(-\/(x)))
      }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A2]] =
      Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A2]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A3]] =
      new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A3]] {
        def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A3]] = c.run match {
          case \/-(\/-(-\/(x))) => Some(x)
          case _                => None
        }
        def reverseGet(x: F[A3]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(-\/(x))))
      }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A3]] =
      Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A3]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A4]] =
      new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A4]] {
        def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A4]] = c.run match {
          case \/-(\/-(\/-(-\/(x)))) => Some(x)
          case _                     => None
        }
        def reverseGet(x: F[A4]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(-\/(x)))))
      }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A4]] =
      Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A4]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A5]] =
      new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A5]] {
        def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A5]] = c.run match {
          case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
          case _                          => None
        }
        def reverseGet(x: F[A5]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(-\/(x))))))
      }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A5]] =
      Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A5]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A6]] =
      new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A6]] {
        def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A6]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
          case _                               => None
        }
        def reverseGet(x: F[A6]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
      }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A6]] =
      Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A6]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A7]] =
      new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A7]] {
        def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A7]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
          case _                                    => None
        }
        def reverseGet(x: F[A7]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
          Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
      }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A7]] =
      Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A7]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      Inj.instance(prisma6F.getOption(_))

    implicit def prisma7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A8]] =
      new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A8]] {
        def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A8]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
          case _                                         => None
        }
        def reverseGet(x: F[A8]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
          Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
      }

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A8]] =
      Inj.instance(prisma7F.reverseGet(_))

    implicit def inja7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A8]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      Inj.instance(prisma7F.getOption(_))

    implicit def prisma8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A9]] =
      new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A9]] {
        def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A9]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
          case _                                              => None
        }
        def reverseGet(x: F[A9]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
          Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
      }

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A9]] =
      Inj.instance(prisma8F.reverseGet(_))

    implicit def inja8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A9]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      Inj.instance(prisma8F.getOption(_))

    implicit def prisma9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A10]] =
      new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A10]] {
        def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A10]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
          case _                                                   => None
        }
        def reverseGet(x: F[A10]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
          Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
      }

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A10]] =
      Inj.instance(prisma9F.reverseGet(_))

    implicit def inja9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A10]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      Inj.instance(prisma9F.getOption(_))

    implicit def prisma10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A11]] =
      new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A11]] {
        def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A11]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
          case _                                                        => None
        }
        def reverseGet(x: F[A11]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
          Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
      }

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A11]] =
      Inj.instance(prisma10F.reverseGet(_))

    implicit def inja10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A11]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      Inj.instance(prisma10F.getOption(_))

    implicit def prisma11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A12]] =
      new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A12]] {
        def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A12]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
          case _                                                             => None
        }
        def reverseGet(x: F[A12]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
          Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
      }

    implicit def inja11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A12]] =
      Inj.instance(prisma11F.reverseGet(_))

    implicit def inja11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A12]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      Inj.instance(prisma11F.getOption(_))

    implicit def prisma12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A13]] =
      new Prism[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A13]] {
        def getOption(c: Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]): Option[F[A13]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))) => Some(x)
          case _                                                             => None
        }
        def reverseGet(x: F[A13]): Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] =
          Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))
      }

    implicit def inja12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], F[A13]] =
      Inj.instance(prisma12F.reverseGet(_))

    implicit def inja12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[F[A13]], Cop13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      Inj.instance(prisma12F.getOption(_))
  }
  object Cop13 extends Cop13LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A1] =
      prisma0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[A1], Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A2] =
      prisma1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[A2], Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A3] =
      prisma2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[A3], Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A4] =
      prisma3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[A4], Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A5] =
      prisma4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[A5], Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A6] =
      prisma5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[A6], Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A7] =
      prisma6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[A7], Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def prisma7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A8] =
      prisma7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja7IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[A8], Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      inja7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def prisma8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A9] =
      prisma8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja8IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[A9], Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      inja8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def prisma9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A10] =
      prisma9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja9IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[A10], Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      inja9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def prisma10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A11] =
      prisma10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja10IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[A11], Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      inja10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def prisma11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A12] =
      prisma11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A12] =
      inja11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja11IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[A12], Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      inja11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def prisma12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Prism[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A13] =
      prisma12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], A13] =
      inja12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]

    implicit def inja12IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: Inj[Option[A13], Cop13[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
      inja12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]
  }

  @newtype case class Prod14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
      run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14])
  )
  trait Prod14LP {
    implicit def Prod14Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14])]
    ): Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      MF.xmap(M, Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](_), (_: Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]).run)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A1], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A2], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A3], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A4], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A5], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A6], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A7], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(_.run._7)

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A8], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(_.run._8)

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A9], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(_.run._9)

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14)))
    }

    implicit def lifta9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A10], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(_.run._10)

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14)))
    }

    implicit def lifta10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A11], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(_.run._11)

    implicit def lifta11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A12]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14)))
    }

    implicit def lifta11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A12], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(_.run._12)

    implicit def lifta12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A13]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14)))
    }

    implicit def lifta12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A13], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(_.run._13)

    implicit def lifta13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A14]] = {
      val t = M.zero.run
      Inj.instance(x => Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x)))
    }

    implicit def lifta13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[F[A14], Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(_.run._14)
  }
  object Prod14 extends Prod14LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A1] = lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[A1, Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A2] = lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[A2, Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A3] = lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[A3, Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A4] = lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[A4, Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A5] = lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[A5, Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A6] = lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[A6, Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A7] = lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[A7, Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A8] = lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta7IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[A8, Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      lifta7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A9] = lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta8IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[A9, Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      lifta8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A10] = lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta9IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[A10, Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      lifta9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A11] = lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta10IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[A11, Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      lifta10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A12] = lifta11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta11IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[A12, Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      lifta11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A13] = lifta12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta12IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[A13, Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      lifta12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
        implicit M: Monoid[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]]
    ): Inj[Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A14] = lifta13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def lifta13IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[A14, Prod14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      lifta13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  }

  @newtype case class Cop14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
      run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ F[A14])))))))))))))
  )
  trait Cop14LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A1]] =
      new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A1]] {
        def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A1]] = c.run match {
          case -\/(x) => Some(x)
          case _      => None
        }
        def reverseGet(x: F[A1]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](-\/(x))
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A1]] =
      Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A1]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A2]] =
      new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A2]] {
        def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A2]] = c.run match {
          case \/-(-\/(x)) => Some(x)
          case _           => None
        }
        def reverseGet(x: F[A2]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(-\/(x)))
      }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A2]] =
      Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A2]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A3]] =
      new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A3]] {
        def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A3]] = c.run match {
          case \/-(\/-(-\/(x))) => Some(x)
          case _                => None
        }
        def reverseGet(x: F[A3]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(-\/(x))))
      }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A3]] =
      Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A3]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A4]] =
      new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A4]] {
        def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A4]] = c.run match {
          case \/-(\/-(\/-(-\/(x)))) => Some(x)
          case _                     => None
        }
        def reverseGet(x: F[A4]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(-\/(x)))))
      }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A4]] =
      Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A4]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A5]] =
      new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A5]] {
        def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A5]] = c.run match {
          case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
          case _                          => None
        }
        def reverseGet(x: F[A5]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
          Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(-\/(x))))))
      }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A5]] =
      Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A5]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A6]] =
      new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A6]] {
        def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A6]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
          case _                               => None
        }
        def reverseGet(x: F[A6]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
          Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
      }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A6]] =
      Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A6]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A7]] =
      new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A7]] {
        def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A7]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
          case _                                    => None
        }
        def reverseGet(x: F[A7]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
          Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
      }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A7]] =
      Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A7]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(prisma6F.getOption(_))

    implicit def prisma7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A8]] =
      new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A8]] {
        def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A8]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
          case _                                         => None
        }
        def reverseGet(x: F[A8]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
          Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
      }

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A8]] =
      Inj.instance(prisma7F.reverseGet(_))

    implicit def inja7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A8]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(prisma7F.getOption(_))

    implicit def prisma8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A9]] =
      new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A9]] {
        def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A9]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
          case _                                              => None
        }
        def reverseGet(x: F[A9]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
          Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
      }

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A9]] =
      Inj.instance(prisma8F.reverseGet(_))

    implicit def inja8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A9]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(prisma8F.getOption(_))

    implicit def prisma9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A10]] =
      new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A10]] {
        def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A10]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
          case _                                                   => None
        }
        def reverseGet(x: F[A10]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
          Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
      }

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A10]] =
      Inj.instance(prisma9F.reverseGet(_))

    implicit def inja9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A10]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(prisma9F.getOption(_))

    implicit def prisma10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A11]] =
      new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A11]] {
        def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A11]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
          case _                                                        => None
        }
        def reverseGet(x: F[A11]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
          Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
      }

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A11]] =
      Inj.instance(prisma10F.reverseGet(_))

    implicit def inja10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A11]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(prisma10F.getOption(_))

    implicit def prisma11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A12]] =
      new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A12]] {
        def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A12]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
          case _                                                             => None
        }
        def reverseGet(x: F[A12]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
          Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
      }

    implicit def inja11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A12]] =
      Inj.instance(prisma11F.reverseGet(_))

    implicit def inja11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A12]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(prisma11F.getOption(_))

    implicit def prisma12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A13]] =
      new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A13]] {
        def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A13]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
          case _                                                                  => None
        }
        def reverseGet(x: F[A13]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
          Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
      }

    implicit def inja12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A13]] =
      Inj.instance(prisma12F.reverseGet(_))

    implicit def inja12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A13]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(prisma12F.getOption(_))

    implicit def prisma13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A14]] =
      new Prism[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A14]] {
        def getOption(c: Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]): Option[F[A14]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))) => Some(x)
          case _                                                                  => None
        }
        def reverseGet(x: F[A14]): Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
          Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))
      }

    implicit def inja13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], F[A14]] =
      Inj.instance(prisma13F.reverseGet(_))

    implicit def inja13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[F[A14]], Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      Inj.instance(prisma13F.getOption(_))
  }
  object Cop14 extends Cop14LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A1] =
      prisma0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[A1], Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A2] =
      prisma1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[A2], Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A3] =
      prisma2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[A3], Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A4] =
      prisma3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[A4], Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A5] =
      prisma4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[A5], Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A6] =
      prisma5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[A6], Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A7] =
      prisma6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[A7], Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def prisma7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A8] =
      prisma7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja7IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[A8], Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      inja7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def prisma8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A9] =
      prisma8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja8IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[A9], Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      inja8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def prisma9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A10] =
      prisma9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja9IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[A10], Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      inja9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def prisma10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A11] =
      prisma10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja10IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[A11], Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      inja10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def prisma11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A12] =
      prisma11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A12] =
      inja11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja11IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[A12], Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      inja11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def prisma12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A13] =
      prisma12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A13] =
      inja12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja12IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[A13], Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      inja12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def prisma13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Prism[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A14] =
      prisma13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], A14] =
      inja13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]

    implicit def inja13IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: Inj[Option[A14], Cop14[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
      inja13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  }

  @newtype case class Prod15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
      run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15])
  )
  trait Prod15LP {
    implicit def Prod15Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15])]
    ): Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      MF.xmap(M, Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](_), (_: Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]).run)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A1]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A1], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A2]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A2], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A3]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A3], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A4]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A4], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A5]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A5], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A6]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A6], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A7]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A7], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(_.run._7)

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A8]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A8], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(_.run._8)

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A9]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A9], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(_.run._9)

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A10]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A10], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(_.run._10)

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A11]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15)))
    }

    implicit def lifta10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A11], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(_.run._11)

    implicit def lifta11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A12]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15)))
    }

    implicit def lifta11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A12], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(_.run._12)

    implicit def lifta12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A13]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15)))
    }

    implicit def lifta12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A13], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(_.run._13)

    implicit def lifta13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A14]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15)))
    }

    implicit def lifta13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A14], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(_.run._14)

    implicit def lifta14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A15]] = {
      val t = M.zero.run
      Inj.instance(x => Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x)))
    }

    implicit def lifta14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[F[A15], Prod15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(_.run._15)
  }
  object Prod15 extends Prod15LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A1] = lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[A1, Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A2] = lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[A2, Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A3] = lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[A3, Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A4] = lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[A4, Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A5] = lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[A5, Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A6] = lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[A6, Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A7] = lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[A7, Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A8] = lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta7IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[A8, Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      lifta7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A9] = lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta8IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[A9, Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      lifta8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A10] = lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta9IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[A10, Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      lifta9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A11] = lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta10IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[A11, Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      lifta10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A12] = lifta11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta11IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[A12, Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      lifta11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A13] = lifta12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta12IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[A13, Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      lifta12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A14] = lifta13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta13IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[A14, Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      lifta13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
        implicit M: Monoid[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]]
    ): Inj[Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A15] = lifta14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def lifta14IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[A15, Prod15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      lifta14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
  }

  @newtype case class Cop15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
      run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ F[A15]))))))))))))))
  )
  trait Cop15LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A1]] =
      new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A1]] {
        def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A1]] = c.run match {
          case -\/(x) => Some(x)
          case _      => None
        }
        def reverseGet(x: F[A1]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](-\/(x))
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A1]] =
      Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A1]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A2]] =
      new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A2]] {
        def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A2]] = c.run match {
          case \/-(-\/(x)) => Some(x)
          case _           => None
        }
        def reverseGet(x: F[A2]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(-\/(x)))
      }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A2]] =
      Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A2]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A3]] =
      new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A3]] {
        def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A3]] = c.run match {
          case \/-(\/-(-\/(x))) => Some(x)
          case _                => None
        }
        def reverseGet(x: F[A3]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
          Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(-\/(x))))
      }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A3]] =
      Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A3]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A4]] =
      new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A4]] {
        def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A4]] = c.run match {
          case \/-(\/-(\/-(-\/(x)))) => Some(x)
          case _                     => None
        }
        def reverseGet(x: F[A4]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
          Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(-\/(x)))))
      }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A4]] =
      Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A4]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A5]] =
      new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A5]] {
        def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A5]] = c.run match {
          case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
          case _                          => None
        }
        def reverseGet(x: F[A5]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
          Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(-\/(x))))))
      }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A5]] =
      Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A5]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A6]] =
      new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A6]] {
        def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A6]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
          case _                               => None
        }
        def reverseGet(x: F[A6]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
          Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
      }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A6]] =
      Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A6]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A7]] =
      new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A7]] {
        def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A7]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
          case _                                    => None
        }
        def reverseGet(x: F[A7]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
          Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
      }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A7]] =
      Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A7]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(prisma6F.getOption(_))

    implicit def prisma7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A8]] =
      new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A8]] {
        def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A8]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
          case _                                         => None
        }
        def reverseGet(x: F[A8]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
          Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
      }

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A8]] =
      Inj.instance(prisma7F.reverseGet(_))

    implicit def inja7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A8]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(prisma7F.getOption(_))

    implicit def prisma8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A9]] =
      new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A9]] {
        def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A9]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
          case _                                              => None
        }
        def reverseGet(x: F[A9]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
          Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
      }

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A9]] =
      Inj.instance(prisma8F.reverseGet(_))

    implicit def inja8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[F[A9]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      Inj.instance(prisma8F.getOption(_))

    implicit def prisma9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A10]] =
      new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A10]] {
        def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A10]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
          case _                                                   => None
        }
        def reverseGet(x: F[A10]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
          Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
      }

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A10]] =
      Inj.instance(prisma9F.reverseGet(_))

    implicit def inja9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
        : Inj[Option[F[A10]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma9F.getOption(_))

    implicit def prisma10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A11]] =
      new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A11]] {
        def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A11]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
          case _                                                        => None
        }
        def reverseGet(x: F[A11]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
          Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
      }

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A11]] =
      Inj.instance(prisma10F.reverseGet(_))

    implicit def inja10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
        : Inj[Option[F[A11]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma10F.getOption(_))

    implicit def prisma11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A12]] =
      new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A12]] {
        def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A12]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
          case _                                                             => None
        }
        def reverseGet(x: F[A12]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
          Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
      }

    implicit def inja11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A12]] =
      Inj.instance(prisma11F.reverseGet(_))

    implicit def inja11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
        : Inj[Option[F[A12]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma11F.getOption(_))

    implicit def prisma12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A13]] =
      new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A13]] {
        def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A13]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
          case _                                                                  => None
        }
        def reverseGet(x: F[A13]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
          Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
      }

    implicit def inja12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A13]] =
      Inj.instance(prisma12F.reverseGet(_))

    implicit def inja12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
        : Inj[Option[F[A13]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma12F.getOption(_))

    implicit def prisma13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A14]] =
      new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A14]] {
        def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A14]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
          case _                                                                       => None
        }
        def reverseGet(x: F[A14]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
          Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
      }

    implicit def inja13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A14]] =
      Inj.instance(prisma13F.reverseGet(_))

    implicit def inja13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
        : Inj[Option[F[A14]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma13F.getOption(_))

    implicit def prisma14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A15]] =
      new Prism[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A15]] {
        def getOption(c: Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]): Option[F[A15]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))) => Some(x)
          case _                                                                       => None
        }
        def reverseGet(x: F[A15]): Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] =
          Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))
      }

    implicit def inja14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], F[A15]] =
      Inj.instance(prisma14F.reverseGet(_))

    implicit def inja14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
        : Inj[Option[F[A15]], Cop15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = Inj.instance(prisma14F.getOption(_))
  }
  object Cop15 extends Cop15LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A1] =
      prisma0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[A1], Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A2] =
      prisma1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[A2], Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A3] =
      prisma2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[A3], Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A4] =
      prisma3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[A4], Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A5] =
      prisma4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[A5], Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A6] =
      prisma5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[A6], Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A7] =
      prisma6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[A7], Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def prisma7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A8] =
      prisma7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja7IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[A8], Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      inja7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def prisma8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A9] =
      prisma8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja8IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[A9], Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      inja8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def prisma9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A10] =
      prisma9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja9IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Option[A10], Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
      inja9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def prisma10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A11] =
      prisma10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja10IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
        : Inj[Option[A11], Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = inja10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def prisma11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A12] =
      prisma11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A12] =
      inja11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja11IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
        : Inj[Option[A12], Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = inja11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def prisma12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A13] =
      prisma12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A13] =
      inja12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja12IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
        : Inj[Option[A13], Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = inja12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def prisma13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A14] =
      prisma13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A14] =
      inja13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja13IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
        : Inj[Option[A14], Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = inja13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def prisma14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Prism[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A15] =
      prisma14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: Inj[Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], A15] =
      inja14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]

    implicit def inja14IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
        : Inj[Option[A15], Cop15[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] = inja14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]
  }

  @newtype case class Prod16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
      run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16])
  )
  trait Prod16LP {
    implicit def Prod16Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16])]
    ): Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      MF.xmap(M, Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](_), (_: Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]).run)

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A1]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16))
      )
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A1], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A2]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16))
      )
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A2], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A3]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16))
      )
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A3], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A4]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16))
      )
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A4], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A5]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16))
      )
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A5], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A6]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16))
      )
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A6], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A7]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16))
      )
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A7], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._7)

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A8]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16))
      )
    }

    implicit def lifta7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A8], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._8)

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A9]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15, t._16))
      )
    }

    implicit def lifta8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A9], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._9)

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A10]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15, t._16))
      )
    }

    implicit def lifta9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A10], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._10)

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A11]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15, t._16))
      )
    }

    implicit def lifta10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A11], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._11)

    implicit def lifta11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A12]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15, t._16))
      )
    }

    implicit def lifta11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A12], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._12)

    implicit def lifta12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A13]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15, t._16))
      )
    }

    implicit def lifta12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A13], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._13)

    implicit def lifta13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A14]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15, t._16))
      )
    }

    implicit def lifta13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A14], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._14)

    implicit def lifta14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A15]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x, t._16))
      )
    }

    implicit def lifta14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A15], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._15)

    implicit def lifta15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A16]] = {
      val t = M.zero.run
      Inj.instance(
        x => Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, x))
      )
    }

    implicit def lifta15FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[F[A16], Prod16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(_.run._16)
  }
  object Prod16 extends Prod16LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A1] = lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A1, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A2] = lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A2, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A3] = lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A3, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A4] = lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A4, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A5] = lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A5, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A6] = lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A6, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A7] = lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A7, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A8] = lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta7IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A8, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A9] = lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta8IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A9, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A10] = lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta9IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A10, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A11] = lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta10IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A11, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A12] = lifta11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta11IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A12, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A13] = lifta12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta12IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A13, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A14] = lifta13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta13IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A14, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A15] = lifta14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta14IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A15, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
        implicit M: Monoid[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]]
    ): Inj[Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A16] = lifta15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def lifta15IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[A16, Prod16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
      lifta15FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
  }

  @newtype case class Cop16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
      run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ F[A16])))))))))))))))
  )
  trait Cop16LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A1]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A1]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A1]] = c.run match {
          case -\/(x) => Some(x)
          case _      => None
        }
        def reverseGet(x: F[A1]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](-\/(x))
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A1]] =
      Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A1]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A2]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A2]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A2]] = c.run match {
          case \/-(-\/(x)) => Some(x)
          case _           => None
        }
        def reverseGet(x: F[A2]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(-\/(x)))
      }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A2]] =
      Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A2]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A3]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A3]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A3]] = c.run match {
          case \/-(\/-(-\/(x))) => Some(x)
          case _                => None
        }
        def reverseGet(x: F[A3]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(-\/(x))))
      }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A3]] =
      Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A3]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A4]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A4]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A4]] = c.run match {
          case \/-(\/-(\/-(-\/(x)))) => Some(x)
          case _                     => None
        }
        def reverseGet(x: F[A4]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(-\/(x)))))
      }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A4]] =
      Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A4]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A5]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A5]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A5]] = c.run match {
          case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
          case _                          => None
        }
        def reverseGet(x: F[A5]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(-\/(x))))))
      }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A5]] =
      Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A5]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A6]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A6]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A6]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
          case _                               => None
        }
        def reverseGet(x: F[A6]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
      }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A6]] =
      Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A6]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A7]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A7]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A7]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
          case _                                    => None
        }
        def reverseGet(x: F[A7]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
      }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A7]] =
      Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A7]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma6F.getOption(_))

    implicit def prisma7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A8]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A8]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A8]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
          case _                                         => None
        }
        def reverseGet(x: F[A8]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
      }

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A8]] =
      Inj.instance(prisma7F.reverseGet(_))

    implicit def inja7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A8]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma7F.getOption(_))

    implicit def prisma8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A9]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A9]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A9]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
          case _                                              => None
        }
        def reverseGet(x: F[A9]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
      }

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A9]] =
      Inj.instance(prisma8F.reverseGet(_))

    implicit def inja8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A9]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma8F.getOption(_))

    implicit def prisma9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A10]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A10]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A10]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
          case _                                                   => None
        }
        def reverseGet(x: F[A10]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
      }

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A10]] =
      Inj.instance(prisma9F.reverseGet(_))

    implicit def inja9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A10]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma9F.getOption(_))

    implicit def prisma10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A11]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A11]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A11]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
          case _                                                        => None
        }
        def reverseGet(x: F[A11]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
      }

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A11]] =
      Inj.instance(prisma10F.reverseGet(_))

    implicit def inja10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A11]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma10F.getOption(_))

    implicit def prisma11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A12]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A12]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A12]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
          case _                                                             => None
        }
        def reverseGet(x: F[A12]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
      }

    implicit def inja11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A12]] =
      Inj.instance(prisma11F.reverseGet(_))

    implicit def inja11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A12]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma11F.getOption(_))

    implicit def prisma12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A13]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A13]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A13]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
          case _                                                                  => None
        }
        def reverseGet(x: F[A13]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
      }

    implicit def inja12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A13]] =
      Inj.instance(prisma12F.reverseGet(_))

    implicit def inja12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A13]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma12F.getOption(_))

    implicit def prisma13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A14]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A14]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A14]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
          case _                                                                       => None
        }
        def reverseGet(x: F[A14]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
      }

    implicit def inja13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A14]] =
      Inj.instance(prisma13F.reverseGet(_))

    implicit def inja13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A14]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma13F.getOption(_))

    implicit def prisma14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A15]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A15]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A15]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
          case _                                                                            => None
        }
        def reverseGet(x: F[A15]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
      }

    implicit def inja14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A15]] =
      Inj.instance(prisma14F.reverseGet(_))

    implicit def inja14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A15]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma14F.getOption(_))

    implicit def prisma15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A16]] =
      new Prism[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A16]] {
        def getOption(c: Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]): Option[F[A16]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))) => Some(x)
          case _                                                                            => None
        }
        def reverseGet(x: F[A16]): Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] =
          Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))
      }

    implicit def inja15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], F[A16]] =
      Inj.instance(prisma15F.reverseGet(_))

    implicit def inja15FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[F[A16]], Cop16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = Inj.instance(prisma15F.getOption(_))
  }
  object Cop16 extends Cop16LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A1] =
      prisma0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A1], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A2] =
      prisma1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A2], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A3] =
      prisma2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A3], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A4] =
      prisma3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A4], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A5] =
      prisma4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A5], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A6] =
      prisma5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A6], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A7] =
      prisma6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A7], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def prisma7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A8] =
      prisma7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja7IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A8], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def prisma8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A9] =
      prisma8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja8IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A9], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def prisma9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A10] =
      prisma9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja9IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A10], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def prisma10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A11] =
      prisma10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja10IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A11], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def prisma11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A12] =
      prisma11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A12] =
      inja11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja11IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A12], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def prisma12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A13] =
      prisma12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A13] =
      inja12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja12IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A13], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def prisma13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A14] =
      prisma13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A14] =
      inja13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja13IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A14], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def prisma14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A15] =
      prisma14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A15] =
      inja14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja14IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A15], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def prisma15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Prism[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A16] =
      prisma15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: Inj[Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], A16] =
      inja15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]

    implicit def inja15IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
        : Inj[Option[A16], Cop16[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] = inja15FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
  }

  @newtype case class Prod17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
      run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17])
  )
  trait Prod17LP {
    implicit def Prod17Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17])]
    ): Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      MF.xmap(
        M,
        Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](_),
        (_: Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]).run
      )

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A1]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
          )
      )
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A1], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A2]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
          )
      )
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A2], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A3]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
          )
      )
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A3], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A4]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
          )
      )
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A4], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A5]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
          )
      )
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A5], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A6]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
          )
      )
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A6], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A7]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
          )
      )
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A7], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._7)

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A8]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
          )
      )
    }

    implicit def lifta7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A8], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._8)

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A9]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
          )
      )
    }

    implicit def lifta8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A9], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._9)

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A10]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
          )
      )
    }

    implicit def lifta9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A10], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._10)

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A11]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15, t._16, t._17)
          )
      )
    }

    implicit def lifta10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A11], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._11)

    implicit def lifta11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A12]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15, t._16, t._17)
          )
      )
    }

    implicit def lifta11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A12], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._12)

    implicit def lifta12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A13]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15, t._16, t._17)
          )
      )
    }

    implicit def lifta12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A13], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._13)

    implicit def lifta13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A14]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15, t._16, t._17)
          )
      )
    }

    implicit def lifta13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A14], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._14)

    implicit def lifta14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A15]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x, t._16, t._17)
          )
      )
    }

    implicit def lifta14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A15], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._15)

    implicit def lifta15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A16]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, x, t._17)
          )
      )
    }

    implicit def lifta15FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A16], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._16)

    implicit def lifta16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A17]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, x)
          )
      )
    }

    implicit def lifta16FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[F[A17], Prod17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(_.run._17)
  }
  object Prod17 extends Prod17LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A1] = lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A1, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A2] = lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A2, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A3] = lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A3, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A4] = lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A4, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A5] = lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A5, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A6] = lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A6, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A7] = lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A7, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A8] = lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta7IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A8, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A9] = lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta8IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A9, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A10] = lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta9IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A10, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A11] = lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta10IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A11, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A12] = lifta11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta11IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A12, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A13] = lifta12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta12IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A13, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A14] = lifta13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta13IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A14, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A15] = lifta14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta14IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A15, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A16] = lifta15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta15IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A16, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta15FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
        implicit M: Monoid[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]]
    ): Inj[Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A17] = lifta16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def lifta16IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[A17, Prod17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      lifta16FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
  }

  @newtype case class Cop17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
      run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ F[A17]))))))))))))))))
  )
  trait Cop17LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A1]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A1]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A1]] = c.run match {
          case -\/(x) => Some(x)
          case _      => None
        }
        def reverseGet(x: F[A1]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](-\/(x))
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A1]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A2]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A2]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A2]] = c.run match {
          case \/-(-\/(x)) => Some(x)
          case _           => None
        }
        def reverseGet(x: F[A2]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(-\/(x)))
      }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A2]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A3]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A3]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A3]] = c.run match {
          case \/-(\/-(-\/(x))) => Some(x)
          case _                => None
        }
        def reverseGet(x: F[A3]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(-\/(x))))
      }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A3]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A4]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A4]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A4]] = c.run match {
          case \/-(\/-(\/-(-\/(x)))) => Some(x)
          case _                     => None
        }
        def reverseGet(x: F[A4]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(-\/(x)))))
      }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A4]] = Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A4]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A5]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A5]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A5]] = c.run match {
          case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
          case _                          => None
        }
        def reverseGet(x: F[A5]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(-\/(x))))))
      }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A5]] = Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A5]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A6]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A6]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A6]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
          case _                               => None
        }
        def reverseGet(x: F[A6]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
      }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A6]] = Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A6]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A7]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A7]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A7]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
          case _                                    => None
        }
        def reverseGet(x: F[A7]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
      }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A7]] = Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A7]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma6F.getOption(_))

    implicit def prisma7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A8]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A8]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A8]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
          case _                                         => None
        }
        def reverseGet(x: F[A8]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
      }

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A8]] = Inj.instance(prisma7F.reverseGet(_))

    implicit def inja7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A8]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma7F.getOption(_))

    implicit def prisma8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A9]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A9]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A9]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
          case _                                              => None
        }
        def reverseGet(x: F[A9]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
      }

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A9]] = Inj.instance(prisma8F.reverseGet(_))

    implicit def inja8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A9]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma8F.getOption(_))

    implicit def prisma9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A10]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A10]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A10]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
          case _                                                   => None
        }
        def reverseGet(x: F[A10]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
      }

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A10]] = Inj.instance(prisma9F.reverseGet(_))

    implicit def inja9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A10]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma9F.getOption(_))

    implicit def prisma10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A11]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A11]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A11]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
          case _                                                        => None
        }
        def reverseGet(x: F[A11]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
      }

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A11]] = Inj.instance(prisma10F.reverseGet(_))

    implicit def inja10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A11]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma10F.getOption(_))

    implicit def prisma11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A12]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A12]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A12]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
          case _                                                             => None
        }
        def reverseGet(x: F[A12]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
      }

    implicit def inja11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A12]] = Inj.instance(prisma11F.reverseGet(_))

    implicit def inja11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A12]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma11F.getOption(_))

    implicit def prisma12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A13]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A13]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A13]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
          case _                                                                  => None
        }
        def reverseGet(x: F[A13]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
      }

    implicit def inja12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A13]] = Inj.instance(prisma12F.reverseGet(_))

    implicit def inja12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A13]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma12F.getOption(_))

    implicit def prisma13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A14]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A14]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A14]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
          case _                                                                       => None
        }
        def reverseGet(x: F[A14]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
      }

    implicit def inja13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A14]] = Inj.instance(prisma13F.reverseGet(_))

    implicit def inja13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A14]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma13F.getOption(_))

    implicit def prisma14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A15]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A15]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A15]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
          case _                                                                            => None
        }
        def reverseGet(x: F[A15]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
      }

    implicit def inja14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A15]] = Inj.instance(prisma14F.reverseGet(_))

    implicit def inja14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A15]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma14F.getOption(_))

    implicit def prisma15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A16]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A16]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A16]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Some(x)
          case _                                                                                 => None
        }
        def reverseGet(x: F[A16]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))
      }

    implicit def inja15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A16]] = Inj.instance(prisma15F.reverseGet(_))

    implicit def inja15FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A16]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma15F.getOption(_))

    implicit def prisma16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A17]] =
      new Prism[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A17]] {
        def getOption(c: Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]): Option[F[A17]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))) => Some(x)
          case _                                                                                 => None
        }
        def reverseGet(x: F[A17]): Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
          Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))
      }

    implicit def inja16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], F[A17]] = Inj.instance(prisma16F.reverseGet(_))

    implicit def inja16FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[F[A17]], Cop17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] = Inj.instance(prisma16F.getOption(_))
  }
  object Cop17 extends Cop17LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A1] = prisma0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A1], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A2] = prisma1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A2], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A3] = prisma2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A3], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A4] = prisma3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A4], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A5] = prisma4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A5], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A6] = prisma5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A6], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A7] = prisma6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A7], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A8] = prisma7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja7IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A8], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A9] = prisma8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja8IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A9], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A10] = prisma9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja9IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A10], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A11] = prisma10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja10IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A11], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A12] = prisma11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A12] =
      inja11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja11IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A12], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A13] = prisma12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A13] =
      inja12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja12IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A13], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A14] = prisma13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A14] =
      inja13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja13IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A14], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A15] = prisma14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A15] =
      inja14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja14IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A15], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A16] = prisma15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A16] =
      inja15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja15IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A16], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja15FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def prisma16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Prism[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A17] = prisma16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: Inj[Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], A17] =
      inja16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]

    implicit def inja16IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
        : Inj[Option[A17], Cop17[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
      inja16FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
  }

  @newtype case class Prod18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
      run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18])
  )
  trait Prod18LP {
    implicit def Prod18Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18])]
    ): Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      MF.xmap(
        M,
        Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](_),
        (_: Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]).run
      )

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A1]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
          )
      )
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A1], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A2]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
          )
      )
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A2], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A3]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
          )
      )
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A3], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A4]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
          )
      )
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A4], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A5]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
          )
      )
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A5], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A6]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
          )
      )
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A6], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A7]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
          )
      )
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A7], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._7)

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A8]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
          )
      )
    }

    implicit def lifta7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A8], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._8)

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A9]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
          )
      )
    }

    implicit def lifta8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A9], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._9)

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A10]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
          )
      )
    }

    implicit def lifta9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A10], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._10)

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A11]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
          )
      )
    }

    implicit def lifta10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A11], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._11)

    implicit def lifta11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A12]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15, t._16, t._17, t._18)
          )
      )
    }

    implicit def lifta11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A12], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._12)

    implicit def lifta12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A13]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15, t._16, t._17, t._18)
          )
      )
    }

    implicit def lifta12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A13], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._13)

    implicit def lifta13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A14]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15, t._16, t._17, t._18)
          )
      )
    }

    implicit def lifta13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A14], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._14)

    implicit def lifta14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A15]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x, t._16, t._17, t._18)
          )
      )
    }

    implicit def lifta14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A15], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._15)

    implicit def lifta15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A16]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, x, t._17, t._18)
          )
      )
    }

    implicit def lifta15FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A16], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._16)

    implicit def lifta16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A17]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, x, t._18)
          )
      )
    }

    implicit def lifta16FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A17], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._17)

    implicit def lifta17F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A18]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, x)
          )
      )
    }

    implicit def lifta17FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[F[A18], Prod18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(_.run._18)
  }
  object Prod18 extends Prod18LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A1, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A2, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A3, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A4, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A5, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A6, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A7, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A8] =
      lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta7IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A8, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A9] =
      lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta8IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A9, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A10] =
      lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta9IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A10, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A11] =
      lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta10IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A11, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A12] =
      lifta11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta11IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A12, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A13] =
      lifta12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta12IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A13, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A14] =
      lifta13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta13IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A14, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A15] =
      lifta14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta14IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A15, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A16] =
      lifta15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta15IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A16, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta15FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A17] =
      lifta16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta16IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A17, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta16FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta17Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
        implicit M: Monoid[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]]
    ): Inj[Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A18] =
      lifta17F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def lifta17IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[A18, Prod18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      lifta17FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
  }

  @newtype case class Cop18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
      run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ F[
        A18
      ])))))))))))))))))
  )
  trait Cop18LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A1]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A1]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A1]] = c.run match {
          case -\/(x) => Some(x)
          case _      => None
        }
        def reverseGet(x: F[A1]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](-\/(x))
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A1]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A2]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A2]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A2]] = c.run match {
          case \/-(-\/(x)) => Some(x)
          case _           => None
        }
        def reverseGet(x: F[A2]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(-\/(x)))
      }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A2]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A3]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A3]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A3]] = c.run match {
          case \/-(\/-(-\/(x))) => Some(x)
          case _                => None
        }
        def reverseGet(x: F[A3]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(-\/(x))))
      }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A3]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A4]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A4]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A4]] = c.run match {
          case \/-(\/-(\/-(-\/(x)))) => Some(x)
          case _                     => None
        }
        def reverseGet(x: F[A4]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(-\/(x)))))
      }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A4]] = Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A4]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A5]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A5]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A5]] = c.run match {
          case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
          case _                          => None
        }
        def reverseGet(x: F[A5]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(-\/(x))))))
      }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A5]] = Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A5]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A6]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A6]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A6]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
          case _                               => None
        }
        def reverseGet(x: F[A6]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
      }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A6]] = Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A6]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A7]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A7]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A7]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
          case _                                    => None
        }
        def reverseGet(x: F[A7]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
      }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A7]] = Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A7]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma6F.getOption(_))

    implicit def prisma7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A8]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A8]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A8]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
          case _                                         => None
        }
        def reverseGet(x: F[A8]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
      }

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A8]] = Inj.instance(prisma7F.reverseGet(_))

    implicit def inja7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A8]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma7F.getOption(_))

    implicit def prisma8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A9]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A9]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A9]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
          case _                                              => None
        }
        def reverseGet(x: F[A9]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
      }

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A9]] = Inj.instance(prisma8F.reverseGet(_))

    implicit def inja8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A9]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma8F.getOption(_))

    implicit def prisma9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A10]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A10]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A10]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
          case _                                                   => None
        }
        def reverseGet(x: F[A10]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
      }

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A10]] = Inj.instance(prisma9F.reverseGet(_))

    implicit def inja9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A10]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma9F.getOption(_))

    implicit def prisma10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A11]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A11]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A11]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
          case _                                                        => None
        }
        def reverseGet(x: F[A11]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
      }

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A11]] = Inj.instance(prisma10F.reverseGet(_))

    implicit def inja10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A11]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma10F.getOption(_))

    implicit def prisma11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A12]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A12]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A12]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
          case _                                                             => None
        }
        def reverseGet(x: F[A12]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
      }

    implicit def inja11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A12]] = Inj.instance(prisma11F.reverseGet(_))

    implicit def inja11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A12]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma11F.getOption(_))

    implicit def prisma12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A13]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A13]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A13]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
          case _                                                                  => None
        }
        def reverseGet(x: F[A13]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
      }

    implicit def inja12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A13]] = Inj.instance(prisma12F.reverseGet(_))

    implicit def inja12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A13]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma12F.getOption(_))

    implicit def prisma13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A14]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A14]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A14]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
          case _                                                                       => None
        }
        def reverseGet(x: F[A14]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
      }

    implicit def inja13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A14]] = Inj.instance(prisma13F.reverseGet(_))

    implicit def inja13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A14]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma13F.getOption(_))

    implicit def prisma14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A15]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A15]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A15]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
          case _                                                                            => None
        }
        def reverseGet(x: F[A15]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
      }

    implicit def inja14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A15]] = Inj.instance(prisma14F.reverseGet(_))

    implicit def inja14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A15]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma14F.getOption(_))

    implicit def prisma15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A16]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A16]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A16]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Some(x)
          case _                                                                                 => None
        }
        def reverseGet(x: F[A16]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))
      }

    implicit def inja15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A16]] = Inj.instance(prisma15F.reverseGet(_))

    implicit def inja15FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A16]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma15F.getOption(_))

    implicit def prisma16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A17]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A17]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A17]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => Some(x)
          case _                                                                                      => None
        }
        def reverseGet(x: F[A17]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))
      }

    implicit def inja16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A17]] = Inj.instance(prisma16F.reverseGet(_))

    implicit def inja16FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A17]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma16F.getOption(_))

    implicit def prisma17F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A18]] =
      new Prism[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A18]] {
        def getOption(c: Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]): Option[F[A18]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))) => Some(x)
          case _                                                                                      => None
        }
        def reverseGet(x: F[A18]): Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
          Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))))
      }

    implicit def inja17F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], F[A18]] = Inj.instance(prisma17F.reverseGet(_))

    implicit def inja17FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[F[A18]], Cop18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] = Inj.instance(prisma17F.getOption(_))
  }
  object Cop18 extends Cop18LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A1] =
      prisma0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A1], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A2] =
      prisma1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A2], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A3] =
      prisma2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A3], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A4] =
      prisma3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A4], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A5] =
      prisma4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A5], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A6] =
      prisma5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A6], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A7] =
      prisma6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A7], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A8] =
      prisma7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja7IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A8], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A9] =
      prisma8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja8IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A9], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A10] =
      prisma9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja9IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A10], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A11] =
      prisma10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja10IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A11], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A12] =
      prisma11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A12] =
      inja11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja11IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A12], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A13] =
      prisma12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A13] =
      inja12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja12IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A13], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A14] =
      prisma13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A14] =
      inja13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja13IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A14], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A15] =
      prisma14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A15] =
      inja14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja14IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A15], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A16] =
      prisma15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A16] =
      inja15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja15IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A16], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja15FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A17] =
      prisma16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A17] =
      inja16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja16IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A17], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja16FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def prisma17Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Prism[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A18] =
      prisma17F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja17Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], A18] =
      inja17F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]

    implicit def inja17IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
        : Inj[Option[A18], Cop18[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
      inja17FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
  }

  @newtype case class Prod19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
      run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19])
  )
  trait Prod19LP {
    implicit def Prod19Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19])]
    ): Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      MF.xmap(
        M,
        Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](_),
        (_: Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]).run
      )

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A1]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A1], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A2]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A2], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A3]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A3], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A4]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A4], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A5]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A5], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A6]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A6], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A7]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A7], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._7)

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A8]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A8], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._8)

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A9]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A9], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._9)

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A10]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A10], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._10)

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A11]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A11], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._11)

    implicit def lifta11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A12]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A12], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._12)

    implicit def lifta12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A13]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15, t._16, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A13], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._13)

    implicit def lifta13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A14]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15, t._16, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A14], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._14)

    implicit def lifta14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A15]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x, t._16, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A15], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._15)

    implicit def lifta15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A16]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, x, t._17, t._18, t._19)
          )
      )
    }

    implicit def lifta15FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A16], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._16)

    implicit def lifta16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A17]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, x, t._18, t._19)
          )
      )
    }

    implicit def lifta16FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A17], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._17)

    implicit def lifta17F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A18]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, x, t._19)
          )
      )
    }

    implicit def lifta17FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A18], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._18)

    implicit def lifta18F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A19]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, x)
          )
      )
    }

    implicit def lifta18FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[F[A19], Prod19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(_.run._19)
  }
  object Prod19 extends Prod19LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A1, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A2, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A3, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A4, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A5, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A6, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A7, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A8] =
      lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta7IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A8, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A9] =
      lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta8IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A9, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A10] =
      lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta9IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A10, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A11] =
      lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta10IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A11, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A12] =
      lifta11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta11IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A12, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A13] =
      lifta12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta12IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A13, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A14] =
      lifta13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta13IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A14, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A15] =
      lifta14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta14IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A15, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A16] =
      lifta15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta15IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A16, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta15FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A17] =
      lifta16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta16IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A17, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta16FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta17Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A18] =
      lifta17F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta17IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A18, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta17FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta18Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
        implicit M: Monoid[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]]
    ): Inj[Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A19] =
      lifta18F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def lifta18IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[A19, Prod19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      lifta18FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
  }

  @newtype case class Cop19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
      run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ F[
        A19
      ]))))))))))))))))))
  )
  trait Cop19LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A1]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A1]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A1]] = c.run match {
          case -\/(x) => Some(x)
          case _      => None
        }
        def reverseGet(x: F[A1]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](-\/(x))
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A1]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A2]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A2]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A2]] = c.run match {
          case \/-(-\/(x)) => Some(x)
          case _           => None
        }
        def reverseGet(x: F[A2]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(-\/(x)))
      }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A2]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A3]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A3]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A3]] = c.run match {
          case \/-(\/-(-\/(x))) => Some(x)
          case _                => None
        }
        def reverseGet(x: F[A3]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(-\/(x))))
      }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A3]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A4]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A4]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A4]] = c.run match {
          case \/-(\/-(\/-(-\/(x)))) => Some(x)
          case _                     => None
        }
        def reverseGet(x: F[A4]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(-\/(x)))))
      }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A4]] = Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A4]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A5]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A5]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A5]] = c.run match {
          case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
          case _                          => None
        }
        def reverseGet(x: F[A5]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(-\/(x))))))
      }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A5]] = Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A5]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A6]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A6]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A6]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
          case _                               => None
        }
        def reverseGet(x: F[A6]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
      }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A6]] = Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A6]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A7]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A7]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A7]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
          case _                                    => None
        }
        def reverseGet(x: F[A7]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
      }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A7]] = Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A7]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma6F.getOption(_))

    implicit def prisma7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A8]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A8]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A8]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
          case _                                         => None
        }
        def reverseGet(x: F[A8]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
      }

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A8]] = Inj.instance(prisma7F.reverseGet(_))

    implicit def inja7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A8]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma7F.getOption(_))

    implicit def prisma8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A9]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A9]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A9]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
          case _                                              => None
        }
        def reverseGet(x: F[A9]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
      }

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A9]] = Inj.instance(prisma8F.reverseGet(_))

    implicit def inja8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A9]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma8F.getOption(_))

    implicit def prisma9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A10]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A10]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A10]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
          case _                                                   => None
        }
        def reverseGet(x: F[A10]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
      }

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A10]] = Inj.instance(prisma9F.reverseGet(_))

    implicit def inja9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A10]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma9F.getOption(_))

    implicit def prisma10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A11]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A11]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A11]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
          case _                                                        => None
        }
        def reverseGet(x: F[A11]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
      }

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A11]] = Inj.instance(prisma10F.reverseGet(_))

    implicit def inja10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A11]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma10F.getOption(_))

    implicit def prisma11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A12]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A12]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A12]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
          case _                                                             => None
        }
        def reverseGet(x: F[A12]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
      }

    implicit def inja11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A12]] = Inj.instance(prisma11F.reverseGet(_))

    implicit def inja11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A12]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma11F.getOption(_))

    implicit def prisma12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A13]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A13]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A13]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
          case _                                                                  => None
        }
        def reverseGet(x: F[A13]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
      }

    implicit def inja12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A13]] = Inj.instance(prisma12F.reverseGet(_))

    implicit def inja12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A13]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma12F.getOption(_))

    implicit def prisma13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A14]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A14]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A14]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
          case _                                                                       => None
        }
        def reverseGet(x: F[A14]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
      }

    implicit def inja13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A14]] = Inj.instance(prisma13F.reverseGet(_))

    implicit def inja13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A14]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma13F.getOption(_))

    implicit def prisma14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A15]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A15]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A15]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
          case _                                                                            => None
        }
        def reverseGet(x: F[A15]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
      }

    implicit def inja14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A15]] = Inj.instance(prisma14F.reverseGet(_))

    implicit def inja14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A15]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma14F.getOption(_))

    implicit def prisma15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A16]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A16]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A16]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Some(x)
          case _                                                                                 => None
        }
        def reverseGet(x: F[A16]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))
      }

    implicit def inja15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A16]] = Inj.instance(prisma15F.reverseGet(_))

    implicit def inja15FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A16]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma15F.getOption(_))

    implicit def prisma16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A17]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A17]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A17]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => Some(x)
          case _                                                                                      => None
        }
        def reverseGet(x: F[A17]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))
      }

    implicit def inja16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A17]] = Inj.instance(prisma16F.reverseGet(_))

    implicit def inja16FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A17]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma16F.getOption(_))

    implicit def prisma17F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A18]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A18]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A18]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) => Some(x)
          case _                                                                                           => None
        }
        def reverseGet(x: F[A18]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))
      }

    implicit def inja17F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A18]] = Inj.instance(prisma17F.reverseGet(_))

    implicit def inja17FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A18]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma17F.getOption(_))

    implicit def prisma18F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A19]] =
      new Prism[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A19]] {
        def getOption(c: Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]): Option[F[A19]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))) => Some(x)
          case _                                                                                           => None
        }
        def reverseGet(x: F[A19]): Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
          Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))))
      }

    implicit def inja18F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], F[A19]] = Inj.instance(prisma18F.reverseGet(_))

    implicit def inja18FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[F[A19]], Cop19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] = Inj.instance(prisma18F.getOption(_))
  }
  object Cop19 extends Cop19LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A1] =
      prisma0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A1], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A2] =
      prisma1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A2], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A3] =
      prisma2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A3], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A4] =
      prisma3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A4], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A5] =
      prisma4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A5], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A6] =
      prisma5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A6], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A7] =
      prisma6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A7], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A8] =
      prisma7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja7IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A8], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A9] =
      prisma8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja8IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A9], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A10] =
      prisma9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja9IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A10], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A11] =
      prisma10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja10IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A11], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A12] =
      prisma11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A12] =
      inja11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja11IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A12], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A13] =
      prisma12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A13] =
      inja12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja12IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A13], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A14] =
      prisma13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A14] =
      inja13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja13IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A14], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A15] =
      prisma14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A15] =
      inja14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja14IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A15], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A16] =
      prisma15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A16] =
      inja15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja15IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A16], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja15FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A17] =
      prisma16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A17] =
      inja16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja16IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A17], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja16FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma17Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A18] =
      prisma17F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja17Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A18] =
      inja17F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja17IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A18], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja17FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def prisma18Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Prism[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A19] =
      prisma18F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja18Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], A19] =
      inja18F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]

    implicit def inja18IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
        : Inj[Option[A19], Cop19[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
      inja18FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
  }

  @newtype case class Prod20[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
      run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20])
  )
  trait Prod20LP {
    implicit def Prod20Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20])]
    ): Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      MF.xmap(
        M,
        Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](_),
        (_: Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]).run
      )

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A1]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A1], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A2]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A2], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A3]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A3], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A4]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A4], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A5]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A5], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A6]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A6], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A7]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A7], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._7)

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A8]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A8], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._8)

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A9]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A9], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._9)

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A10]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A10], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._10)

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A11]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A11], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._11)

    implicit def lifta11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A12]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A12], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._12)

    implicit def lifta12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A13]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A13], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._13)

    implicit def lifta13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A14]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15, t._16, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A14], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._14)

    implicit def lifta14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A15]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x, t._16, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A15], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._15)

    implicit def lifta15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A16]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, x, t._17, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta15FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A16], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._16)

    implicit def lifta16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A17]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, x, t._18, t._19, t._20)
          )
      )
    }

    implicit def lifta16FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A17], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._17)

    implicit def lifta17F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A18]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, x, t._19, t._20)
          )
      )
    }

    implicit def lifta17FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A18], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._18)

    implicit def lifta18F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A19]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, x, t._20)
          )
      )
    }

    implicit def lifta18FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A19], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._19)

    implicit def lifta19F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A20]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, x)
          )
      )
    }

    implicit def lifta19FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[F[A20], Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(_.run._20)
  }
  object Prod20 extends Prod20LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A1, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A2, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A3, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A4, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A5, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A6, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A7, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A8] =
      lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta7IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A8, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A9] =
      lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta8IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A9, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A10] =
      lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta9IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A10, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A11] =
      lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta10IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A11, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A12] =
      lifta11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta11IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A12, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A13] =
      lifta12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta12IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A13, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A14] =
      lifta13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta13IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A14, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A15] =
      lifta14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta14IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A15, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A16] =
      lifta15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta15IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A16, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta15FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A17] =
      lifta16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta16IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A17, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta16FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta17Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A18] =
      lifta17F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta17IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A18, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta17FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta18Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A19] =
      lifta18F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta18IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A19, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta18FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta19Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
        implicit M: Monoid[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]]
    ): Inj[Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A20] =
      lifta19F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def lifta19IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[A20, Prod20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      lifta19FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
  }

  @newtype case class Cop20[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
      run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[
        A19
      ] \/ F[A20])))))))))))))))))))
  )
  trait Cop20LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A1]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A1]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A1]] = c.run match {
          case -\/(x) => Some(x)
          case _      => None
        }
        def reverseGet(x: F[A1]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](-\/(x))
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A1]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A2]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A2]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A2]] = c.run match {
          case \/-(-\/(x)) => Some(x)
          case _           => None
        }
        def reverseGet(x: F[A2]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(-\/(x)))
      }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A2]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A3]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A3]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A3]] = c.run match {
          case \/-(\/-(-\/(x))) => Some(x)
          case _                => None
        }
        def reverseGet(x: F[A3]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(-\/(x))))
      }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A3]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A4]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A4]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A4]] = c.run match {
          case \/-(\/-(\/-(-\/(x)))) => Some(x)
          case _                     => None
        }
        def reverseGet(x: F[A4]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(-\/(x)))))
      }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A4]] = Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A4]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A5]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A5]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A5]] = c.run match {
          case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
          case _                          => None
        }
        def reverseGet(x: F[A5]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(-\/(x))))))
      }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A5]] = Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A5]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A6]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A6]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A6]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
          case _                               => None
        }
        def reverseGet(x: F[A6]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
      }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A6]] = Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A6]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A7]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A7]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A7]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
          case _                                    => None
        }
        def reverseGet(x: F[A7]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
      }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A7]] = Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A7]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma6F.getOption(_))

    implicit def prisma7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A8]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A8]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A8]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
          case _                                         => None
        }
        def reverseGet(x: F[A8]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
      }

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A8]] = Inj.instance(prisma7F.reverseGet(_))

    implicit def inja7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A8]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma7F.getOption(_))

    implicit def prisma8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A9]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A9]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A9]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
          case _                                              => None
        }
        def reverseGet(x: F[A9]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
      }

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A9]] = Inj.instance(prisma8F.reverseGet(_))

    implicit def inja8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A9]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma8F.getOption(_))

    implicit def prisma9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A10]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A10]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A10]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
          case _                                                   => None
        }
        def reverseGet(x: F[A10]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
      }

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A10]] = Inj.instance(prisma9F.reverseGet(_))

    implicit def inja9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A10]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma9F.getOption(_))

    implicit def prisma10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A11]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A11]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A11]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
          case _                                                        => None
        }
        def reverseGet(x: F[A11]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
      }

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A11]] = Inj.instance(prisma10F.reverseGet(_))

    implicit def inja10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A11]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma10F.getOption(_))

    implicit def prisma11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A12]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A12]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A12]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
          case _                                                             => None
        }
        def reverseGet(x: F[A12]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
      }

    implicit def inja11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A12]] = Inj.instance(prisma11F.reverseGet(_))

    implicit def inja11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A12]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma11F.getOption(_))

    implicit def prisma12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A13]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A13]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A13]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
          case _                                                                  => None
        }
        def reverseGet(x: F[A13]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
      }

    implicit def inja12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A13]] = Inj.instance(prisma12F.reverseGet(_))

    implicit def inja12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A13]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma12F.getOption(_))

    implicit def prisma13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A14]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A14]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A14]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
          case _                                                                       => None
        }
        def reverseGet(x: F[A14]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
      }

    implicit def inja13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A14]] = Inj.instance(prisma13F.reverseGet(_))

    implicit def inja13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A14]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma13F.getOption(_))

    implicit def prisma14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A15]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A15]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A15]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
          case _                                                                            => None
        }
        def reverseGet(x: F[A15]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
      }

    implicit def inja14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A15]] = Inj.instance(prisma14F.reverseGet(_))

    implicit def inja14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A15]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma14F.getOption(_))

    implicit def prisma15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A16]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A16]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A16]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Some(x)
          case _                                                                                 => None
        }
        def reverseGet(x: F[A16]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))
      }

    implicit def inja15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A16]] = Inj.instance(prisma15F.reverseGet(_))

    implicit def inja15FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A16]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma15F.getOption(_))

    implicit def prisma16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A17]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A17]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A17]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => Some(x)
          case _                                                                                      => None
        }
        def reverseGet(x: F[A17]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))
      }

    implicit def inja16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A17]] = Inj.instance(prisma16F.reverseGet(_))

    implicit def inja16FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A17]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma16F.getOption(_))

    implicit def prisma17F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A18]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A18]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A18]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) => Some(x)
          case _                                                                                           => None
        }
        def reverseGet(x: F[A18]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))
          )
      }

    implicit def inja17F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A18]] = Inj.instance(prisma17F.reverseGet(_))

    implicit def inja17FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A18]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma17F.getOption(_))

    implicit def prisma18F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A19]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A19]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A19]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))) => Some(x)
          case _                                                                                                => None
        }
        def reverseGet(x: F[A19]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))
          )
      }

    implicit def inja18F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A19]] = Inj.instance(prisma18F.reverseGet(_))

    implicit def inja18FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A19]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma18F.getOption(_))

    implicit def prisma19F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A20]] =
      new Prism[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A20]] {
        def getOption(c: Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]): Option[F[A20]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))))) => Some(x)
          case _                                                                                                => None
        }
        def reverseGet(x: F[A20]): Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
          Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
            \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))))
          )
      }

    implicit def inja19F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], F[A20]] = Inj.instance(prisma19F.reverseGet(_))

    implicit def inja19FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[F[A20]], Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] = Inj.instance(prisma19F.getOption(_))
  }
  object Cop20 extends Cop20LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A1] =
      prisma0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A1], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A2] =
      prisma1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A2], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A3] =
      prisma2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A3], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A4] =
      prisma3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A4], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A5] =
      prisma4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A5], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A6] =
      prisma5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A6], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A7] =
      prisma6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A7], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A8] =
      prisma7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja7IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A8], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A9] =
      prisma8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja8IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A9], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A10] =
      prisma9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja9IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A10], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A11] =
      prisma10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja10IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A11], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A12] =
      prisma11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A12] =
      inja11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja11IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A12], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A13] =
      prisma12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A13] =
      inja12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja12IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A13], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A14] =
      prisma13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A14] =
      inja13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja13IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A14], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A15] =
      prisma14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A15] =
      inja14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja14IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A15], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A16] =
      prisma15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A16] =
      inja15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja15IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A16], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja15FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A17] =
      prisma16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A17] =
      inja16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja16IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A17], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja16FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma17Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A18] =
      prisma17F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja17Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A18] =
      inja17F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja17IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A18], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja17FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma18Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A19] =
      prisma18F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja18Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A19] =
      inja18F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja18IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A19], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja18FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def prisma19Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Prism[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A20] =
      prisma19F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja19Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], A20] =
      inja19F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]

    implicit def inja19IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
        : Inj[Option[A20], Cop20[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
      inja19FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
  }

  @newtype case class Prod21[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
      run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20], F[A21])
  )
  trait Prod21LP {
    implicit def Prod21Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20], F[A21])]
    ): Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      MF.xmap(
        M,
        Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](_),
        (_: Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]).run
      )

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A1]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A1], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A2]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A2], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A3]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A3], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A4]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A4], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A5]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A5], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A6]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A6], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A7]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A7], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._7)

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A8]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A8], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._8)

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A9]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A9], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._9)

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A10]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A10], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._10)

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A11]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A11], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._11)

    implicit def lifta11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A12]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A12], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._12)

    implicit def lifta12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A13]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A13], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._13)

    implicit def lifta13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A14]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A14], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._14)

    implicit def lifta14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A15]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x, t._16, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A15], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._15)

    implicit def lifta15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A16]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, x, t._17, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta15FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A16], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._16)

    implicit def lifta16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A17]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, x, t._18, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta16FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A17], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._17)

    implicit def lifta17F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A18]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, x, t._19, t._20, t._21)
          )
      )
    }

    implicit def lifta17FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A18], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._18)

    implicit def lifta18F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A19]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, x, t._20, t._21)
          )
      )
    }

    implicit def lifta18FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A19], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._19)

    implicit def lifta19F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A20]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, x, t._21)
          )
      )
    }

    implicit def lifta19FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A20], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._20)

    implicit def lifta20F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A21]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, x)
          )
      )
    }

    implicit def lifta20FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[F[A21], Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(_.run._21)
  }
  object Prod21 extends Prod21LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A1, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A2, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A3, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A4, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A5, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A6, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A7, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A8] =
      lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta7IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A8, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A9] =
      lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta8IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A9, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A10] =
      lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta9IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A10, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A11] =
      lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta10IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A11, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A12] =
      lifta11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta11IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A12, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A13] =
      lifta12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta12IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A13, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A14] =
      lifta13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta13IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A14, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A15] =
      lifta14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta14IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A15, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A16] =
      lifta15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta15IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A16, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta15FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A17] =
      lifta16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta16IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A17, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta16FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta17Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A18] =
      lifta17F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta17IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A18, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta17FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta18Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A19] =
      lifta18F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta18IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A19, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta18FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta19Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A20] =
      lifta19F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta19IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A20, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta19FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta20Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
        implicit M: Monoid[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]]
    ): Inj[Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A21] =
      lifta20F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def lifta20IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[A21, Prod21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      lifta20FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
  }

  @newtype case class Cop21[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
      run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[
        A19
      ] \/ (F[A20] \/ F[A21]))))))))))))))))))))
  )
  trait Cop21LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A1]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A1]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A1]] = c.run match {
          case -\/(x) => Some(x)
          case _      => None
        }
        def reverseGet(x: F[A1]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](-\/(x))
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A1]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A2]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A2]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A2]] = c.run match {
          case \/-(-\/(x)) => Some(x)
          case _           => None
        }
        def reverseGet(x: F[A2]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(-\/(x)))
      }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A2]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A3]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A3]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A3]] = c.run match {
          case \/-(\/-(-\/(x))) => Some(x)
          case _                => None
        }
        def reverseGet(x: F[A3]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(-\/(x))))
      }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A3]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A4]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A4]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A4]] = c.run match {
          case \/-(\/-(\/-(-\/(x)))) => Some(x)
          case _                     => None
        }
        def reverseGet(x: F[A4]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(-\/(x)))))
      }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A4]] = Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A4]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A5]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A5]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A5]] = c.run match {
          case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
          case _                          => None
        }
        def reverseGet(x: F[A5]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(-\/(x))))))
      }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A5]] = Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A5]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A6]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A6]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A6]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
          case _                               => None
        }
        def reverseGet(x: F[A6]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
      }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A6]] = Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A6]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A7]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A7]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A7]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
          case _                                    => None
        }
        def reverseGet(x: F[A7]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
      }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A7]] = Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A7]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma6F.getOption(_))

    implicit def prisma7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A8]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A8]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A8]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
          case _                                         => None
        }
        def reverseGet(x: F[A8]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
      }

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A8]] = Inj.instance(prisma7F.reverseGet(_))

    implicit def inja7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A8]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma7F.getOption(_))

    implicit def prisma8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A9]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A9]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A9]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
          case _                                              => None
        }
        def reverseGet(x: F[A9]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
      }

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A9]] = Inj.instance(prisma8F.reverseGet(_))

    implicit def inja8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A9]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma8F.getOption(_))

    implicit def prisma9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A10]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A10]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A10]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
          case _                                                   => None
        }
        def reverseGet(x: F[A10]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
      }

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A10]] = Inj.instance(prisma9F.reverseGet(_))

    implicit def inja9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A10]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma9F.getOption(_))

    implicit def prisma10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A11]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A11]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A11]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
          case _                                                        => None
        }
        def reverseGet(x: F[A11]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
      }

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A11]] = Inj.instance(prisma10F.reverseGet(_))

    implicit def inja10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A11]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma10F.getOption(_))

    implicit def prisma11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A12]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A12]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A12]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
          case _                                                             => None
        }
        def reverseGet(x: F[A12]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
      }

    implicit def inja11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A12]] = Inj.instance(prisma11F.reverseGet(_))

    implicit def inja11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A12]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma11F.getOption(_))

    implicit def prisma12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A13]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A13]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A13]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
          case _                                                                  => None
        }
        def reverseGet(x: F[A13]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
      }

    implicit def inja12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A13]] = Inj.instance(prisma12F.reverseGet(_))

    implicit def inja12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A13]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma12F.getOption(_))

    implicit def prisma13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A14]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A14]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A14]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
          case _                                                                       => None
        }
        def reverseGet(x: F[A14]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
      }

    implicit def inja13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A14]] = Inj.instance(prisma13F.reverseGet(_))

    implicit def inja13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A14]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma13F.getOption(_))

    implicit def prisma14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A15]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A15]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A15]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
          case _                                                                            => None
        }
        def reverseGet(x: F[A15]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
      }

    implicit def inja14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A15]] = Inj.instance(prisma14F.reverseGet(_))

    implicit def inja14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A15]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma14F.getOption(_))

    implicit def prisma15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A16]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A16]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A16]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Some(x)
          case _                                                                                 => None
        }
        def reverseGet(x: F[A16]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))
      }

    implicit def inja15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A16]] = Inj.instance(prisma15F.reverseGet(_))

    implicit def inja15FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A16]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma15F.getOption(_))

    implicit def prisma16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A17]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A17]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A17]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => Some(x)
          case _                                                                                      => None
        }
        def reverseGet(x: F[A17]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))
          )
      }

    implicit def inja16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A17]] = Inj.instance(prisma16F.reverseGet(_))

    implicit def inja16FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A17]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma16F.getOption(_))

    implicit def prisma17F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A18]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A18]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A18]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) => Some(x)
          case _                                                                                           => None
        }
        def reverseGet(x: F[A18]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))
          )
      }

    implicit def inja17F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A18]] = Inj.instance(prisma17F.reverseGet(_))

    implicit def inja17FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A18]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma17F.getOption(_))

    implicit def prisma18F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A19]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A19]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A19]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))) => Some(x)
          case _                                                                                                => None
        }
        def reverseGet(x: F[A19]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))
          )
      }

    implicit def inja18F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A19]] = Inj.instance(prisma18F.reverseGet(_))

    implicit def inja18FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A19]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma18F.getOption(_))

    implicit def prisma19F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A20]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A20]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A20]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))) => Some(x)
          case _                                                                                                     => None
        }
        def reverseGet(x: F[A20]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))))
          )
      }

    implicit def inja19F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A20]] = Inj.instance(prisma19F.reverseGet(_))

    implicit def inja19FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A20]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma19F.getOption(_))

    implicit def prisma20F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A21]] =
      new Prism[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A21]] {
        def getOption(c: Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]): Option[F[A21]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))))) => Some(x)
          case _                                                                                                     => None
        }
        def reverseGet(x: F[A21]): Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
          Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
            \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))))))
          )
      }

    implicit def inja20F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], F[A21]] = Inj.instance(prisma20F.reverseGet(_))

    implicit def inja20FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[F[A21]], Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] = Inj.instance(prisma20F.getOption(_))
  }
  object Cop21 extends Cop21LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A1] =
      prisma0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A1], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A2] =
      prisma1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A2], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A3] =
      prisma2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A3], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A4] =
      prisma3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A4], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A5] =
      prisma4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A5], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A6] =
      prisma5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A6], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A7] =
      prisma6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A7], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A8] =
      prisma7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja7IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A8], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A9] =
      prisma8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja8IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A9], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A10] =
      prisma9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja9IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A10], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A11] =
      prisma10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja10IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A11], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A12] =
      prisma11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A12] =
      inja11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja11IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A12], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A13] =
      prisma12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A13] =
      inja12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja12IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A13], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A14] =
      prisma13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A14] =
      inja13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja13IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A14], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A15] =
      prisma14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A15] =
      inja14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja14IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A15], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A16] =
      prisma15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A16] =
      inja15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja15IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A16], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja15FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A17] =
      prisma16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A17] =
      inja16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja16IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A17], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja16FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma17Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A18] =
      prisma17F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja17Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A18] =
      inja17F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja17IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A18], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja17FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma18Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A19] =
      prisma18F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja18Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A19] =
      inja18F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja18IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A19], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja18FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma19Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A20] =
      prisma19F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja19Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A20] =
      inja19F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja19IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A20], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja19FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def prisma20Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Prism[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A21] =
      prisma20F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja20Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], A21] =
      inja20F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]

    implicit def inja20IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
        : Inj[Option[A21], Cop21[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
      inja20FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
  }

  @newtype case class Prod22[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
      run: (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20], F[A21], F[A22])
  )
  trait Prod22LP {
    implicit def Prod22Monoid[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[(F[A1], F[A2], F[A3], F[A4], F[A5], F[A6], F[A7], F[A8], F[A9], F[A10], F[A11], F[A12], F[A13], F[A14], F[A15], F[A16], F[A17], F[A18], F[A19], F[A20], F[A21], F[A22])]
    ): Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      MF.xmap(
        M,
        Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](_),
        (_: Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]).run
      )

    implicit def lifta0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A1]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A1], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._1)

    implicit def lifta1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A2]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A2], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._2)

    implicit def lifta2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A3]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A3], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._3)

    implicit def lifta3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A4]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A4], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._4)

    implicit def lifta4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A5]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A5], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._5)

    implicit def lifta5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A6]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A6], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._6)

    implicit def lifta6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A7]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A7], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._7)

    implicit def lifta7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A8]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A8], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._8)

    implicit def lifta8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A9]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A9], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._9)

    implicit def lifta9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A10]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, x, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A10], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._10)

    implicit def lifta10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A11]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, x, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A11], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._11)

    implicit def lifta11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A12]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, x, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A12], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._12)

    implicit def lifta12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A13]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, x, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A13], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._13)

    implicit def lifta13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A14]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, x, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A14], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._14)

    implicit def lifta14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A15]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, x, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A15], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._15)

    implicit def lifta15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A16]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, x, t._17, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta15FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A16], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._16)

    implicit def lifta16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A17]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, x, t._18, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta16FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A17], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._17)

    implicit def lifta17F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A18]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, x, t._19, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta17FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A18], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._18)

    implicit def lifta18F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A19]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, x, t._20, t._21, t._22)
          )
      )
    }

    implicit def lifta18FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A19], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._19)

    implicit def lifta19F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A20]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, x, t._21, t._22)
          )
      )
    }

    implicit def lifta19FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A20], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._20)

    implicit def lifta20F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A21]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, x, t._22)
          )
      )
    }

    implicit def lifta20FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A21], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._21)

    implicit def lifta21F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A22]] = {
      val t = M.zero.run
      Inj.instance(
        x =>
          Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, x)
          )
      )
    }

    implicit def lifta21FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[F[A22], Prod22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(_.run._22)
  }
  object Prod22 extends Prod22LP {
    implicit def lifta0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A1] =
      lifta0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta0IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A1, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A2] =
      lifta1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta1IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A2, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A3] =
      lifta2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta2IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A3, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A4] =
      lifta3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta3IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A4, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A5] =
      lifta4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta4IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A5, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A6] =
      lifta5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta5IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A6, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A7] =
      lifta6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta6IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A7, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A8] =
      lifta7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta7IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A8, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A9] =
      lifta8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta8IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A9, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A10] =
      lifta9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta9IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A10, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A11] =
      lifta10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta10IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A11, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A12] =
      lifta11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta11IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A12, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A13] =
      lifta12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta12IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A13, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A14] =
      lifta13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta13IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A14, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A15] =
      lifta14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta14IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A15, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A16] =
      lifta15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta15IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A16, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta15FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A17] =
      lifta16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta16IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A17, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta16FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta17Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A18] =
      lifta17F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta17IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A18, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta17FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta18Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A19] =
      lifta18F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta18IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A19, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta18FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta19Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A20] =
      lifta19F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta19IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A20, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta19FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta20Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A21] =
      lifta20F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta20IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A21, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta20FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta21Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
        implicit M: Monoid[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]]
    ): Inj[Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A22] =
      lifta21F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def lifta21IdInverse[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[A22, Prod22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      lifta21FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
  }

  @newtype case class Cop22[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
      run: (F[A1] \/ (F[A2] \/ (F[A3] \/ (F[A4] \/ (F[A5] \/ (F[A6] \/ (F[A7] \/ (F[A8] \/ (F[A9] \/ (F[A10] \/ (F[A11] \/ (F[A12] \/ (F[A13] \/ (F[A14] \/ (F[A15] \/ (F[A16] \/ (F[A17] \/ (F[A18] \/ (F[
        A19
      ] \/ (F[A20] \/ (F[A21] \/ F[A22])))))))))))))))))))))
  )
  trait Cop22LP {
    implicit def prisma0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A1]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A1]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A1]] = c.run match {
          case -\/(x) => Some(x)
          case _      => None
        }
        def reverseGet(x: F[A1]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](-\/(x))
      }

    implicit def inja0F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A1]] = Inj.instance(prisma0F.reverseGet(_))

    implicit def inja0FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A1]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma0F.getOption(_))

    implicit def prisma1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A2]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A2]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A2]] = c.run match {
          case \/-(-\/(x)) => Some(x)
          case _           => None
        }
        def reverseGet(x: F[A2]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(-\/(x)))
      }

    implicit def inja1F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A2]] = Inj.instance(prisma1F.reverseGet(_))

    implicit def inja1FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A2]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma1F.getOption(_))

    implicit def prisma2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A3]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A3]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A3]] = c.run match {
          case \/-(\/-(-\/(x))) => Some(x)
          case _                => None
        }
        def reverseGet(x: F[A3]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(-\/(x))))
      }

    implicit def inja2F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A3]] = Inj.instance(prisma2F.reverseGet(_))

    implicit def inja2FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A3]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma2F.getOption(_))

    implicit def prisma3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A4]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A4]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A4]] = c.run match {
          case \/-(\/-(\/-(-\/(x)))) => Some(x)
          case _                     => None
        }
        def reverseGet(x: F[A4]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(-\/(x)))))
      }

    implicit def inja3F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A4]] = Inj.instance(prisma3F.reverseGet(_))

    implicit def inja3FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A4]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma3F.getOption(_))

    implicit def prisma4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A5]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A5]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A5]] = c.run match {
          case \/-(\/-(\/-(\/-(-\/(x))))) => Some(x)
          case _                          => None
        }
        def reverseGet(x: F[A5]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(-\/(x))))))
      }

    implicit def inja4F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A5]] = Inj.instance(prisma4F.reverseGet(_))

    implicit def inja4FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A5]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma4F.getOption(_))

    implicit def prisma5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A6]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A6]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A6]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(-\/(x)))))) => Some(x)
          case _                               => None
        }
        def reverseGet(x: F[A6]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(-\/(x)))))))
      }

    implicit def inja5F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A6]] = Inj.instance(prisma5F.reverseGet(_))

    implicit def inja5FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A6]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma5F.getOption(_))

    implicit def prisma6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A7]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A7]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A7]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))) => Some(x)
          case _                                    => None
        }
        def reverseGet(x: F[A7]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))
      }

    implicit def inja6F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A7]] = Inj.instance(prisma6F.reverseGet(_))

    implicit def inja6FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A7]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma6F.getOption(_))

    implicit def prisma7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A8]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A8]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A8]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))) => Some(x)
          case _                                         => None
        }
        def reverseGet(x: F[A8]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))
      }

    implicit def inja7F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A8]] = Inj.instance(prisma7F.reverseGet(_))

    implicit def inja7FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A8]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma7F.getOption(_))

    implicit def prisma8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A9]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A9]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A9]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))) => Some(x)
          case _                                              => None
        }
        def reverseGet(x: F[A9]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))
      }

    implicit def inja8F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A9]] = Inj.instance(prisma8F.reverseGet(_))

    implicit def inja8FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A9]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma8F.getOption(_))

    implicit def prisma9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A10]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A10]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A10]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))) => Some(x)
          case _                                                   => None
        }
        def reverseGet(x: F[A10]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))
      }

    implicit def inja9F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A10]] = Inj.instance(prisma9F.reverseGet(_))

    implicit def inja9FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A10]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma9F.getOption(_))

    implicit def prisma10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A11]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A11]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A11]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))) => Some(x)
          case _                                                        => None
        }
        def reverseGet(x: F[A11]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))
      }

    implicit def inja10F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A11]] = Inj.instance(prisma10F.reverseGet(_))

    implicit def inja10FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A11]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma10F.getOption(_))

    implicit def prisma11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A12]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A12]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A12]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))) => Some(x)
          case _                                                             => None
        }
        def reverseGet(x: F[A12]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))
      }

    implicit def inja11F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A12]] = Inj.instance(prisma11F.reverseGet(_))

    implicit def inja11FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A12]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma11F.getOption(_))

    implicit def prisma12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A13]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A13]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A13]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))) => Some(x)
          case _                                                                  => None
        }
        def reverseGet(x: F[A13]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))
      }

    implicit def inja12F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A13]] = Inj.instance(prisma12F.reverseGet(_))

    implicit def inja12FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A13]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma12F.getOption(_))

    implicit def prisma13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A14]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A14]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A14]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))) => Some(x)
          case _                                                                       => None
        }
        def reverseGet(x: F[A14]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))
      }

    implicit def inja13F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A14]] = Inj.instance(prisma13F.reverseGet(_))

    implicit def inja13FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A14]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma13F.getOption(_))

    implicit def prisma14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A15]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A15]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A15]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))) => Some(x)
          case _                                                                            => None
        }
        def reverseGet(x: F[A15]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
      }

    implicit def inja14F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A15]] = Inj.instance(prisma14F.reverseGet(_))

    implicit def inja14FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A15]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma14F.getOption(_))

    implicit def prisma15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A16]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A16]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A16]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))) => Some(x)
          case _                                                                                 => None
        }
        def reverseGet(x: F[A16]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))
          )
      }

    implicit def inja15F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A16]] = Inj.instance(prisma15F.reverseGet(_))

    implicit def inja15FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A16]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma15F.getOption(_))

    implicit def prisma16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A17]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A17]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A17]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))) => Some(x)
          case _                                                                                      => None
        }
        def reverseGet(x: F[A17]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))
          )
      }

    implicit def inja16F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A17]] = Inj.instance(prisma16F.reverseGet(_))

    implicit def inja16FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A17]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma16F.getOption(_))

    implicit def prisma17F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A18]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A18]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A18]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))) => Some(x)
          case _                                                                                           => None
        }
        def reverseGet(x: F[A18]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))
          )
      }

    implicit def inja17F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A18]] = Inj.instance(prisma17F.reverseGet(_))

    implicit def inja17FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A18]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma17F.getOption(_))

    implicit def prisma18F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A19]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A19]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A19]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))) => Some(x)
          case _                                                                                                => None
        }
        def reverseGet(x: F[A19]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))
          )
      }

    implicit def inja18F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A19]] = Inj.instance(prisma18F.reverseGet(_))

    implicit def inja18FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A19]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma18F.getOption(_))

    implicit def prisma19F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A20]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A20]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A20]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))) => Some(x)
          case _                                                                                                     => None
        }
        def reverseGet(x: F[A20]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))))
          )
      }

    implicit def inja19F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A20]] = Inj.instance(prisma19F.reverseGet(_))

    implicit def inja19FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A20]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma19F.getOption(_))

    implicit def prisma20F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A21]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A21]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A21]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x))))))))))))))))))))) => Some(x)
          case _                                                                                                          => None
        }
        def reverseGet(x: F[A21]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(x)))))))))))))))))))))
          )
      }

    implicit def inja20F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A21]] = Inj.instance(prisma20F.reverseGet(_))

    implicit def inja20FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A21]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma20F.getOption(_))

    implicit def prisma21F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A22]] =
      new Prism[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A22]] {
        def getOption(c: Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]): Option[F[A22]] = c.run match {
          case \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x))))))))))))))))))))) => Some(x)
          case _                                                                                                          => None
        }
        def reverseGet(x: F[A22]): Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] =
          Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
            \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(x)))))))))))))))))))))
          )
      }

    implicit def inja21F[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], F[A22]] = Inj.instance(prisma21F.reverseGet(_))

    implicit def inja21FInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[F[A22]], Cop22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] = Inj.instance(prisma21F.getOption(_))
  }
  object Cop22 extends Cop22LP {
    implicit def prisma0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A1] =
      prisma0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja0Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A1] =
      inja0F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja0IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A1], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja0FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A2] =
      prisma1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja1Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A2] =
      inja1F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja1IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A2], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja1FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A3] =
      prisma2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja2Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A3] =
      inja2F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja2IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A3], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja2FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A4] =
      prisma3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja3Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A4] =
      inja3F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja3IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A4], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja3FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A5] =
      prisma4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja4Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A5] =
      inja4F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja4IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A5], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja4FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A6] =
      prisma5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja5Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A6] =
      inja5F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja5IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A6], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja5FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A7] =
      prisma6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja6Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A7] =
      inja6F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja6IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A7], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja6FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A8] =
      prisma7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja7Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A8] =
      inja7F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja7IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A8], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja7FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A9] =
      prisma8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja8Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A9] =
      inja8F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja8IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A9], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja8FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A10] =
      prisma9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja9Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A10] =
      inja9F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja9IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A10], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja9FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A11] =
      prisma10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja10Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A11] =
      inja10F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja10IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A11], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja10FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A12] =
      prisma11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja11Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A12] =
      inja11F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja11IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A12], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja11FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A13] =
      prisma12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja12Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A13] =
      inja12F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja12IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A13], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja12FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A14] =
      prisma13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja13Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A14] =
      inja13F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja13IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A14], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja13FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A15] =
      prisma14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja14Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A15] =
      inja14F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja14IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A15], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja14FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A16] =
      prisma15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja15Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A16] =
      inja15F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja15IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A16], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja15FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A17] =
      prisma16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja16Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A17] =
      inja16F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja16IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A17], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja16FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma17Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A18] =
      prisma17F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja17Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A18] =
      inja17F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja17IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A18], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja17FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma18Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A19] =
      prisma18F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja18Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A19] =
      inja18F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja18IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A19], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja18FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma19Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A20] =
      prisma19F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja19Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A20] =
      inja19F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja19IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A20], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja19FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma20Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A21] =
      prisma20F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja20Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A21] =
      inja20F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja20IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A21], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja20FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def prisma21Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Prism[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A22] =
      prisma21F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja21Id[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], A22] =
      inja21F[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]

    implicit def inja21IdInverse[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
        : Inj[Option[A22], Cop22[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
      inja21FInverse[Id, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]
  }

}
