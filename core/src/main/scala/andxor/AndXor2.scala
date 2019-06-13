package andxor

import andxor.types.{Cop2, Prod2}
import scalaz.{Apply, Monoid, \/}
import scalaz.Id.Id

trait AndXor2[A1 <: AndXor, A2 <: AndXor] extends AndXor {
  type Prod[F[_]] = Prod2[F, A1, A2]
  object Prod {
    def apply[F[_]](p: (A1#Prod[F], A2#Prod[F])): Prod[F] = Prod2[F, A1, A2](p)
  }

  type Cop[F[_]] = Cop2[F, A1, A2]
  object Cop {
    def apply[F[_]](c: (A1#Cop[F] \/ A2#Cop[F])): Cop[F] = Cop2[F, A1, A2](c)
  }

  def mkChoose[TC[_], F[_], B](f: B => Cop[F])(implicit d: Decidable[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]]): TC[B] =
    Combine.choose2(a0, a1)(f(_).run)

  def mkAlt[TC[_], F[_], B](f: Cop[F] => B)(implicit a: Alt[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]]): TC[B] =
    Combine.altly2(a0, a1)(x => f(Cop(x)))

  def mkDivide[TC[_], F[_], B](f: B => Prod[F])(implicit d: Divide[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]]): TC[B] =
    Combine.divide2(a0, a1)(f(_).run)

  def mkApply[TC[_], F[_], B](f: Prod[F] => B)(implicit a: Apply[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]]): TC[B] =

    Combine.apply2(a0, a1) {
      case (i0, i1) =>
        f(Prod((i0, i1)))
    }

  def mkChoose[TC[_], B](f: B => Cop[Id])(implicit d: Decidable[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], dummy: DummyImplicit): TC[B] = mkChoose[TC, Id, B](f)
  def mkAlt[TC[_], B](f: Cop[Id] => B)(implicit a: Alt[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], dummy: DummyImplicit): TC[B] = mkAlt[TC, Id, B](f)
  def mkDivide[TC[_], B](f: B => Prod[Id])(implicit d: Divide[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], dummy: DummyImplicit): TC[B] = mkDivide[TC, Id, B](f)
  def mkApply[TC[_], B](f: Prod[Id] => B)(implicit a: Apply[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], dummy: DummyImplicit): TC[B] = mkApply[TC, Id, B](f)

  def choose[TC[_], F[_]](implicit d: Decidable[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]]): TC[Cop[F]] = mkChoose[TC, F, Cop[F]](identity)
  def alt[TC[_], F[_]](implicit a: Alt[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]]): TC[Cop[F]] = mkAlt[TC, F, Cop[F]](identity)
  def divide[TC[_], F[_]](implicit d: Divide[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]]): TC[Prod[F]] = mkDivide[TC, F, Prod[F]](identity)
  def apply[TC[_], F[_]](implicit a: Apply[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]]): TC[Prod[F]] = mkApply[TC, F, Prod[F]](identity)

  def choose[TC[_]](implicit d: Decidable[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], dummy: DummyImplicit): TC[Cop[Id]] = mkChoose[TC, Cop[Id]](identity)
  def alt[TC[_]](implicit a: Alt[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], dummy: DummyImplicit): TC[Cop[Id]] = mkAlt[TC, Cop[Id]](identity)
  def divide[TC[_]](implicit d: Divide[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], dummy: DummyImplicit): TC[Prod[Id]] = mkDivide[TC, Prod[Id]](identity)
  def apply[TC[_]](implicit a: Apply[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], dummy: DummyImplicit): TC[Prod[Id]] = mkApply[TC, Prod[Id]](identity)

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = choose[Inj[Cop[F], ?], F]
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = divide[Inj[Prod[F], ?], F]
  }
}

object AndXor2 {
  def apply[A1 <: AndXor, A2 <: AndXor]: AndXor2[A1, A2] =
    new AndXor2[A1, A2] {}
}
