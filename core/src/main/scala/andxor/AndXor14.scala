package andxor

import andxor.types.{Cop14, Prod14}
import scalaz.{Apply, Monoid, \/}
import scalaz.Id.Id

trait AndXor14[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor] extends AndXor {
  type Prod[F[_]] = Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  object Prod {
    def apply[F[_]](p: (A1#Prod[F], A2#Prod[F], A3#Prod[F], A4#Prod[F], A5#Prod[F], A6#Prod[F], A7#Prod[F], A8#Prod[F], A9#Prod[F], A10#Prod[F], A11#Prod[F], A12#Prod[F], A13#Prod[F], A14#Prod[F])): Prod[F] = Prod14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](p)
  }

  type Cop[F[_]] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]
  object Cop {
    def apply[F[_]](c: (A1#Cop[F] \/ (A2#Cop[F] \/ (A3#Cop[F] \/ (A4#Cop[F] \/ (A5#Cop[F] \/ (A6#Cop[F] \/ (A7#Cop[F] \/ (A8#Cop[F] \/ (A9#Cop[F] \/ (A10#Cop[F] \/ (A11#Cop[F] \/ (A12#Cop[F] \/ (A13#Cop[F] \/ A14#Cop[F])))))))))))))): Cop[F] = Cop14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](c)
  }

  def mkChoose[TC[_], F[_], B](f: B => Cop[F])(implicit d: Decidable[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]], a2: TC[A3#Cop[F]], a3: TC[A4#Cop[F]], a4: TC[A5#Cop[F]], a5: TC[A6#Cop[F]], a6: TC[A7#Cop[F]], a7: TC[A8#Cop[F]], a8: TC[A9#Cop[F]], a9: TC[A10#Cop[F]], a10: TC[A11#Cop[F]], a11: TC[A12#Cop[F]], a12: TC[A13#Cop[F]], a13: TC[A14#Cop[F]]): TC[B] =
    Combine.choose14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(f(_).run)

  def mkAlt[TC[_], F[_], B](f: Cop[F] => B)(implicit a: Alt[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]], a2: TC[A3#Cop[F]], a3: TC[A4#Cop[F]], a4: TC[A5#Cop[F]], a5: TC[A6#Cop[F]], a6: TC[A7#Cop[F]], a7: TC[A8#Cop[F]], a8: TC[A9#Cop[F]], a9: TC[A10#Cop[F]], a10: TC[A11#Cop[F]], a11: TC[A12#Cop[F]], a12: TC[A13#Cop[F]], a13: TC[A14#Cop[F]]): TC[B] =
    Combine.altly14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(x => f(Cop(x)))

  def mkDivide[TC[_], F[_], B](f: B => Prod[F])(implicit d: Divide[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]], a2: TC[A3#Prod[F]], a3: TC[A4#Prod[F]], a4: TC[A5#Prod[F]], a5: TC[A6#Prod[F]], a6: TC[A7#Prod[F]], a7: TC[A8#Prod[F]], a8: TC[A9#Prod[F]], a9: TC[A10#Prod[F]], a10: TC[A11#Prod[F]], a11: TC[A12#Prod[F]], a12: TC[A13#Prod[F]], a13: TC[A14#Prod[F]]): TC[B] =
    Combine.divide14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(f(_).run)

  def mkApply[TC[_], F[_], B](f: Prod[F] => B)(implicit a: Apply[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]], a2: TC[A3#Prod[F]], a3: TC[A4#Prod[F]], a4: TC[A5#Prod[F]], a5: TC[A6#Prod[F]], a6: TC[A7#Prod[F]], a7: TC[A8#Prod[F]], a8: TC[A9#Prod[F]], a9: TC[A10#Prod[F]], a10: TC[A11#Prod[F]], a11: TC[A12#Prod[F]], a12: TC[A13#Prod[F]], a13: TC[A14#Prod[F]]): TC[B] =

    Combine.apply14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) {
      case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13) =>
        f(Prod((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13)))
    }

  def mkChoose[TC[_], B](f: B => Cop[Id])(implicit d: Decidable[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], a2: TC[A3#Cop[Id]], a3: TC[A4#Cop[Id]], a4: TC[A5#Cop[Id]], a5: TC[A6#Cop[Id]], a6: TC[A7#Cop[Id]], a7: TC[A8#Cop[Id]], a8: TC[A9#Cop[Id]], a9: TC[A10#Cop[Id]], a10: TC[A11#Cop[Id]], a11: TC[A12#Cop[Id]], a12: TC[A13#Cop[Id]], a13: TC[A14#Cop[Id]], dummy: DummyImplicit): TC[B] = mkChoose[TC, Id, B](f)
  def mkAlt[TC[_], B](f: Cop[Id] => B)(implicit a: Alt[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], a2: TC[A3#Cop[Id]], a3: TC[A4#Cop[Id]], a4: TC[A5#Cop[Id]], a5: TC[A6#Cop[Id]], a6: TC[A7#Cop[Id]], a7: TC[A8#Cop[Id]], a8: TC[A9#Cop[Id]], a9: TC[A10#Cop[Id]], a10: TC[A11#Cop[Id]], a11: TC[A12#Cop[Id]], a12: TC[A13#Cop[Id]], a13: TC[A14#Cop[Id]], dummy: DummyImplicit): TC[B] = mkAlt[TC, Id, B](f)
  def mkDivide[TC[_], B](f: B => Prod[Id])(implicit d: Divide[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], a2: TC[A3#Prod[Id]], a3: TC[A4#Prod[Id]], a4: TC[A5#Prod[Id]], a5: TC[A6#Prod[Id]], a6: TC[A7#Prod[Id]], a7: TC[A8#Prod[Id]], a8: TC[A9#Prod[Id]], a9: TC[A10#Prod[Id]], a10: TC[A11#Prod[Id]], a11: TC[A12#Prod[Id]], a12: TC[A13#Prod[Id]], a13: TC[A14#Prod[Id]], dummy: DummyImplicit): TC[B] = mkDivide[TC, Id, B](f)
  def mkApply[TC[_], B](f: Prod[Id] => B)(implicit a: Apply[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], a2: TC[A3#Prod[Id]], a3: TC[A4#Prod[Id]], a4: TC[A5#Prod[Id]], a5: TC[A6#Prod[Id]], a6: TC[A7#Prod[Id]], a7: TC[A8#Prod[Id]], a8: TC[A9#Prod[Id]], a9: TC[A10#Prod[Id]], a10: TC[A11#Prod[Id]], a11: TC[A12#Prod[Id]], a12: TC[A13#Prod[Id]], a13: TC[A14#Prod[Id]], dummy: DummyImplicit): TC[B] = mkApply[TC, Id, B](f)

  def choose[TC[_], F[_]](implicit d: Decidable[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]], a2: TC[A3#Cop[F]], a3: TC[A4#Cop[F]], a4: TC[A5#Cop[F]], a5: TC[A6#Cop[F]], a6: TC[A7#Cop[F]], a7: TC[A8#Cop[F]], a8: TC[A9#Cop[F]], a9: TC[A10#Cop[F]], a10: TC[A11#Cop[F]], a11: TC[A12#Cop[F]], a12: TC[A13#Cop[F]], a13: TC[A14#Cop[F]]): TC[Cop[F]] = mkChoose[TC, F, Cop[F]](identity)
  def alt[TC[_], F[_]](implicit a: Alt[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]], a2: TC[A3#Cop[F]], a3: TC[A4#Cop[F]], a4: TC[A5#Cop[F]], a5: TC[A6#Cop[F]], a6: TC[A7#Cop[F]], a7: TC[A8#Cop[F]], a8: TC[A9#Cop[F]], a9: TC[A10#Cop[F]], a10: TC[A11#Cop[F]], a11: TC[A12#Cop[F]], a12: TC[A13#Cop[F]], a13: TC[A14#Cop[F]]): TC[Cop[F]] = mkAlt[TC, F, Cop[F]](identity)
  def divide[TC[_], F[_]](implicit d: Divide[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]], a2: TC[A3#Prod[F]], a3: TC[A4#Prod[F]], a4: TC[A5#Prod[F]], a5: TC[A6#Prod[F]], a6: TC[A7#Prod[F]], a7: TC[A8#Prod[F]], a8: TC[A9#Prod[F]], a9: TC[A10#Prod[F]], a10: TC[A11#Prod[F]], a11: TC[A12#Prod[F]], a12: TC[A13#Prod[F]], a13: TC[A14#Prod[F]]): TC[Prod[F]] = mkDivide[TC, F, Prod[F]](identity)
  def apply[TC[_], F[_]](implicit a: Apply[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]], a2: TC[A3#Prod[F]], a3: TC[A4#Prod[F]], a4: TC[A5#Prod[F]], a5: TC[A6#Prod[F]], a6: TC[A7#Prod[F]], a7: TC[A8#Prod[F]], a8: TC[A9#Prod[F]], a9: TC[A10#Prod[F]], a10: TC[A11#Prod[F]], a11: TC[A12#Prod[F]], a12: TC[A13#Prod[F]], a13: TC[A14#Prod[F]]): TC[Prod[F]] = mkApply[TC, F, Prod[F]](identity)

  def choose[TC[_]](implicit d: Decidable[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], a2: TC[A3#Cop[Id]], a3: TC[A4#Cop[Id]], a4: TC[A5#Cop[Id]], a5: TC[A6#Cop[Id]], a6: TC[A7#Cop[Id]], a7: TC[A8#Cop[Id]], a8: TC[A9#Cop[Id]], a9: TC[A10#Cop[Id]], a10: TC[A11#Cop[Id]], a11: TC[A12#Cop[Id]], a12: TC[A13#Cop[Id]], a13: TC[A14#Cop[Id]], dummy: DummyImplicit): TC[Cop[Id]] = mkChoose[TC, Cop[Id]](identity)
  def alt[TC[_]](implicit a: Alt[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], a2: TC[A3#Cop[Id]], a3: TC[A4#Cop[Id]], a4: TC[A5#Cop[Id]], a5: TC[A6#Cop[Id]], a6: TC[A7#Cop[Id]], a7: TC[A8#Cop[Id]], a8: TC[A9#Cop[Id]], a9: TC[A10#Cop[Id]], a10: TC[A11#Cop[Id]], a11: TC[A12#Cop[Id]], a12: TC[A13#Cop[Id]], a13: TC[A14#Cop[Id]], dummy: DummyImplicit): TC[Cop[Id]] = mkAlt[TC, Cop[Id]](identity)
  def divide[TC[_]](implicit d: Divide[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], a2: TC[A3#Prod[Id]], a3: TC[A4#Prod[Id]], a4: TC[A5#Prod[Id]], a5: TC[A6#Prod[Id]], a6: TC[A7#Prod[Id]], a7: TC[A8#Prod[Id]], a8: TC[A9#Prod[Id]], a9: TC[A10#Prod[Id]], a10: TC[A11#Prod[Id]], a11: TC[A12#Prod[Id]], a12: TC[A13#Prod[Id]], a13: TC[A14#Prod[Id]], dummy: DummyImplicit): TC[Prod[Id]] = mkDivide[TC, Prod[Id]](identity)
  def apply[TC[_]](implicit a: Apply[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], a2: TC[A3#Prod[Id]], a3: TC[A4#Prod[Id]], a4: TC[A5#Prod[Id]], a5: TC[A6#Prod[Id]], a6: TC[A7#Prod[Id]], a7: TC[A8#Prod[Id]], a8: TC[A9#Prod[Id]], a9: TC[A10#Prod[Id]], a10: TC[A11#Prod[Id]], a11: TC[A12#Prod[Id]], a12: TC[A13#Prod[Id]], a13: TC[A14#Prod[Id]], dummy: DummyImplicit): TC[Prod[Id]] = mkApply[TC, Prod[Id]](identity)

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = choose[Inj[Cop[F], ?], F]
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = divide[Inj[Prod[F], ?], F]
  }
}

object AndXor14 {
  def apply[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor]: AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] =
    new AndXor14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}
}
