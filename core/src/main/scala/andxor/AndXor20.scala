package andxor

import andxor.types.{Cop20, Prod20}
import scalaz.{Apply, Monoid, \/}
import scalaz.Id.Id

trait AndXor20[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor, A19 <: AndXor, A20 <: AndXor] extends AndXor {
  type Prod[F[_]] = Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
  object Prod {
    def apply[F[_]](p: (A1#Prod[F], A2#Prod[F], A3#Prod[F], A4#Prod[F], A5#Prod[F], A6#Prod[F], A7#Prod[F], A8#Prod[F], A9#Prod[F], A10#Prod[F], A11#Prod[F], A12#Prod[F], A13#Prod[F], A14#Prod[F], A15#Prod[F], A16#Prod[F], A17#Prod[F], A18#Prod[F], A19#Prod[F], A20#Prod[F])): Prod[F] = Prod20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](p)
  }

  type Cop[F[_]] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
  object Cop {
    def apply[F[_]](c: (A1#Cop[F] \/ (A2#Cop[F] \/ (A3#Cop[F] \/ (A4#Cop[F] \/ (A5#Cop[F] \/ (A6#Cop[F] \/ (A7#Cop[F] \/ (A8#Cop[F] \/ (A9#Cop[F] \/ (A10#Cop[F] \/ (A11#Cop[F] \/ (A12#Cop[F] \/ (A13#Cop[F] \/ (A14#Cop[F] \/ (A15#Cop[F] \/ (A16#Cop[F] \/ (A17#Cop[F] \/ (A18#Cop[F] \/ (A19#Cop[F] \/ A20#Cop[F])))))))))))))))))))): Cop[F] = Cop20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](c)
  }

  def mkChoose[TC[_], F[_], B](f: B => Cop[F])(implicit d: Decidable[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]], a2: TC[A3#Cop[F]], a3: TC[A4#Cop[F]], a4: TC[A5#Cop[F]], a5: TC[A6#Cop[F]], a6: TC[A7#Cop[F]], a7: TC[A8#Cop[F]], a8: TC[A9#Cop[F]], a9: TC[A10#Cop[F]], a10: TC[A11#Cop[F]], a11: TC[A12#Cop[F]], a12: TC[A13#Cop[F]], a13: TC[A14#Cop[F]], a14: TC[A15#Cop[F]], a15: TC[A16#Cop[F]], a16: TC[A17#Cop[F]], a17: TC[A18#Cop[F]], a18: TC[A19#Cop[F]], a19: TC[A20#Cop[F]]): TC[B] =
    Combine.choose20(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)(f(_).run)

  def mkAlt[TC[_], F[_], B](f: Cop[F] => B)(implicit a: Alt[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]], a2: TC[A3#Cop[F]], a3: TC[A4#Cop[F]], a4: TC[A5#Cop[F]], a5: TC[A6#Cop[F]], a6: TC[A7#Cop[F]], a7: TC[A8#Cop[F]], a8: TC[A9#Cop[F]], a9: TC[A10#Cop[F]], a10: TC[A11#Cop[F]], a11: TC[A12#Cop[F]], a12: TC[A13#Cop[F]], a13: TC[A14#Cop[F]], a14: TC[A15#Cop[F]], a15: TC[A16#Cop[F]], a16: TC[A17#Cop[F]], a17: TC[A18#Cop[F]], a18: TC[A19#Cop[F]], a19: TC[A20#Cop[F]]): TC[B] =
    Combine.altly20(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)(x => f(Cop(x)))

  def mkDivide[TC[_], F[_], B](f: B => Prod[F])(implicit d: Divide[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]], a2: TC[A3#Prod[F]], a3: TC[A4#Prod[F]], a4: TC[A5#Prod[F]], a5: TC[A6#Prod[F]], a6: TC[A7#Prod[F]], a7: TC[A8#Prod[F]], a8: TC[A9#Prod[F]], a9: TC[A10#Prod[F]], a10: TC[A11#Prod[F]], a11: TC[A12#Prod[F]], a12: TC[A13#Prod[F]], a13: TC[A14#Prod[F]], a14: TC[A15#Prod[F]], a15: TC[A16#Prod[F]], a16: TC[A17#Prod[F]], a17: TC[A18#Prod[F]], a18: TC[A19#Prod[F]], a19: TC[A20#Prod[F]]): TC[B] =
    Combine.divide20(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)(f(_).run)

  def mkApply[TC[_], F[_], B](f: Prod[F] => B)(implicit a: Apply[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]], a2: TC[A3#Prod[F]], a3: TC[A4#Prod[F]], a4: TC[A5#Prod[F]], a5: TC[A6#Prod[F]], a6: TC[A7#Prod[F]], a7: TC[A8#Prod[F]], a8: TC[A9#Prod[F]], a9: TC[A10#Prod[F]], a10: TC[A11#Prod[F]], a11: TC[A12#Prod[F]], a12: TC[A13#Prod[F]], a13: TC[A14#Prod[F]], a14: TC[A15#Prod[F]], a15: TC[A16#Prod[F]], a16: TC[A17#Prod[F]], a17: TC[A18#Prod[F]], a18: TC[A19#Prod[F]], a19: TC[A20#Prod[F]]): TC[B] =

    Combine.apply20(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) {
      case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19) =>
        f(Prod((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19)))
    }

  def mkChoose[TC[_], B](f: B => Cop[Id])(implicit d: Decidable[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], a2: TC[A3#Cop[Id]], a3: TC[A4#Cop[Id]], a4: TC[A5#Cop[Id]], a5: TC[A6#Cop[Id]], a6: TC[A7#Cop[Id]], a7: TC[A8#Cop[Id]], a8: TC[A9#Cop[Id]], a9: TC[A10#Cop[Id]], a10: TC[A11#Cop[Id]], a11: TC[A12#Cop[Id]], a12: TC[A13#Cop[Id]], a13: TC[A14#Cop[Id]], a14: TC[A15#Cop[Id]], a15: TC[A16#Cop[Id]], a16: TC[A17#Cop[Id]], a17: TC[A18#Cop[Id]], a18: TC[A19#Cop[Id]], a19: TC[A20#Cop[Id]], dummy: DummyImplicit): TC[B] = mkChoose[TC, Id, B](f)
  def mkAlt[TC[_], B](f: Cop[Id] => B)(implicit a: Alt[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], a2: TC[A3#Cop[Id]], a3: TC[A4#Cop[Id]], a4: TC[A5#Cop[Id]], a5: TC[A6#Cop[Id]], a6: TC[A7#Cop[Id]], a7: TC[A8#Cop[Id]], a8: TC[A9#Cop[Id]], a9: TC[A10#Cop[Id]], a10: TC[A11#Cop[Id]], a11: TC[A12#Cop[Id]], a12: TC[A13#Cop[Id]], a13: TC[A14#Cop[Id]], a14: TC[A15#Cop[Id]], a15: TC[A16#Cop[Id]], a16: TC[A17#Cop[Id]], a17: TC[A18#Cop[Id]], a18: TC[A19#Cop[Id]], a19: TC[A20#Cop[Id]], dummy: DummyImplicit): TC[B] = mkAlt[TC, Id, B](f)
  def mkDivide[TC[_], B](f: B => Prod[Id])(implicit d: Divide[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], a2: TC[A3#Prod[Id]], a3: TC[A4#Prod[Id]], a4: TC[A5#Prod[Id]], a5: TC[A6#Prod[Id]], a6: TC[A7#Prod[Id]], a7: TC[A8#Prod[Id]], a8: TC[A9#Prod[Id]], a9: TC[A10#Prod[Id]], a10: TC[A11#Prod[Id]], a11: TC[A12#Prod[Id]], a12: TC[A13#Prod[Id]], a13: TC[A14#Prod[Id]], a14: TC[A15#Prod[Id]], a15: TC[A16#Prod[Id]], a16: TC[A17#Prod[Id]], a17: TC[A18#Prod[Id]], a18: TC[A19#Prod[Id]], a19: TC[A20#Prod[Id]], dummy: DummyImplicit): TC[B] = mkDivide[TC, Id, B](f)
  def mkApply[TC[_], B](f: Prod[Id] => B)(implicit a: Apply[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], a2: TC[A3#Prod[Id]], a3: TC[A4#Prod[Id]], a4: TC[A5#Prod[Id]], a5: TC[A6#Prod[Id]], a6: TC[A7#Prod[Id]], a7: TC[A8#Prod[Id]], a8: TC[A9#Prod[Id]], a9: TC[A10#Prod[Id]], a10: TC[A11#Prod[Id]], a11: TC[A12#Prod[Id]], a12: TC[A13#Prod[Id]], a13: TC[A14#Prod[Id]], a14: TC[A15#Prod[Id]], a15: TC[A16#Prod[Id]], a16: TC[A17#Prod[Id]], a17: TC[A18#Prod[Id]], a18: TC[A19#Prod[Id]], a19: TC[A20#Prod[Id]], dummy: DummyImplicit): TC[B] = mkApply[TC, Id, B](f)

  def choose[TC[_], F[_]](implicit d: Decidable[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]], a2: TC[A3#Cop[F]], a3: TC[A4#Cop[F]], a4: TC[A5#Cop[F]], a5: TC[A6#Cop[F]], a6: TC[A7#Cop[F]], a7: TC[A8#Cop[F]], a8: TC[A9#Cop[F]], a9: TC[A10#Cop[F]], a10: TC[A11#Cop[F]], a11: TC[A12#Cop[F]], a12: TC[A13#Cop[F]], a13: TC[A14#Cop[F]], a14: TC[A15#Cop[F]], a15: TC[A16#Cop[F]], a16: TC[A17#Cop[F]], a17: TC[A18#Cop[F]], a18: TC[A19#Cop[F]], a19: TC[A20#Cop[F]]): TC[Cop[F]] = mkChoose[TC, F, Cop[F]](identity)
  def alt[TC[_], F[_]](implicit a: Alt[TC], a0: TC[A1#Cop[F]], a1: TC[A2#Cop[F]], a2: TC[A3#Cop[F]], a3: TC[A4#Cop[F]], a4: TC[A5#Cop[F]], a5: TC[A6#Cop[F]], a6: TC[A7#Cop[F]], a7: TC[A8#Cop[F]], a8: TC[A9#Cop[F]], a9: TC[A10#Cop[F]], a10: TC[A11#Cop[F]], a11: TC[A12#Cop[F]], a12: TC[A13#Cop[F]], a13: TC[A14#Cop[F]], a14: TC[A15#Cop[F]], a15: TC[A16#Cop[F]], a16: TC[A17#Cop[F]], a17: TC[A18#Cop[F]], a18: TC[A19#Cop[F]], a19: TC[A20#Cop[F]]): TC[Cop[F]] = mkAlt[TC, F, Cop[F]](identity)
  def divide[TC[_], F[_]](implicit d: Divide[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]], a2: TC[A3#Prod[F]], a3: TC[A4#Prod[F]], a4: TC[A5#Prod[F]], a5: TC[A6#Prod[F]], a6: TC[A7#Prod[F]], a7: TC[A8#Prod[F]], a8: TC[A9#Prod[F]], a9: TC[A10#Prod[F]], a10: TC[A11#Prod[F]], a11: TC[A12#Prod[F]], a12: TC[A13#Prod[F]], a13: TC[A14#Prod[F]], a14: TC[A15#Prod[F]], a15: TC[A16#Prod[F]], a16: TC[A17#Prod[F]], a17: TC[A18#Prod[F]], a18: TC[A19#Prod[F]], a19: TC[A20#Prod[F]]): TC[Prod[F]] = mkDivide[TC, F, Prod[F]](identity)
  def apply[TC[_], F[_]](implicit a: Apply[TC], a0: TC[A1#Prod[F]], a1: TC[A2#Prod[F]], a2: TC[A3#Prod[F]], a3: TC[A4#Prod[F]], a4: TC[A5#Prod[F]], a5: TC[A6#Prod[F]], a6: TC[A7#Prod[F]], a7: TC[A8#Prod[F]], a8: TC[A9#Prod[F]], a9: TC[A10#Prod[F]], a10: TC[A11#Prod[F]], a11: TC[A12#Prod[F]], a12: TC[A13#Prod[F]], a13: TC[A14#Prod[F]], a14: TC[A15#Prod[F]], a15: TC[A16#Prod[F]], a16: TC[A17#Prod[F]], a17: TC[A18#Prod[F]], a18: TC[A19#Prod[F]], a19: TC[A20#Prod[F]]): TC[Prod[F]] = mkApply[TC, F, Prod[F]](identity)

  def choose[TC[_]](implicit d: Decidable[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], a2: TC[A3#Cop[Id]], a3: TC[A4#Cop[Id]], a4: TC[A5#Cop[Id]], a5: TC[A6#Cop[Id]], a6: TC[A7#Cop[Id]], a7: TC[A8#Cop[Id]], a8: TC[A9#Cop[Id]], a9: TC[A10#Cop[Id]], a10: TC[A11#Cop[Id]], a11: TC[A12#Cop[Id]], a12: TC[A13#Cop[Id]], a13: TC[A14#Cop[Id]], a14: TC[A15#Cop[Id]], a15: TC[A16#Cop[Id]], a16: TC[A17#Cop[Id]], a17: TC[A18#Cop[Id]], a18: TC[A19#Cop[Id]], a19: TC[A20#Cop[Id]], dummy: DummyImplicit): TC[Cop[Id]] = mkChoose[TC, Cop[Id]](identity)
  def alt[TC[_]](implicit a: Alt[TC], a0: TC[A1#Cop[Id]], a1: TC[A2#Cop[Id]], a2: TC[A3#Cop[Id]], a3: TC[A4#Cop[Id]], a4: TC[A5#Cop[Id]], a5: TC[A6#Cop[Id]], a6: TC[A7#Cop[Id]], a7: TC[A8#Cop[Id]], a8: TC[A9#Cop[Id]], a9: TC[A10#Cop[Id]], a10: TC[A11#Cop[Id]], a11: TC[A12#Cop[Id]], a12: TC[A13#Cop[Id]], a13: TC[A14#Cop[Id]], a14: TC[A15#Cop[Id]], a15: TC[A16#Cop[Id]], a16: TC[A17#Cop[Id]], a17: TC[A18#Cop[Id]], a18: TC[A19#Cop[Id]], a19: TC[A20#Cop[Id]], dummy: DummyImplicit): TC[Cop[Id]] = mkAlt[TC, Cop[Id]](identity)
  def divide[TC[_]](implicit d: Divide[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], a2: TC[A3#Prod[Id]], a3: TC[A4#Prod[Id]], a4: TC[A5#Prod[Id]], a5: TC[A6#Prod[Id]], a6: TC[A7#Prod[Id]], a7: TC[A8#Prod[Id]], a8: TC[A9#Prod[Id]], a9: TC[A10#Prod[Id]], a10: TC[A11#Prod[Id]], a11: TC[A12#Prod[Id]], a12: TC[A13#Prod[Id]], a13: TC[A14#Prod[Id]], a14: TC[A15#Prod[Id]], a15: TC[A16#Prod[Id]], a16: TC[A17#Prod[Id]], a17: TC[A18#Prod[Id]], a18: TC[A19#Prod[Id]], a19: TC[A20#Prod[Id]], dummy: DummyImplicit): TC[Prod[Id]] = mkDivide[TC, Prod[Id]](identity)
  def apply[TC[_]](implicit a: Apply[TC], a0: TC[A1#Prod[Id]], a1: TC[A2#Prod[Id]], a2: TC[A3#Prod[Id]], a3: TC[A4#Prod[Id]], a4: TC[A5#Prod[Id]], a5: TC[A6#Prod[Id]], a6: TC[A7#Prod[Id]], a7: TC[A8#Prod[Id]], a8: TC[A9#Prod[Id]], a9: TC[A10#Prod[Id]], a10: TC[A11#Prod[Id]], a11: TC[A12#Prod[Id]], a12: TC[A13#Prod[Id]], a13: TC[A14#Prod[Id]], a14: TC[A15#Prod[Id]], a15: TC[A16#Prod[Id]], a16: TC[A17#Prod[Id]], a17: TC[A18#Prod[Id]], a18: TC[A19#Prod[Id]], a19: TC[A20#Prod[Id]], dummy: DummyImplicit): TC[Prod[Id]] = mkApply[TC, Prod[Id]](identity)

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = choose[Inj[Cop[F], ?], F]
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = divide[Inj[Prod[F], ?], F]
  }
}

object AndXor20 {
  def apply[A1 <: AndXor, A2 <: AndXor, A3 <: AndXor, A4 <: AndXor, A5 <: AndXor, A6 <: AndXor, A7 <: AndXor, A8 <: AndXor, A9 <: AndXor, A10 <: AndXor, A11 <: AndXor, A12 <: AndXor, A13 <: AndXor, A14 <: AndXor, A15 <: AndXor, A16 <: AndXor, A17 <: AndXor, A18 <: AndXor, A19 <: AndXor, A20 <: AndXor]: AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
    new AndXor20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}
}
