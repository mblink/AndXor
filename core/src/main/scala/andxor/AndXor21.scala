package andxor

import andxor.types.{Cop21, Prod21}
import scala.annotation.tailrec
import scalaz.{Apply, Functor, PlusEmpty, Monoid, \/, -\/, \/-, ~>}
import scalaz.Id.Id

trait AndXor21[
    A1 <: AndXor,
    A2 <: AndXor,
    A3 <: AndXor,
    A4 <: AndXor,
    A5 <: AndXor,
    A6 <: AndXor,
    A7 <: AndXor,
    A8 <: AndXor,
    A9 <: AndXor,
    A10 <: AndXor,
    A11 <: AndXor,
    A12 <: AndXor,
    A13 <: AndXor,
    A14 <: AndXor,
    A15 <: AndXor,
    A16 <: AndXor,
    A17 <: AndXor,
    A18 <: AndXor,
    A19 <: AndXor,
    A20 <: AndXor,
    A21 <: AndXor
] extends AndXor {
  type Prod[F[_]] = Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
  object Prod {
    def apply[F[_]](
        p: (
            A1#Prod[F],
            A2#Prod[F],
            A3#Prod[F],
            A4#Prod[F],
            A5#Prod[F],
            A6#Prod[F],
            A7#Prod[F],
            A8#Prod[F],
            A9#Prod[F],
            A10#Prod[F],
            A11#Prod[F],
            A12#Prod[F],
            A13#Prod[F],
            A14#Prod[F],
            A15#Prod[F],
            A16#Prod[F],
            A17#Prod[F],
            A18#Prod[F],
            A19#Prod[F],
            A20#Prod[F],
            A21#Prod[F]
        )
    ): Prod[F] = Prod21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](p)
  }

  type Cop[F[_]] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
  object Cop {
    def apply[F[_]](
        c: (A1#Cop[F] \/ (A2#Cop[F] \/ (A3#Cop[F] \/ (A4#Cop[F] \/ (A5#Cop[F] \/ (A6#Cop[F] \/ (A7#Cop[F] \/ (A8#Cop[F] \/ (A9#Cop[F] \/ (A10#Cop[F] \/ (A11#Cop[F] \/ (A12#Cop[F] \/ (A13#Cop[F] \/ (A14#Cop[
          F
        ] \/ (A15#Cop[F] \/ (A16#Cop[F] \/ (A17#Cop[F] \/ (A18#Cop[F] \/ (A19#Cop[F] \/ (A20#Cop[F] \/ A21#Cop[F]))))))))))))))))))))
    ): Cop[F] = Cop21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](c)
  }

  def mkChoose[TC[_], F[_], B](f: B => Cop[F])(
      implicit d: Decidable[TC],
      a0: TC[A1#Cop[F]],
      a1: TC[A2#Cop[F]],
      a2: TC[A3#Cop[F]],
      a3: TC[A4#Cop[F]],
      a4: TC[A5#Cop[F]],
      a5: TC[A6#Cop[F]],
      a6: TC[A7#Cop[F]],
      a7: TC[A8#Cop[F]],
      a8: TC[A9#Cop[F]],
      a9: TC[A10#Cop[F]],
      a10: TC[A11#Cop[F]],
      a11: TC[A12#Cop[F]],
      a12: TC[A13#Cop[F]],
      a13: TC[A14#Cop[F]],
      a14: TC[A15#Cop[F]],
      a15: TC[A16#Cop[F]],
      a16: TC[A17#Cop[F]],
      a17: TC[A18#Cop[F]],
      a18: TC[A19#Cop[F]],
      a19: TC[A20#Cop[F]],
      a20: TC[A21#Cop[F]]
  ): TC[B] =
    Combine.choose21(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(f(_).run)

  def mkAlt[TC[_], F[_], B](f: Cop[F] => B)(
      implicit a: Alt[TC],
      a0: TC[A1#Cop[F]],
      a1: TC[A2#Cop[F]],
      a2: TC[A3#Cop[F]],
      a3: TC[A4#Cop[F]],
      a4: TC[A5#Cop[F]],
      a5: TC[A6#Cop[F]],
      a6: TC[A7#Cop[F]],
      a7: TC[A8#Cop[F]],
      a8: TC[A9#Cop[F]],
      a9: TC[A10#Cop[F]],
      a10: TC[A11#Cop[F]],
      a11: TC[A12#Cop[F]],
      a12: TC[A13#Cop[F]],
      a13: TC[A14#Cop[F]],
      a14: TC[A15#Cop[F]],
      a15: TC[A16#Cop[F]],
      a16: TC[A17#Cop[F]],
      a17: TC[A18#Cop[F]],
      a18: TC[A19#Cop[F]],
      a19: TC[A20#Cop[F]],
      a20: TC[A21#Cop[F]]
  ): TC[B] =
    Combine.altly21(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(x => f(Cop(x)))

  def mkDivide[TC[_], F[_], B](f: B => Prod[F])(
      implicit d: Divide[TC],
      a0: TC[A1#Prod[F]],
      a1: TC[A2#Prod[F]],
      a2: TC[A3#Prod[F]],
      a3: TC[A4#Prod[F]],
      a4: TC[A5#Prod[F]],
      a5: TC[A6#Prod[F]],
      a6: TC[A7#Prod[F]],
      a7: TC[A8#Prod[F]],
      a8: TC[A9#Prod[F]],
      a9: TC[A10#Prod[F]],
      a10: TC[A11#Prod[F]],
      a11: TC[A12#Prod[F]],
      a12: TC[A13#Prod[F]],
      a13: TC[A14#Prod[F]],
      a14: TC[A15#Prod[F]],
      a15: TC[A16#Prod[F]],
      a16: TC[A17#Prod[F]],
      a17: TC[A18#Prod[F]],
      a18: TC[A19#Prod[F]],
      a19: TC[A20#Prod[F]],
      a20: TC[A21#Prod[F]]
  ): TC[B] =
    Combine.divide21(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(f(_).run)

  def mkApply[TC[_], F[_], B](f: Prod[F] => B)(
      implicit a: Apply[TC],
      a0: TC[A1#Prod[F]],
      a1: TC[A2#Prod[F]],
      a2: TC[A3#Prod[F]],
      a3: TC[A4#Prod[F]],
      a4: TC[A5#Prod[F]],
      a5: TC[A6#Prod[F]],
      a6: TC[A7#Prod[F]],
      a7: TC[A8#Prod[F]],
      a8: TC[A9#Prod[F]],
      a9: TC[A10#Prod[F]],
      a10: TC[A11#Prod[F]],
      a11: TC[A12#Prod[F]],
      a12: TC[A13#Prod[F]],
      a13: TC[A14#Prod[F]],
      a14: TC[A15#Prod[F]],
      a15: TC[A16#Prod[F]],
      a16: TC[A17#Prod[F]],
      a17: TC[A18#Prod[F]],
      a18: TC[A19#Prod[F]],
      a19: TC[A20#Prod[F]],
      a20: TC[A21#Prod[F]]
  ): TC[B] =
    Combine.apply21(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) {
      case (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20) =>
        f(Prod((i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20)))
    }

  def mkChoose[TC[_], B](f: B => Cop[Id])(
      implicit d: Decidable[TC],
      a0: TC[A1#Cop[Id]],
      a1: TC[A2#Cop[Id]],
      a2: TC[A3#Cop[Id]],
      a3: TC[A4#Cop[Id]],
      a4: TC[A5#Cop[Id]],
      a5: TC[A6#Cop[Id]],
      a6: TC[A7#Cop[Id]],
      a7: TC[A8#Cop[Id]],
      a8: TC[A9#Cop[Id]],
      a9: TC[A10#Cop[Id]],
      a10: TC[A11#Cop[Id]],
      a11: TC[A12#Cop[Id]],
      a12: TC[A13#Cop[Id]],
      a13: TC[A14#Cop[Id]],
      a14: TC[A15#Cop[Id]],
      a15: TC[A16#Cop[Id]],
      a16: TC[A17#Cop[Id]],
      a17: TC[A18#Cop[Id]],
      a18: TC[A19#Cop[Id]],
      a19: TC[A20#Cop[Id]],
      a20: TC[A21#Cop[Id]],
      dummy: DummyImplicit
  ): TC[B] = mkChoose[TC, Id, B](f)
  def mkAlt[TC[_], B](f: Cop[Id] => B)(
      implicit a: Alt[TC],
      a0: TC[A1#Cop[Id]],
      a1: TC[A2#Cop[Id]],
      a2: TC[A3#Cop[Id]],
      a3: TC[A4#Cop[Id]],
      a4: TC[A5#Cop[Id]],
      a5: TC[A6#Cop[Id]],
      a6: TC[A7#Cop[Id]],
      a7: TC[A8#Cop[Id]],
      a8: TC[A9#Cop[Id]],
      a9: TC[A10#Cop[Id]],
      a10: TC[A11#Cop[Id]],
      a11: TC[A12#Cop[Id]],
      a12: TC[A13#Cop[Id]],
      a13: TC[A14#Cop[Id]],
      a14: TC[A15#Cop[Id]],
      a15: TC[A16#Cop[Id]],
      a16: TC[A17#Cop[Id]],
      a17: TC[A18#Cop[Id]],
      a18: TC[A19#Cop[Id]],
      a19: TC[A20#Cop[Id]],
      a20: TC[A21#Cop[Id]],
      dummy: DummyImplicit
  ): TC[B] = mkAlt[TC, Id, B](f)
  def mkDivide[TC[_], B](f: B => Prod[Id])(
      implicit d: Divide[TC],
      a0: TC[A1#Prod[Id]],
      a1: TC[A2#Prod[Id]],
      a2: TC[A3#Prod[Id]],
      a3: TC[A4#Prod[Id]],
      a4: TC[A5#Prod[Id]],
      a5: TC[A6#Prod[Id]],
      a6: TC[A7#Prod[Id]],
      a7: TC[A8#Prod[Id]],
      a8: TC[A9#Prod[Id]],
      a9: TC[A10#Prod[Id]],
      a10: TC[A11#Prod[Id]],
      a11: TC[A12#Prod[Id]],
      a12: TC[A13#Prod[Id]],
      a13: TC[A14#Prod[Id]],
      a14: TC[A15#Prod[Id]],
      a15: TC[A16#Prod[Id]],
      a16: TC[A17#Prod[Id]],
      a17: TC[A18#Prod[Id]],
      a18: TC[A19#Prod[Id]],
      a19: TC[A20#Prod[Id]],
      a20: TC[A21#Prod[Id]],
      dummy: DummyImplicit
  ): TC[B] = mkDivide[TC, Id, B](f)
  def mkApply[TC[_], B](f: Prod[Id] => B)(
      implicit a: Apply[TC],
      a0: TC[A1#Prod[Id]],
      a1: TC[A2#Prod[Id]],
      a2: TC[A3#Prod[Id]],
      a3: TC[A4#Prod[Id]],
      a4: TC[A5#Prod[Id]],
      a5: TC[A6#Prod[Id]],
      a6: TC[A7#Prod[Id]],
      a7: TC[A8#Prod[Id]],
      a8: TC[A9#Prod[Id]],
      a9: TC[A10#Prod[Id]],
      a10: TC[A11#Prod[Id]],
      a11: TC[A12#Prod[Id]],
      a12: TC[A13#Prod[Id]],
      a13: TC[A14#Prod[Id]],
      a14: TC[A15#Prod[Id]],
      a15: TC[A16#Prod[Id]],
      a16: TC[A17#Prod[Id]],
      a17: TC[A18#Prod[Id]],
      a18: TC[A19#Prod[Id]],
      a19: TC[A20#Prod[Id]],
      a20: TC[A21#Prod[Id]],
      dummy: DummyImplicit
  ): TC[B] = mkApply[TC, Id, B](f)

  def choose[TC[_], F[_]](
      implicit d: Decidable[TC],
      a0: TC[A1#Cop[F]],
      a1: TC[A2#Cop[F]],
      a2: TC[A3#Cop[F]],
      a3: TC[A4#Cop[F]],
      a4: TC[A5#Cop[F]],
      a5: TC[A6#Cop[F]],
      a6: TC[A7#Cop[F]],
      a7: TC[A8#Cop[F]],
      a8: TC[A9#Cop[F]],
      a9: TC[A10#Cop[F]],
      a10: TC[A11#Cop[F]],
      a11: TC[A12#Cop[F]],
      a12: TC[A13#Cop[F]],
      a13: TC[A14#Cop[F]],
      a14: TC[A15#Cop[F]],
      a15: TC[A16#Cop[F]],
      a16: TC[A17#Cop[F]],
      a17: TC[A18#Cop[F]],
      a18: TC[A19#Cop[F]],
      a19: TC[A20#Cop[F]],
      a20: TC[A21#Cop[F]]
  ): TC[Cop[F]] = mkChoose[TC, F, Cop[F]](identity)
  def alt[TC[_], F[_]](
      implicit a: Alt[TC],
      a0: TC[A1#Cop[F]],
      a1: TC[A2#Cop[F]],
      a2: TC[A3#Cop[F]],
      a3: TC[A4#Cop[F]],
      a4: TC[A5#Cop[F]],
      a5: TC[A6#Cop[F]],
      a6: TC[A7#Cop[F]],
      a7: TC[A8#Cop[F]],
      a8: TC[A9#Cop[F]],
      a9: TC[A10#Cop[F]],
      a10: TC[A11#Cop[F]],
      a11: TC[A12#Cop[F]],
      a12: TC[A13#Cop[F]],
      a13: TC[A14#Cop[F]],
      a14: TC[A15#Cop[F]],
      a15: TC[A16#Cop[F]],
      a16: TC[A17#Cop[F]],
      a17: TC[A18#Cop[F]],
      a18: TC[A19#Cop[F]],
      a19: TC[A20#Cop[F]],
      a20: TC[A21#Cop[F]]
  ): TC[Cop[F]] = mkAlt[TC, F, Cop[F]](identity)
  def divide[TC[_], F[_]](
      implicit d: Divide[TC],
      a0: TC[A1#Prod[F]],
      a1: TC[A2#Prod[F]],
      a2: TC[A3#Prod[F]],
      a3: TC[A4#Prod[F]],
      a4: TC[A5#Prod[F]],
      a5: TC[A6#Prod[F]],
      a6: TC[A7#Prod[F]],
      a7: TC[A8#Prod[F]],
      a8: TC[A9#Prod[F]],
      a9: TC[A10#Prod[F]],
      a10: TC[A11#Prod[F]],
      a11: TC[A12#Prod[F]],
      a12: TC[A13#Prod[F]],
      a13: TC[A14#Prod[F]],
      a14: TC[A15#Prod[F]],
      a15: TC[A16#Prod[F]],
      a16: TC[A17#Prod[F]],
      a17: TC[A18#Prod[F]],
      a18: TC[A19#Prod[F]],
      a19: TC[A20#Prod[F]],
      a20: TC[A21#Prod[F]]
  ): TC[Prod[F]] = mkDivide[TC, F, Prod[F]](identity)
  def apply[TC[_], F[_]](
      implicit a: Apply[TC],
      a0: TC[A1#Prod[F]],
      a1: TC[A2#Prod[F]],
      a2: TC[A3#Prod[F]],
      a3: TC[A4#Prod[F]],
      a4: TC[A5#Prod[F]],
      a5: TC[A6#Prod[F]],
      a6: TC[A7#Prod[F]],
      a7: TC[A8#Prod[F]],
      a8: TC[A9#Prod[F]],
      a9: TC[A10#Prod[F]],
      a10: TC[A11#Prod[F]],
      a11: TC[A12#Prod[F]],
      a12: TC[A13#Prod[F]],
      a13: TC[A14#Prod[F]],
      a14: TC[A15#Prod[F]],
      a15: TC[A16#Prod[F]],
      a16: TC[A17#Prod[F]],
      a17: TC[A18#Prod[F]],
      a18: TC[A19#Prod[F]],
      a19: TC[A20#Prod[F]],
      a20: TC[A21#Prod[F]]
  ): TC[Prod[F]] = mkApply[TC, F, Prod[F]](identity)

  def choose[TC[_]](
      implicit d: Decidable[TC],
      a0: TC[A1#Cop[Id]],
      a1: TC[A2#Cop[Id]],
      a2: TC[A3#Cop[Id]],
      a3: TC[A4#Cop[Id]],
      a4: TC[A5#Cop[Id]],
      a5: TC[A6#Cop[Id]],
      a6: TC[A7#Cop[Id]],
      a7: TC[A8#Cop[Id]],
      a8: TC[A9#Cop[Id]],
      a9: TC[A10#Cop[Id]],
      a10: TC[A11#Cop[Id]],
      a11: TC[A12#Cop[Id]],
      a12: TC[A13#Cop[Id]],
      a13: TC[A14#Cop[Id]],
      a14: TC[A15#Cop[Id]],
      a15: TC[A16#Cop[Id]],
      a16: TC[A17#Cop[Id]],
      a17: TC[A18#Cop[Id]],
      a18: TC[A19#Cop[Id]],
      a19: TC[A20#Cop[Id]],
      a20: TC[A21#Cop[Id]],
      dummy: DummyImplicit
  ): TC[Cop[Id]] = mkChoose[TC, Cop[Id]](identity)
  def alt[TC[_]](
      implicit a: Alt[TC],
      a0: TC[A1#Cop[Id]],
      a1: TC[A2#Cop[Id]],
      a2: TC[A3#Cop[Id]],
      a3: TC[A4#Cop[Id]],
      a4: TC[A5#Cop[Id]],
      a5: TC[A6#Cop[Id]],
      a6: TC[A7#Cop[Id]],
      a7: TC[A8#Cop[Id]],
      a8: TC[A9#Cop[Id]],
      a9: TC[A10#Cop[Id]],
      a10: TC[A11#Cop[Id]],
      a11: TC[A12#Cop[Id]],
      a12: TC[A13#Cop[Id]],
      a13: TC[A14#Cop[Id]],
      a14: TC[A15#Cop[Id]],
      a15: TC[A16#Cop[Id]],
      a16: TC[A17#Cop[Id]],
      a17: TC[A18#Cop[Id]],
      a18: TC[A19#Cop[Id]],
      a19: TC[A20#Cop[Id]],
      a20: TC[A21#Cop[Id]],
      dummy: DummyImplicit
  ): TC[Cop[Id]] = mkAlt[TC, Cop[Id]](identity)
  def divide[TC[_]](
      implicit d: Divide[TC],
      a0: TC[A1#Prod[Id]],
      a1: TC[A2#Prod[Id]],
      a2: TC[A3#Prod[Id]],
      a3: TC[A4#Prod[Id]],
      a4: TC[A5#Prod[Id]],
      a5: TC[A6#Prod[Id]],
      a6: TC[A7#Prod[Id]],
      a7: TC[A8#Prod[Id]],
      a8: TC[A9#Prod[Id]],
      a9: TC[A10#Prod[Id]],
      a10: TC[A11#Prod[Id]],
      a11: TC[A12#Prod[Id]],
      a12: TC[A13#Prod[Id]],
      a13: TC[A14#Prod[Id]],
      a14: TC[A15#Prod[Id]],
      a15: TC[A16#Prod[Id]],
      a16: TC[A17#Prod[Id]],
      a17: TC[A18#Prod[Id]],
      a18: TC[A19#Prod[Id]],
      a19: TC[A20#Prod[Id]],
      a20: TC[A21#Prod[Id]],
      dummy: DummyImplicit
  ): TC[Prod[Id]] = mkDivide[TC, Prod[Id]](identity)
  def apply[TC[_]](
      implicit a: Apply[TC],
      a0: TC[A1#Prod[Id]],
      a1: TC[A2#Prod[Id]],
      a2: TC[A3#Prod[Id]],
      a3: TC[A4#Prod[Id]],
      a4: TC[A5#Prod[Id]],
      a5: TC[A6#Prod[Id]],
      a6: TC[A7#Prod[Id]],
      a7: TC[A8#Prod[Id]],
      a8: TC[A9#Prod[Id]],
      a9: TC[A10#Prod[Id]],
      a10: TC[A11#Prod[Id]],
      a11: TC[A12#Prod[Id]],
      a12: TC[A13#Prod[Id]],
      a13: TC[A14#Prod[Id]],
      a14: TC[A15#Prod[Id]],
      a15: TC[A16#Prod[Id]],
      a16: TC[A17#Prod[Id]],
      a17: TC[A18#Prod[Id]],
      a18: TC[A19#Prod[Id]],
      a19: TC[A20#Prod[Id]],
      a20: TC[A21#Prod[Id]],
      dummy: DummyImplicit
  ): TC[Prod[Id]] = mkApply[TC, Prod[Id]](identity)

  object evidence extends AndXorEvidence[Cop, Prod] {
    implicit def injEv[F[_]]: Inj[Cop[F], Cop[F]] = choose[Inj[Cop[F], ?], F]
    implicit def liftEv[F[_]](implicit M: Monoid[Prod[F]]): Inj[Prod[F], Prod[F]] = divide[Inj[Prod[F], ?], F]
  }

  def transformP[F[_], G[_]](nt: (F ~> G))(
      implicit trans0: Transform[A1#Prod],
      trans1: Transform[A2#Prod],
      trans2: Transform[A3#Prod],
      trans3: Transform[A4#Prod],
      trans4: Transform[A5#Prod],
      trans5: Transform[A6#Prod],
      trans6: Transform[A7#Prod],
      trans7: Transform[A8#Prod],
      trans8: Transform[A9#Prod],
      trans9: Transform[A10#Prod],
      trans10: Transform[A11#Prod],
      trans11: Transform[A12#Prod],
      trans12: Transform[A13#Prod],
      trans13: Transform[A14#Prod],
      trans14: Transform[A15#Prod],
      trans15: Transform[A16#Prod],
      trans16: Transform[A17#Prod],
      trans17: Transform[A18#Prod],
      trans18: Transform[A19#Prod],
      trans19: Transform[A20#Prod],
      trans20: Transform[A21#Prod]
  ): Prod[F] => Prod[G] =
    Transform[Prod].transform(nt)

  def transformC[F[_], G[_]](nt: (F ~> G))(
      implicit trans0: Transform[A1#Cop],
      trans1: Transform[A2#Cop],
      trans2: Transform[A3#Cop],
      trans3: Transform[A4#Cop],
      trans4: Transform[A5#Cop],
      trans5: Transform[A6#Cop],
      trans6: Transform[A7#Cop],
      trans7: Transform[A8#Cop],
      trans8: Transform[A9#Cop],
      trans9: Transform[A10#Cop],
      trans10: Transform[A11#Cop],
      trans11: Transform[A12#Cop],
      trans12: Transform[A13#Cop],
      trans13: Transform[A14#Cop],
      trans14: Transform[A15#Cop],
      trans15: Transform[A16#Cop],
      trans16: Transform[A17#Cop],
      trans17: Transform[A18#Cop],
      trans18: Transform[A19#Cop],
      trans19: Transform[A20#Cop],
      trans20: Transform[A21#Cop]
  ): Cop[F] => Cop[G] =
    Transform[Cop].transform(nt)

  def sequenceP[F[_]](p: Prod[F])(
      implicit F: Apply[F],
      seq0: Sequence[A1#Prod, Apply],
      seq1: Sequence[A2#Prod, Apply],
      seq2: Sequence[A3#Prod, Apply],
      seq3: Sequence[A4#Prod, Apply],
      seq4: Sequence[A5#Prod, Apply],
      seq5: Sequence[A6#Prod, Apply],
      seq6: Sequence[A7#Prod, Apply],
      seq7: Sequence[A8#Prod, Apply],
      seq8: Sequence[A9#Prod, Apply],
      seq9: Sequence[A10#Prod, Apply],
      seq10: Sequence[A11#Prod, Apply],
      seq11: Sequence[A12#Prod, Apply],
      seq12: Sequence[A13#Prod, Apply],
      seq13: Sequence[A14#Prod, Apply],
      seq14: Sequence[A15#Prod, Apply],
      seq15: Sequence[A16#Prod, Apply],
      seq16: Sequence[A17#Prod, Apply],
      seq17: Sequence[A18#Prod, Apply],
      seq18: Sequence[A19#Prod, Apply],
      seq19: Sequence[A20#Prod, Apply],
      seq20: Sequence[A21#Prod, Apply]
  ): F[Prod[Id]] =
    Sequence[Prod, Apply].sequence(p)

  def sequenceC[F[_]](c: Cop[F])(
      implicit F: Functor[F],
      seq0: Sequence[A1#Cop, Functor],
      seq1: Sequence[A2#Cop, Functor],
      seq2: Sequence[A3#Cop, Functor],
      seq3: Sequence[A4#Cop, Functor],
      seq4: Sequence[A5#Cop, Functor],
      seq5: Sequence[A6#Cop, Functor],
      seq6: Sequence[A7#Cop, Functor],
      seq7: Sequence[A8#Cop, Functor],
      seq8: Sequence[A9#Cop, Functor],
      seq9: Sequence[A10#Cop, Functor],
      seq10: Sequence[A11#Cop, Functor],
      seq11: Sequence[A12#Cop, Functor],
      seq12: Sequence[A13#Cop, Functor],
      seq13: Sequence[A14#Cop, Functor],
      seq14: Sequence[A15#Cop, Functor],
      seq15: Sequence[A16#Cop, Functor],
      seq16: Sequence[A17#Cop, Functor],
      seq17: Sequence[A18#Cop, Functor],
      seq18: Sequence[A19#Cop, Functor],
      seq19: Sequence[A20#Cop, Functor],
      seq20: Sequence[A21#Cop, Functor]
  ): F[Cop[Id]] =
    Sequence[Cop, Functor].sequence(c)

  def extractC[F[_], B](c: Cop[F])(implicit inj: Inj[Option[B], Cop[F]]): Option[B] = inj(c)

  def extractP[F[_], B](p: Prod[F])(implicit inj: Inj[B, Prod[F]]): B = inj(p)

  // format: off
  def foldMap[F[_], C](p: Prod[F])(map: Cop[Id] => C)(
    implicit O: Ordering[Cop[Id]],
    M: Monoid[C],
    PE: PlusEmpty[F],
    U: Uncons[F],
    U0: Uncons0[A1#Prod, A1#Cop], U1: Uncons0[A2#Prod, A2#Cop], U2: Uncons0[A3#Prod, A3#Cop], U3: Uncons0[A4#Prod, A4#Cop], U4: Uncons0[A5#Prod, A5#Cop], U5: Uncons0[A6#Prod, A6#Cop], U6: Uncons0[A7#Prod, A7#Cop], U7: Uncons0[A8#Prod, A8#Cop], U8: Uncons0[A9#Prod, A9#Cop], U9: Uncons0[A10#Prod, A10#Cop], U10: Uncons0[A11#Prod, A11#Cop], U11: Uncons0[A12#Prod, A12#Cop], U12: Uncons0[A13#Prod, A13#Cop], U13: Uncons0[A14#Prod, A14#Cop], U14: Uncons0[A15#Prod, A15#Cop], U15: Uncons0[A16#Prod, A16#Cop], U16: Uncons0[A17#Prod, A17#Cop], U17: Uncons0[A18#Prod, A18#Cop], U18: Uncons0[A19#Prod, A19#Cop], U19: Uncons0[A20#Prod, A20#Cop], U20: Uncons0[A21#Prod, A21#Cop]
  ): C = {
    import scala.collection.mutable.{PriorityQueue => PQ}

    def uncons(p: Prod[F]): (List[Cop[Id]], Prod[F]) = {
      val (h1, t1) = U0(p.t1)
      val (h2, t2) = U1(p.t2)
      val (h3, t3) = U2(p.t3)
      val (h4, t4) = U3(p.t4)
      val (h5, t5) = U4(p.t5)
      val (h6, t6) = U5(p.t6)
      val (h7, t7) = U6(p.t7)
      val (h8, t8) = U7(p.t8)
      val (h9, t9) = U8(p.t9)
      val (h10, t10) = U9(p.t10)
      val (h11, t11) = U10(p.t11)
      val (h12, t12) = U11(p.t12)
      val (h13, t13) = U12(p.t13)
      val (h14, t14) = U13(p.t14)
      val (h15, t15) = U14(p.t15)
      val (h16, t16) = U15(p.t16)
      val (h17, t17) = U16(p.t17)
      val (h18, t18) = U17(p.t18)
      val (h19, t19) = U18(p.t19)
      val (h20, t20) = U19(p.t20)
      val (h21, t21) = U20(p.t21)
      (List(h1.map(inj(_: A1#Cop[Id])), h2.map(inj(_: A2#Cop[Id])), h3.map(inj(_: A3#Cop[Id])), h4.map(inj(_: A4#Cop[Id])), h5.map(inj(_: A5#Cop[Id])), h6.map(inj(_: A6#Cop[Id])), h7.map(inj(_: A7#Cop[Id])), h8.map(inj(_: A8#Cop[Id])), h9.map(inj(_: A9#Cop[Id])), h10.map(inj(_: A10#Cop[Id])), h11.map(inj(_: A11#Cop[Id])), h12.map(inj(_: A12#Cop[Id])), h13.map(inj(_: A13#Cop[Id])), h14.map(inj(_: A14#Cop[Id])), h15.map(inj(_: A15#Cop[Id])), h16.map(inj(_: A16#Cop[Id])), h17.map(inj(_: A17#Cop[Id])), h18.map(inj(_: A18#Cop[Id])), h19.map(inj(_: A19#Cop[Id])), h20.map(inj(_: A20#Cop[Id])), h21.map(inj(_: A21#Cop[Id]))).flatten,
        Prod[F]((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21)))
    }

    @tailrec
    def appendAll(out: C, q: PQ[Cop[Id]]): C =
      q.isEmpty match {
        case true => out
        case false =>
          val newOut = M.append(out, map(q.dequeue))
          appendAll(newOut, q)
      }

    @tailrec
    def go(prod: Prod[F], q: PQ[Cop[Id]], out: C): C =
      (prod.run.==((PE.empty[A1], PE.empty[A2], PE.empty[A3], PE.empty[A4], PE.empty[A5], PE.empty[A6], PE.empty[A7], PE.empty[A8], PE.empty[A9], PE.empty[A10], PE.empty[A11], PE.empty[A12], PE.empty[A13], PE.empty[A14], PE.empty[A15], PE.empty[A16], PE.empty[A17], PE.empty[A18], PE.empty[A19], PE.empty[A20], PE.empty[A21]))) match {
        case true => appendAll(out, q)
        case false => q.isEmpty match {
          case true => {
            val (hs, ts) = uncons(prod)
            q ++= hs
            go(ts, q, out)
          }
          case false => q.dequeue.run match {
            case dj @ -\/(_) =>
              val (h, t) = U0(prod.t1)
              go(Prod[F]((t, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A1#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(-\/(_)) =>
              val (h, t) = U1(prod.t2)
              go(Prod[F]((prod.t1, t, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A2#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(-\/(_))) =>
              val (h, t) = U2(prod.t3)
              go(Prod[F]((prod.t1, prod.t2, t, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A3#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(-\/(_)))) =>
              val (h, t) = U3(prod.t4)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, t, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A4#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(-\/(_))))) =>
              val (h, t) = U4(prod.t5)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, t, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A5#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(-\/(_)))))) =>
              val (h, t) = U5(prod.t6)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, t, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A6#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))) =>
              val (h, t) = U6(prod.t7)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, t, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A7#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))) =>
              val (h, t) = U7(prod.t8)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, t, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A8#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))) =>
              val (h, t) = U8(prod.t9)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, t, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A9#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))) =>
              val (h, t) = U9(prod.t10)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, t, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A10#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))) =>
              val (h, t) = U10(prod.t11)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, t, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A11#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))))) =>
              val (h, t) = U11(prod.t12)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, t, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A12#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))))) =>
              val (h, t) = U12(prod.t13)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, t, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A13#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))))))) =>
              val (h, t) = U13(prod.t14)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, t, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A14#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))))))) =>
              val (h, t) = U14(prod.t15)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, t, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A15#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))))))))) =>
              val (h, t) = U15(prod.t16)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, t, prod.t17, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A16#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))))))))) =>
              val (h, t) = U16(prod.t17)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, t, prod.t18, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A17#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))))))))))) =>
              val (h, t) = U17(prod.t18)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, t, prod.t19, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A18#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_))))))))))))))))))) =>
              val (h, t) = U18(prod.t19)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, t, prod.t20, prod.t21)),
                q ++= h.map(inj(_: A19#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(-\/(_)))))))))))))))))))) =>
              val (h, t) = U19(prod.t20)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, t, prod.t21)),
                q ++= h.map(inj(_: A20#Cop[Id])), M.append(out, map(Cop[Id](dj))))
            case dj @ \/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(\/-(_)))))))))))))))))))) =>
              val (h, t) = U20(prod.t21)
              go(Prod[F]((prod.t1, prod.t2, prod.t3, prod.t4, prod.t5, prod.t6, prod.t7, prod.t8, prod.t9, prod.t10, prod.t11, prod.t12, prod.t13, prod.t14, prod.t15, prod.t16, prod.t17, prod.t18, prod.t19, prod.t20, t)),
                q ++= h.map(inj(_: A21#Cop[Id])), M.append(out, map(Cop[Id](dj))))

          }
        }
      }
    val Q = new PQ[Cop[Id]]()(O)
    val (hs, ts) = uncons(p)
    Q ++= hs
    go(ts, Q, M.zero)
  }
  // format: on
}

object AndXor21 {
  def apply[
      A1 <: AndXor,
      A2 <: AndXor,
      A3 <: AndXor,
      A4 <: AndXor,
      A5 <: AndXor,
      A6 <: AndXor,
      A7 <: AndXor,
      A8 <: AndXor,
      A9 <: AndXor,
      A10 <: AndXor,
      A11 <: AndXor,
      A12 <: AndXor,
      A13 <: AndXor,
      A14 <: AndXor,
      A15 <: AndXor,
      A16 <: AndXor,
      A17 <: AndXor,
      A18 <: AndXor,
      A19 <: AndXor,
      A20 <: AndXor,
      A21 <: AndXor
  ]: AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
    new AndXor21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}
}
