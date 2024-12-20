package andxor

import cats.{Apply, Id}
import cats.syntax.eq.*
import scala.annotation.tailrec
import scala.deriving.Mirror
import scala.compiletime.summonAll

type ReduceSum[Types <: Tuple, F[_]] = Types match {
  case h *: EmptyTuple => F[h]
  case h *: t => F[h] |: ReduceSum[t, F]
}

type ZipWith[T1 <: Tuple, T2 <: Tuple, F[_, _]] <: Tuple = (T1, T2) match {
  case (h1 *: t1, h2 *: t2) => F[h1, h2] *: ZipWith[t1, t2, F]
  case (EmptyTuple, _) => EmptyTuple
  case (_, EmptyTuple) => EmptyTuple
  case _ => Tuple
}

sealed trait AndXorIso { self =>
  type Axo
  type Cop[f[_]]
  type Prod[f[_]] <: Tuple
  protected final type Axo0 = Axo & AndXorNonEmpty {
    type Cop[f[_]] = self.Cop[f]
    type Prod[f[_]] = self.Prod[f]
  }

  type LabelledAxo
  type LabelledCop[f[_]]
  type LabelledProd[f[_]] <: Tuple
  protected final type LabelledAxo0 = LabelledAxo & AndXorNonEmpty {
    type Cop[f[_]] = self.LabelledCop[f]
    type Prod[f[_]] = self.LabelledProd[f]
  }
  val andxor: Axo0
  final lazy val andxorLabelled: LabelledAxo0 = andxor.asInstanceOf[LabelledAxo0]
}

object AndXorIso {
  sealed trait Types[ElemLabels <: Tuple, ElemTypes <: Tuple] extends AndXorIso {
    final type Axo = Tuple.Fold[ElemTypes, AndXorEmpty, AndXor.Prepend]
    final type Cop[F[_]] = ReduceSum[ElemTypes, F]
    final type Prod[F[_]] = Tuple.Map[ElemTypes, F]

    type LabelledAxo = Tuple.Fold[ZipWith[ElemTypes, ElemLabels, Labelled], AndXorEmpty, AndXor.Prepend]
    type LabelledCop[F[_]] = ReduceSum[ZipWith[ElemTypes, ElemLabels, Labelled], F]
    type LabelledProd[F[_]] = Tuple.Map[ZipWith[ElemTypes, ElemLabels, Labelled], F]
  }
}

sealed trait AndXorProdIso[X] extends AndXorIso {
  val iso: Iso[X, Prod[Id]]
  final lazy val isoLabelled: Iso[X, LabelledProd[Id]] = iso.asInstanceOf[Iso[X, LabelledProd[Id]]]

  final def deriving[TC[_]](implicit I: AndXorInstances[TC, Prod[Id]]): AndXorDeriving[TC, Cop[Id], X] =
    andxor.derivingId[TC].imapProd(iso.reverseGet)(iso.get)

  final def derivingCovariant[TC[_]](implicit I: AndXorInstances[TC, Prod[Id]], A: Apply[TC]): TC[X] =
    deriving[TC].apply

  final def derivingContravariant[TC[_]](implicit I: AndXorInstances[TC, Prod[Id]], D: Divide[TC]): TC[X] =
    deriving[TC].divide

  final def derivingLabelled[TC[_]](implicit I: AndXorInstances[TC, LabelledProd[Id]]): AndXorDeriving[TC, LabelledCop[Id], X] =
    andxorLabelled.derivingId[TC].imapProd(isoLabelled.reverseGet)(isoLabelled.get)

  final def derivingLabelledCovariant[TC[_]](implicit I: AndXorInstances[TC, LabelledProd[Id]], A: Apply[TC]): TC[X] =
    derivingLabelled[TC].apply

  final def derivingLabelledContravariant[TC[_]](implicit I: AndXorInstances[TC, LabelledProd[Id]], D: Divide[TC]): TC[X] =
    derivingLabelled[TC].divide
}

object AndXorProdIso {
  type Aux[X, A, C[_[_]], P[_[_]] <: Tuple, LA, LC[_[_]], LP[_[_]] <: Tuple] = AndXorProdIso[X] {
    type Axo = A
    type Cop[f[_]] = C[f]
    type Prod[f[_]] = P[f]
    type LabelledAxo = LA
    type LabelledCop[f[_]] = LC[f]
    type LabelledProd[f[_]] = LP[f]
  }

  @inline final def apply[X](implicit p: AndXorProdIso[X]): Aux[X, p.Axo, p.Cop, p.Prod, p.LabelledAxo, p.LabelledCop, p.LabelledProd] =
    p

  private[AndXorProdIso] final class Inst[X, ElemLabels <: Tuple, ElemTypes <: Tuple] (
    axosT: Tuple.Map[ElemTypes, AndXor1],
    toTuple: X => ElemTypes,
    fromTuple: ElemTypes => X,
  ) extends AndXorProdIso[X], AndXorIso.Types[ElemLabels, ElemTypes] {
    private val axos = axosT.productIterator.toList

    val andxor: Axo0 = axos.init.foldRight(axos.last.asInstanceOf[AndXorNonEmpty])(
      (x, acc) => (x.asInstanceOf[AndXor1[Any]] *: acc).asInstanceOf[AndXorNonEmpty]).asInstanceOf[Axo0]

    val iso: Iso[X, Prod[Id]] =
      Iso(toTuple(_: X).asInstanceOf[Prod[Id]])((p: Prod[Id]) => fromTuple(p.asInstanceOf[ElemTypes]))
  }

  inline given inst[X <: Product](using m: Mirror.ProductOf[X]): Aux[
    X,
    Tuple.Fold[m.MirroredElemTypes, AndXorEmpty, AndXor.Prepend],
    [F[_]] =>> ReduceSum[m.MirroredElemTypes, F],
    [F[_]] =>> Tuple.Map[m.MirroredElemTypes, F],
    Tuple.Fold[ZipWith[m.MirroredElemTypes, m.MirroredElemLabels, Labelled], AndXorEmpty, AndXor.Prepend],
    [F[_]] =>> ReduceSum[ZipWith[m.MirroredElemTypes, m.MirroredElemLabels, Labelled], F],
    [F[_]] =>> Tuple.Map[ZipWith[m.MirroredElemTypes, m.MirroredElemLabels, Labelled], F],
  ] =
    new Inst[X, m.MirroredElemLabels, m.MirroredElemTypes](
      summonAll[Tuple.Map[m.MirroredElemTypes, AndXor1]],
      Tuple.fromProductTyped,
      m.fromTuple,
    )
}

sealed trait AndXorCopIso[X] extends AndXorIso {
  val iso: Iso[X, Cop[Id]]
  final lazy val isoLabelled: Iso[X, LabelledCop[Id]] = iso.asInstanceOf[Iso[X, LabelledCop[Id]]]

  final def deriving[TC[_]](implicit I: AndXorInstances[TC, Prod[Id]]): AndXorDeriving[TC, X, Prod[Id]] =
    andxor.derivingId[TC].imapCop(iso.reverseGet)(iso.get)

  final def derivingCovariant[TC[_]](implicit I: AndXorInstances[TC, Prod[Id]], A: Alt[TC]): TC[X] =
    deriving[TC].alt

  final def derivingContravariant[TC[_]](implicit I: AndXorInstances[TC, Prod[Id]], D: Decidable[TC]): TC[X] =
    deriving[TC].choose

  final def derivingLabelled[TC[_]](implicit I: AndXorInstances[TC, LabelledProd[Id]]): AndXorDeriving[TC, X, LabelledProd[Id]] =
    andxorLabelled.derivingId[TC].imapCop(isoLabelled.reverseGet)(isoLabelled.get)

  final def derivingLabelledCovariant[TC[_]](implicit I: AndXorInstances[TC, LabelledProd[Id]], A: Alt[TC]): TC[X] =
    derivingLabelled[TC].alt

  final def derivingLabelledContravariant[TC[_]](implicit I: AndXorInstances[TC, LabelledProd[Id]], D: Decidable[TC]): TC[X] =
    derivingLabelled[TC].choose
}

object AndXorCopIso {
  type Aux[X, A, C[_[_]], P[_[_]] <: Tuple, LA, LC[_[_]], LP[_[_]] <: Tuple] = AndXorCopIso[X] {
    type Axo = A
    type Cop[f[_]] = C[f]
    type Prod[f[_]] = P[f]
    type LabelledAxo = LA
    type LabelledCop[f[_]] = LC[f]
    type LabelledProd[f[_]] = LP[f]
  }

  @inline final def apply[X](implicit c: AndXorCopIso[X]): Aux[X, c.Axo, c.Cop, c.Prod, c.LabelledAxo, c.LabelledCop, c.LabelledProd] =
    c

  private[AndXorCopIso] final class Inst[X, ElemLabels <: Tuple, ElemTypes <: Tuple] (
    axosT: Tuple.Map[ElemTypes, AndXor1],
    ordinal: X => Int,
  ) extends AndXorCopIso[X], AndXorIso.Types[ElemLabels, ElemTypes] {
    private val axos = axosT.productIterator.toList
    private val numMembers = axos.length

    val andxor: Axo0 = axos.init.foldRight(axos.last.asInstanceOf[AndXorNonEmpty])(
      (x, acc) => (x.asInstanceOf[AndXor1[Any]] *: acc).asInstanceOf[AndXorNonEmpty]).asInstanceOf[Axo0]

    @tailrec private def unEither(x: Matchable): Any = x match {
      case Left(l) => unEither(l)
      case Right(r) => unEither(r)
      case _ => x
    }

    val iso: Iso[X, Cop[Id]] = Iso((x: X) => {
      val ord = ordinal(x)
      val init: Any = if (ord === numMembers - 1) x else Left(x)
      1.to(ord).foldRight(init)((_, acc) => Right(acc)).asInstanceOf[Cop[Id]]
    })(unEither(_: Cop[Id]).asInstanceOf[X])
  }

  inline given inst[X](using m: Mirror.SumOf[X]): Aux[
    X,
    Tuple.Fold[m.MirroredElemTypes, AndXorEmpty, AndXor.Prepend],
    [F[_]] =>> ReduceSum[m.MirroredElemTypes, F],
    [F[_]] =>> Tuple.Map[m.MirroredElemTypes, F],
    Tuple.Fold[ZipWith[m.MirroredElemTypes, m.MirroredElemLabels, Labelled], AndXorEmpty, AndXor.Prepend],
    [F[_]] =>> ReduceSum[ZipWith[m.MirroredElemTypes, m.MirroredElemLabels, Labelled], F],
    [F[_]] =>> Tuple.Map[ZipWith[m.MirroredElemTypes, m.MirroredElemLabels, Labelled], F],
  ] =
    new Inst[X, m.MirroredElemLabels, m.MirroredElemTypes](summonAll[Tuple.Map[m.MirroredElemTypes, AndXor1]], m.ordinal)
}
