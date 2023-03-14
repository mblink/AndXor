package andxor

import cats.{Apply, Id}
import cats.syntax.eq.*
import monocle.Iso
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
  protected final type Axo0 = Axo & AndXor.NonEmpty {
    type Cop[f[_]] = self.Cop[f]
    type Prod[f[_]] = self.Prod[f]
  }

  type LabelledAxo
  type LabelledCop[f[_]]
  type LabelledProd[f[_]] <: Tuple
  protected final type LabelledAxo0 = LabelledAxo & AndXor.NonEmpty {
    type Cop[f[_]] = self.LabelledCop[f]
    type Prod[f[_]] = self.LabelledProd[f]
  }
  val andxor: Axo0
  final lazy val andxorLabelled: LabelledAxo0 = andxor.asInstanceOf[LabelledAxo0]
}

object AndXorIso {
  sealed trait Types[ElemLabels <: Tuple, ElemTypes <: Tuple] extends AndXorIso {
    final type Axo = Tuple.Fold[ElemTypes, AndXor.Empty, AndXor.MakeNext]
    final type Cop[F[_]] = ReduceSum[ElemTypes, F]
    final type Prod[F[_]] = Tuple.Map[ElemTypes, F]

    type LabelledAxo = Tuple.Fold[ZipWith[ElemTypes, ElemLabels, Labelled], AndXor.Empty, AndXor.MakeNext]
    type LabelledCop[F[_]] = ReduceSum[ZipWith[ElemTypes, ElemLabels, Labelled], F]
    type LabelledProd[F[_]] = Tuple.Map[ZipWith[ElemTypes, ElemLabels, Labelled], F]
  }
}

sealed trait AndXorProdIso[X] extends AndXorIso {
  val iso: Iso[X, Prod[Id]]
  final lazy val isoLabelled: Iso[X, LabelledProd[Id]] = iso.asInstanceOf[Iso[X, LabelledProd[Id]]]

  @inline final def deriving[TC[_]](implicit I: AndXorInstances[TC, Prod[Id]]): AndXorDeriving[TC, Cop[Id], X] =
    andxor.derivingId[TC].imapProd(iso.reverseGet)(iso.get)

  @inline final def derivingLabelled[TC[_]](implicit I: AndXorInstances[TC, LabelledProd[Id]]): AndXorDeriving[TC, LabelledCop[Id], X] =
    andxorLabelled.derivingId[TC].imapProd(isoLabelled.reverseGet)(isoLabelled.get)
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

  inline given inst[X <: Product](using m: Mirror.ProductOf[X]): Aux[
    X,
    Tuple.Fold[m.MirroredElemTypes, AndXor.Empty, AndXor.MakeNext],
    [F[_]] =>> ReduceSum[m.MirroredElemTypes, F],
    [F[_]] =>> Tuple.Map[m.MirroredElemTypes, F],
    Tuple.Fold[ZipWith[m.MirroredElemTypes, m.MirroredElemLabels, Labelled], AndXor.Empty, AndXor.MakeNext],
    [F[_]] =>> ReduceSum[ZipWith[m.MirroredElemTypes, m.MirroredElemLabels, Labelled], F],
    [F[_]] =>> Tuple.Map[ZipWith[m.MirroredElemTypes, m.MirroredElemLabels, Labelled], F],
  ] =
    new AndXorProdIso[X] with AndXorIso.Types[m.MirroredElemLabels, m.MirroredElemTypes] {
      private val axos = summonAll[Tuple.Map[m.MirroredElemTypes, AndXor._1]].productIterator.toList

      val andxor: Axo0 = axos.init.foldRight(axos.last.asInstanceOf[Axo0])(
        (x, acc) => (x.asInstanceOf[AndXor._1[Any]] *: acc).asInstanceOf[Axo0])

      val iso: Iso[X, Prod[Id]] = Iso(Tuple.fromProductTyped(_: X).asInstanceOf[Prod[Id]])(
        (p: Prod[Id]) => m.fromTuple(p.asInstanceOf[m.MirroredElemTypes]))
    }
}

sealed trait AndXorCopIso[X] extends AndXorIso {
  val iso: Iso[X, Cop[Id]]
  final lazy val isoLabelled: Iso[X, LabelledCop[Id]] = iso.asInstanceOf[Iso[X, LabelledCop[Id]]]

  @inline final def deriving[TC[_]](implicit I: AndXorInstances[TC, Prod[Id]]): AndXorDeriving[TC, X, Prod[Id]] =
    andxor.derivingId[TC].imapCop(iso.reverseGet)(iso.get)

  @inline final def derivingLabelled[TC[_]](implicit I: AndXorInstances[TC, LabelledProd[Id]]): AndXorDeriving[TC, X, LabelledProd[Id]] =
    andxorLabelled.derivingId[TC].imapCop(isoLabelled.reverseGet)(isoLabelled.get)
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

  inline given inst[X](using m: Mirror.SumOf[X]): Aux[
    X,
    Tuple.Fold[m.MirroredElemTypes, AndXor.Empty, AndXor.MakeNext],
    [F[_]] =>> ReduceSum[m.MirroredElemTypes, F],
    [F[_]] =>> Tuple.Map[m.MirroredElemTypes, F],
    Tuple.Fold[ZipWith[m.MirroredElemTypes, m.MirroredElemLabels, Labelled], AndXor.Empty, AndXor.MakeNext],
    [F[_]] =>> ReduceSum[ZipWith[m.MirroredElemTypes, m.MirroredElemLabels, Labelled], F],
    [F[_]] =>> Tuple.Map[ZipWith[m.MirroredElemTypes, m.MirroredElemLabels, Labelled], F],
  ] =
    new AndXorCopIso[X] with AndXorIso.Types[m.MirroredElemLabels, m.MirroredElemTypes] {
      private val axos = summonAll[Tuple.Map[m.MirroredElemTypes, AndXor._1]].productIterator.toList
      private val numMembers = axos.length

      val andxor: Axo0 = axos.init.foldRight(axos.last.asInstanceOf[Axo0])(
        (x, acc) => (x.asInstanceOf[AndXor._1[Any]] *: acc).asInstanceOf[Axo0])

      val iso: Iso[X, Cop[Id]] = Iso((x: X) => {
        val ord = m.ordinal(x)
        val init: Any = if (ord === numMembers - 1) x else Left(x)
        1.to(ord).foldRight(init)((_, acc) => Right(acc)).asInstanceOf[Cop[Id]]
      })((c: Cop[Id]) => c.asInstanceOf[X])
    }
}
