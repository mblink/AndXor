package andxor

import scala.collection.immutable.SortedSet
import scalaz.{\/, Zipper}
import scalaz.syntax.comonad._
import scalaz.syntax.either._
import scalaz.syntax.std.boolean._

object syntax {
  val tupleLen = 22

  def range(start: Int, end: Int): List[Int] = start.to(end).toList

  val parens = (s: String) => s"($s)"

  type LS = List[String]

  def mkTpeList(start: Int, end: Int): List[LS] = start.to(end).toList.map(1.to(_).toList.map(x => s"A${x}"))

  case class BoundedType(tpe: String, bound: String) { def tp: String = s"${tpe}${bound}" }
  implicit class BoundedTypeOps(tpes: List[BoundedType]) {
    def tpeParams: String = tpes.map(_.tp).mkString(", ")
  }

  implicit class TpeOps(s: String) {
    def bounded(bound: String): BoundedType = BoundedType(s, bound)
  }

  implicit class TpeDjOps(tpes: List[String \/ BoundedType]) {
    def tpeParams: String = tpes.map(_.map(_.tp).merge).mkString(", ")
  }

  implicit class TpesTpesOps(tpess: List[List[String]]) {
    def boundedPermutations(bound: String): Iterator[SortedSet[String \/ BoundedType]] =
      tpess.toIterator.flatMap { ts =>
        val tpes = ts.toSet
        tpes.subsets.map(bounded => SortedSet((tpes.diff(bounded).map(_.left[BoundedType]) ++
          bounded.map(BoundedType(_, bound).right[String])).toSeq:_*)(Ordering.by(_.map(_.tp).merge[String])))
      }
  }

  implicit class TpesOps(tpes: LS) {
    def selCopOrProd(copOrProd: String, F: Option[String]): LS =
      foldLen01(tpes.map(t => F.fold(t)(f => s"$f[$t]")))(tpes.map(t => s"$t#${copOrProd}${F.fold("")(f => s"[$f]")}"))

    def nestedCopOrProd(copOrProd: String, F: String): String =
      (tpes.length <= 2).fold(
        (copOrProd == "Prod").fold(prodTpeF(F), copTpeF(F)),
        s"${copOrProd}2[$F, ${tpes.head}, ${tpes.tail.reduceRight((t, a) => s"AndXor2[$t, $a]")}]")

    def selCop(F: Option[String]): LS = selCopOrProd("Cop", F)
    def selProd(F: Option[String]): LS = selCopOrProd("Prod", F)

    def copName = s"Cop${tpes.length}"
    def copTpeDef = s"$copName[F[_], $axoTpeParams]"
    def copTpeF(F: String) = s"$copName[$F, $tpeParams]"
    def copTpe = nestedCopOrProd("Cop", "F")
    def copTpes(F: String = "F"): LS = foldLen01(tpes.map(t => s"$F[$t]"))(tpes.map(t => s"$t#Cop[$F]"))

    def prodName = s"Prod${tpes.length}"
    def prodTpeDef = s"$prodName[F[_], $axoTpeParams]"
    def prodTpeF(F: String) = s"$prodName[$F, $tpeParams]"
    def prodTpe = nestedCopOrProd("Prod", "F")
    def prodTpes(F: String = "F"): LS = foldLen01(tpes.map(t => s"$F[$t]"))(tpes.map(t => s"$t#Prod[$F]"))

    def tcDeps(copOrProd: String, TC: String = "TC", F: String = "F"): String =
      foldLen01(paramSig(List(TC) ++ (F == "Id").fold(Nil, List(F)), "a"))(selCopOrProd(copOrProd, Some(F)).paramSig(TC, "a"))

    def djBase(wrapTpe: String => String): String =
      tpes.init.foldRight(wrapTpe(tpes.last))((e, a) => s"(${wrapTpe(e)} \\/ $a)")

    def dj: String = djBase(identity _)
    def djK(F: String): String = djBase(t => foldLen01(s"$F[$t]")(s"$t#Cop[$F]"))

    def mkTuple: String = if (tpes.length <= 1) tpes.mkString(", ") else parens(tpes.mkString(", "))

    def tupleVals(a: String, v: String, spaces: String, sIx: Int = 0): String =
      paramList(a, sIx).zipWithIndex.map(t => s"val ${t._1} = ${v}${tupleAccess(t._2 + 1)}").mkString(s"\n$spaces")

    def prodBase(wrapTpe: String => String): String =
      tpes.map(wrapTpe).mkTuple

    def prod: String = prodBase(identity _)
    def prodK(F: String): String = prodBase(t => foldLen01(s"$F[$t]")(s"$t#Prod[$F]"))

    def tpeParams: String = tpes.mkString(", ")
    def tpeParamsF(F: String): String = tpes.map(s => s"$F[$s]").mkString(", ")

    def hkTpeParams: String = tpes.map(t => s"$t[_[_]]").mkString(", ")

    def bounded(bound: String): List[BoundedType] = tpes.map(_.bounded(bound))

    def axoTpeParamsList: List[BoundedType] = bounded(foldLen01("")(" <: AndXor"))
    def axoTpeParams: String = axoTpeParamsList.tpeParams

    def paramSig(FG: LS, a: String): String =
      tpes.zipWithIndex.map(s => s"${a}${s._2}: ${FG.foldRight(s._1)((e, a) => s"${e}[${a}]")}").mkString(", ")

    def paramSigArgs(a: String): LS =
      tpes.zipWithIndex.map(s => s"${a}${s._2}: ${s._1}")

    def paramSigArgs(F: String, a: String): LS =
      tpes.map(t => s"$F[$t]").paramSigArgs(a)

    def paramSig(F: String, a: String): String =
      tpes.map(t => s"$F[$t]").paramSigArgs(a).mkString(", ")

    def paramSig(a: String): String =
      paramSigArgs(a).mkString(", ")

    def paramListF(f: Int => String, sIx: Int = 0): LS =
      tpes.zipWithIndex.map(t => f(t._2 + sIx))

    def paramList(a: String, sIx: Int = 0): LS =
      paramListF(i => s"${a}${i}", sIx)

    def params(a: String, sIx: Int = 0): String =
      paramList(a, sIx).mkString(", ")

    def derivingParams(copOrProd: String, TC: String, F: String): LS =
      foldLen[LS](Nil)(List(s"tc: $TC[$F[${tpes.head}]]"))(selCopOrProd(copOrProd, None)
        .map(t => s"Deriving$copOrProd[$t, $F, $TC]").paramSigArgs("deriving"))

    def transformParams(copOrProd: String): LS =
      foldLen01[LS](Nil)(selCopOrProd(copOrProd, None).paramSigArgs("Transform", "trans"))

    def sequenceParams(copOrProd: String): LS =
      foldLen01[LS](Nil)(selCopOrProd(copOrProd, None).map(t =>
        s"$t, ${if (copOrProd == "Prod") "Apply" else "Functor"}").paramSigArgs("Sequence", "seq"))

    def foldMapParams: LS =
      foldLen01[LS](Nil)(selProd(None).zip(selCop(None)).map(t => s"${t._1}, ${t._2}").paramSigArgs("FoldMap", "fm"))

    def asImpls(otherImpls: Boolean): String =
      tpes.isEmpty.fold("", otherImpls.fold(s", ${tpes.mkString(", ")}", s"(implicit ${tpes.mkString(", ")})"))

    def toZipper: Zipper[String] =
      Zipper.zipper(Stream.empty[String], tpes.head, tpes.tail.toStream)

    def zipper[A](fn: Zipper[String] => A): List[A] =
      toZipper.cobind(fn).toStream.toList

    def tupleAccess(idx: Int): String = foldLen01("")(s".t$idx")
    def prodAccess(idx: Int): String = foldLen01(".run")(tupleAccess(idx))
    def prodAccessSelf(idx: Int): String = prodAccess(idx).stripPrefix(".")

    def tupleAccessNoSyntax(idx: Int): String = foldLen01("")(s"._$idx")

    def foldLen0[A](eq0: => A)(gt0: => A): A = (tpes.length == 0).fold(eq0, gt0)
    def foldLen01[A](lteq1: => A)(gt1: => A): A = (tpes.length <= 1).fold(lteq1, gt1)
    def foldLen[A](eq0: => A)(eq1: => A)(gt1: => A): A = foldLen0[A](eq0)((tpes.length == 1).fold(eq1, gt1))

    def foldMapName(idx: Int): String = "fm" ++ foldLen01("")(idx.toString)

    def builtAndXor: String = tpes.zipWithIndex.map(t => s"l${t._2}.AXO").reduceRight((t, a) => s"AndXor2[$t, $a]")

    def andxorParent: String = (tpes.length <= 2).fold("AndXor", tpes.reduceRight((t, a) => s"AndXor2[$t, $a]"))

    def flatToNestedTuple(mkVal: Int => String, wrap: (List[String], String) => String): String =
      foldLen01("")(tpes.init.zipWithIndex.foldRight(mkVal(tpes.length - 1))(
        (t, a) => wrap(tpes.takeRight(tpes.length - t._2), s"(${mkVal(t._2)}, $a)")))

    // def flatToNestedDj(mkVal: Int => String, wrap: (List[String], String) => String): String =
    //   foldLen01("")(tpes.init.zipWithIndex.foldRight(mkVal(tpes.length - 1))(
    //     (t, a) => wrap()))
  }

  implicit class TpesWithIndexOps(tpes: List[(String, Int)]) {
    def prod: String = tpes.map(_._1).prod
  }

  implicit class ZipperOps(z: Zipper[String]) {
    def toList: LS = z.toStream.toList

    def djVal(v: String): String =
      z.lefts.foldLeft(Some(z.rights).filter(_.nonEmpty).map(_ => s"-\\/($v)").getOrElse(v))((a, _) => s"\\/-($a)")

    def djFold(v: String, fail: String => String, succ: String => String): String = {
      val init = z.rights.nonEmpty.fold((x: String) => s"$x.fold(l => ${succ("l")}, r => ${fail("r")})", fail)
      val folds = 0.to(z.lefts.length - 1).foldLeft(init) { (acc, i) =>
        val (l, r) = (s"a$i", s"a${i + 1}")
        (x: String) => s"$x.fold($l => ${fail(l)}, $r => ${acc(r)})"
      }
      folds(v)
    }

    def wrapProdOrTuple(prod: Boolean)(inner: String): String =
      s"${prod.fold(s"${z.modify(_ => "B").toList.prodTpe}(", "")}${inner}${prod.fold(")", "")}"

    def wrapCopOrTuple(cop: Boolean)(inner: String): String =
      s"${cop.fold(s"${z.modify(_ => "B").toList.copTpe}(", "")}${inner}${cop.fold(")", "")}"

    def dummyImpl(used: Boolean): String =
      s"(implicit ${used.fold("", "@scalaz.unused ")}d: Dummy${z.index + 1})"
  }
}
