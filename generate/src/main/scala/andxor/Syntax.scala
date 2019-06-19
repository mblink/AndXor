package andxor

import scalaz.Zipper
import scalaz.syntax.comonad._
import scalaz.syntax.std.boolean._

object syntax {
  val tupleLen = 22

  def range(start: Int, end: Int): List[Int] = start.to(end).toList

  val parens = (s: String) => s"($s)"

  type LS = List[String]

  def mkTpeList(start: Int, end: Int): List[LS] = start.to(end).toList.map(1.to(_).toList.map(x => s"A${x}"))

  implicit class TpesOps(tpes: LS) {
    def selCopOrProd(copOrProd: String, F: Option[String]): LS =
      (copOrProd == "Prod").fold(prodTpes(F), copTpes(F))

    def selCop(F: Option[String]): LS = selCopOrProd("Cop", F)
    def selProd(F: Option[String]): LS = selCopOrProd("Prod", F)

    def copName = s"Cop${tpes.length}"
    def copTpeDef = s"$copName[F[_], $rank2TpeParams]"
    def copTpeF(F: String) = foldLen01(s"$tpeParams[$F]")(s"$copName[$F, $tpeParams]")
    def copTpe = copTpeF("F")
    def copTpes(F: Option[String] = Some("F")): LS = tpes.map(t => s"${t}${F.fold("")(f => s"[$f]")}")

    def wrapCopVal(v: String, F: String = "F"): String = foldLen01(v)(s"${copTpeF(F)}($v)")

    def prodName = s"Prod${tpes.length}"
    def prodTpeDef = s"$prodName[F[_], $rank2TpeParams]"
    def prodTpeF(F: String) = foldLen01(s"$tpeParams[$F]")(s"$prodName[$F, $tpeParams]")
    def prodTpe = prodTpeF("F")
    def prodTpes(F: Option[String] = Some("F")): LS = tpes.map(t => s"${t}${F.fold("")(f => s"[$f]")}")

    def wrapProdVal(v: String, F: String = "F"): String = foldLen01(v)(s"${prodTpeF(F)}($v)")

    def tcDeps(copOrProd: String, TC: String = "TC", F: String = "F"): String =
      foldLen01(paramSig(List(TC) ++ (F == "Id").fold(Nil, List(F)), "a"))(selCopOrProd(copOrProd, Some(F)).paramSig(TC, "a"))

    def djBase(wrapTpe: String => String): String =
      tpes.init.foldRight(wrapTpe(tpes.last))((e, a) => s"(${wrapTpe(e)} \\/ $a)")

    def dj: String = djBase(identity _)
    def djK(F: String): String = djBase(t => foldLen01(s"$t[$F]")(s"$t[$F]"))

    def mkTuple: String = if (tpes.length <= 1) tpes.mkString(", ") else parens(tpes.mkString(", "))

    def tupleVals(a: String, v: String, spaces: String, sIx: Int = 0): String =
      paramList(a, sIx).zipWithIndex.map(t => s"val ${t._1} = ${v}${tupleAccess(t._2 + 1)}").mkString(s"\n$spaces")

    def prodBase(wrapTpe: String => String): String =
      tpes.map(wrapTpe).mkTuple

    def prod: String = prodBase(identity _)
    def prodK(F: String): String = prodBase(t => foldLen01(s"$t[$F]")(s"$t[$F]"))

    def tpeParams: String = tpes.mkString(", ")
    def tpeParamsF(F: String): String = tpes.map(s => s"$F[$s]").mkString(", ")

    def rank2TpeParamsList: List[String] = tpes.map(t => s"$t[_[_]]")
    def rank2TpeParams: String = rank2TpeParamsList.tpeParams

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

    def derivingIsoParams(copOrProd: String, TC: String, F: String): LS =
      foldLen[LS](Nil)(List(s"TC: $TC[$F[${tpes.head}]]"))(selCopOrProd(copOrProd, None)
        .map(t => s"DerivingIso[$t, $F, $TC]").paramSigArgs("deriving"))

    def ffunctorParams(copOrProd: String): LS =
      foldLen01[LS](Nil)(selCopOrProd(copOrProd, None).paramSigArgs("FFunctor", "ff"))

    def ftraverseParams(copOrProd: String): LS =
      foldLen01[LS](Nil)(selCopOrProd(copOrProd, None).paramSigArgs("FTraverse", "ft"))

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

    def builtAndXor(extra: String): String = s"AndXor${extra}${tpes.length}[${tpes.tpeParams}]"

    def const: LS = tpes.map(t => s"FConst[?[_], $t]")

    def flatToNestedProd: String =
      tpes.init.zipWithIndex.foldRight(s"p._${tpes.length}")((t, a) =>
        s"${tpes.takeRight(tpes.length - t._2).prodTpe}((p._${t._2 + 1}, $a))")

    def flatToNestedCop: String =
      tpes.init.zipWithIndex.foldRight(s"x${tpes.length - 1}")((t, a) =>
        s"${tpes.takeRight(tpes.length - t._2).copTpe}(${(t._2 == 0).fold("c", s"x${t._2}")}${
          (t._2 == tpes.length - 2).fold("", s".map(x${t._2 + 1} => $a)")})")
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

    def prodAccesses(v: String): LS =
      1.to(z.length).toList.map(i => s"${v}${z.toList.prodAccess(i)}")
  }
}
