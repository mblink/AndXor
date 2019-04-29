package andxor

import scalaz.Zipper
import scalaz.syntax.comonad._
// import scalaz.syntax.id._
import scalaz.syntax.std.boolean._

object syntax {
  val tupleLen = 22

  def range(start: Int, end: Int): List[Int] = start.to(end).toList

  def parens(s: String): String = s"($s)"

  type LS = List[String]

  sealed trait ScalazTC {
    val tpes: List[String]
    val tpeName: String
    val name: String
    val combineF: String
    val identityF: String
    val secondParams: String
  }
  case class ApplyTC(tpes: List[String]) extends ScalazTC {
    val tpeName = tpes.prodName
    val name = "Apply"
    val combineF = "apply"
    val identityF = "map"
    val secondParams = s" { case ${tpes.paramList("i").mkTuple} => ${tpes.prodTpe}(${tpes.paramList("i").mkTuple}) }"
  }
  case class AltTC(tpes: List[String]) extends ScalazTC {
    val tpeName = tpes.copName
    val name = "Alt"
    val combineF = "altly"
    val identityF = "map"
    val secondParams = s"(${tpes.copTpe}(_))"
  }
  case class DecidableTC(tpes: List[String]) extends ScalazTC {
    val tpeName = tpes.copName
    val name = "Decidable"
    val combineF = "choose"
    val identityF = "contramap"
    val secondParams = "(_.run)"
  }
  case class DivideTC(tpes: List[String]) extends ScalazTC {
    val tpeName = tpes.prodName
    val name = "Divide"
    val combineF = "divide"
    val identityF = "contramap"
    val secondParams = "(_.run)"
  }

  implicit class TpesOps(tpes: LS) {
    def copName = s"Cop${tpes.length}"
    def copTpeDef = s"$copName[F[_], $tpeParams]"
    def copTpeF(F: String) = s"$copName[$F, $tpeParams]"
    def copTpe = copTpeF("F")

    def prodName = s"Prod${tpes.length}"
    def prodTpeDef = s"$prodName[F[_], $tpeParams]"
    def prodTpeF(F: String) = s"$prodName[$F, $tpeParams]"
    def prodTpe = prodTpeF("F")

    def djBase(wrapTpe: String => String): String =
      tpes.init.foldRight(wrapTpe(tpes.last))((e, a) => s"(${wrapTpe(e)} \\/ $a)")

    def dj: String = djBase(identity _)
    def djK(F: String): String = djBase(t => s"$F[$t]")

    def mkTuple: String = if (tpes.length <= 1) tpes.mkString(", ") else parens(tpes.mkString(", "))
      // tpes.reduceRight((tpe, acc) => parens(tpe ++ ", " ++ acc))

    def tupleVals(a: String, v: String, spaces: String, sIx: Int = 0): String =
      paramList(a, sIx).zipWithIndex.map(t => s"val ${t._1} = ${v}${tupleAccess(t._2 + 1)}").mkString(s"\n$spaces")

    def prodBase(wrapTpe: String => String): String =
      tpes.map(wrapTpe).mkTuple

    def prod: String = prodBase(identity _)
    def prodK(F: String): String = prodBase(t => s"$F[$t]")

    def tpeParams: String = tpes.mkString(", ")
    def tpeParamsF(F: String): String = tpes.map(s => s"$F[$s]").mkString(", ")

    def paramSig(FG: List[String], a: String): String =
      tpes.zipWithIndex.map(s => s"${a}${s._2}: ${FG.foldRight(s._1)((e, a) => s"${e}[${a}]")}").mkString(", ")

    def paramSig(F: String, a: String): String =
      tpes.zipWithIndex.map(s => s"${a}${s._2}: $F[${s._1}]").mkString(", ")

    def paramSig(a: String): String =
      tpes.zipWithIndex.map(s => s"${a}${s._2}: ${s._1}").mkString(", ")

    def paramListF(f: Int => String, sIx: Int = 0): List[String] =
      tpes.zipWithIndex.map(t => f(t._2 + sIx))

    def paramList(a: String, sIx: Int = 0): List[String] =
      paramListF(i => s"${a}${i}", sIx)

    def params(a: String, sIx: Int = 0): String =
      paramList(a, sIx).mkString(", ")

    def toZipper: Zipper[String] =
      Zipper.zipper(Stream.empty[String], tpes.head, tpes.tail.toStream)

    def zipper[A](fn: Zipper[String] => A): List[A] =
      toZipper.cobind(fn).toStream.toList

    def tupleAccess(idx: Int): String = foldLen01("")(s".t$idx")

    def tupleAccessNoSyntax(idx: Int): String = foldLen01("")(s"._$idx")
      // toZipper.move(idx - 1).get |> (z => z.lefts.foldLeft(Some(z.rights)
      //   .filter(_.nonEmpty).map(_ => "._1").getOrElse(""))((a, _) => s"._2$a"))

    def foldLen0[A](eq0: => A)(gt0: => A): A = (tpes.length == 0).fold(eq0, gt0)
    def foldLen01[A](lteq1: => A)(gt1: => A): A = (tpes.length <= 1).fold(lteq1, gt1)
    def foldLen[A](eq0: => A)(eq1: => A)(gt1: => A): A = foldLen0[A](eq0)((tpes.length == 1).fold(eq1, gt1))

    def tcDef(leadingWs: String, tc: ScalazTC, isId: Boolean): String = {
      val F = isId.fold("Id", "F")
      val tp = isId.fold("", s"F[_], ")
      val fNameBase = s"${tc.tpeName}TC${tc.name}"

      s"""
${leadingWs}implicit def ${fNameBase}${F}[TC[_], ${tp}${tpeParams}](implicit x: ${tc.name}[TC], ${paramSig(List("TC") ++ isId.fold(List(), List("F")), "a")}): TC[${tc.tpeName}[$F, ${tpeParams}]] =
${leadingWs}  ${isId.fold(s"${fNameBase}F[TC, Id, ${tpeParams}]", (tpes.length <= 1).fold(
                  s"x.${tc.identityF}(${params("a")})${tc.secondParams}",
                  s"Combine.${tc.combineF}${tpes.length}(${params("a")})${tc.secondParams}"))}
"""
    }
  }

  implicit class TpesWithIndexOps(tpes: List[(String, Int)]) {
    def prod: String = tpes.map(_._1).prod
  }

  implicit class ZipperOps(z: Zipper[String]) {
    def toList: List[String] = z.toStream.toList

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
