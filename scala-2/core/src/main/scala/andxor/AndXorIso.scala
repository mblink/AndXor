package andxor

import cats.{Eval, Id}
import monocle.Iso
import scala.reflect.macros

sealed trait AndXorIso { self =>
  type Axo <: AndXor
  val andxor: Axo

  final type Cop[f[_]] = andxor.Cop[f]
  final type Prod[f[_]] = andxor.Prod[f]

  type LabelledAxo <: AndXor
  val andxorLabelled: LabelledAxo

  final type LabelledCop[f[_]] = andxorLabelled.Cop[f]
  final type LabelledProd[f[_]] = andxorLabelled.Prod[f]
}

object AndXorIso {
  trait DerivingCovariant[TC[_]] {
    @inline final def derived[X]: TC[X] = macro AndXorIso.BlackboxMacros.derivingCovariant[TC, X]
  }
  trait DerivingContravariant[TC[_]] {
    @inline final def derived[X]: TC[X] = macro AndXorIso.BlackboxMacros.derivingContravariant[TC, X]
  }
  trait DerivingLabelledCovariant[TC[_]] {
    @inline final def derived[X]: TC[X] = macro AndXorIso.BlackboxMacros.derivingLabelledCovariant[TC, X]
  }
  trait DerivingLabelledContravariant[TC[_]] {
    @inline final def derived[X]: TC[X] = macro AndXorIso.BlackboxMacros.derivingLabelledContravariant[TC, X]
  }

  sealed class GenMacros(c: macros.blackbox.Context) {
    protected final def fail(message: String): Nothing = c.abort(c.enclosingPosition, message)
  }

  class WhiteboxMacros(val c: macros.whitebox.Context) extends GenMacros(c) {
    import c.universe._

    private def constTypeName(name: Name): ConstantType =
      internal.constantType(Constant(name.decodedName.toString))

    private case class Member(name: TermName, tpe: Type) {
      final lazy val typeTree: Tree = tq"$tpe"
    }

    private object Member {
      final def fromSymbol(tpe: Type)(sym: Symbol): Member = {
        val name = sym.name
        val decl = tpe.decl(name)

        if (!decl.isMethod)
          fail(s"No method $name in $tpe (this is probably because a constructor parameter isn't a val)")

        Member(name.toTermName, decl.asMethod.returnType.asSeenFrom(tpe, tpe.typeSymbol))
      }
    }

    private sealed trait ProductRepr {
      val paramLists: List[List[Member]]
      def instantiate(params: List[List[Tree]]): Tree
    }
    private case class ProductReprFromCtor(tpe: Type, paramLists: List[List[Member]]) extends ProductRepr {
      def instantiate(params: List[List[Tree]]): Tree = q"new $tpe(...$params)"
    }
    private case class ProductReprFromApply(tpe: Type, paramLists: List[List[Member]]) extends ProductRepr {
      def instantiate(params: List[List[Tree]]): Tree = q"${tpe.typeSymbol.companion}.apply(...$params)"
    }

    private def productReprFromCtor(tpe: Type): Option[ProductReprFromCtor] =
      if (tpe.typeSymbol.isAbstract) None else tpe.decls.collectFirst {
        case m: MethodSymbol if m.isPrimaryConstructor && m.isPublic && !m.isAbstract =>
          ProductReprFromCtor(tpe, m.paramLists.map(_.map(Member.fromSymbol(tpe))))
      }

    private def productReprFromCompanionApply(tpe: Type): Option[ProductReprFromApply] =
      tpe.typeSymbol.companion.typeSignature.member(TermName("apply")) match {
        case NoSymbol => None
        case sym =>
          sym.alternatives.collect { case m: MethodSymbol => m.paramLists }.sortBy(-_.map(_.size).sum)
            .headOption.map(ps => ProductReprFromApply(tpe, ps.map(_.map(Member.fromSymbol(tpe)))))
      }

    private def productRepr(tpe: Type): Option[ProductRepr] =
      productReprFromCtor(tpe).orElse(productReprFromCompanionApply(tpe))

    private val catsIdType = tq"_root_.cats.Id"

    private def genInst[X: c.WeakTypeTag, A](instType: Tree, members: List[A])(
      name: A => Name,
      typeTree: A => Tree,
      iso: (List[A], Int) => ValDef,
    ): Tree = {
      val tpe = weakTypeOf[X]
      val len = members.length

      def axoType(wrap: A => Tree): AppliedTypeTree =
        AppliedTypeTree(tq"_root_.andxor.${TypeName(s"AndXor$len")}", members.map(wrap))

      q"""
        new $instType[$tpe] {
          type Axo = ${axoType(typeTree)}
          val andxor: Axo = new ${axoType(typeTree)} {}

          type LabelledAxo = ${axoType(a => tq"_root_.andxor.Labelled[${typeTree(a)}, ${constTypeName(name(a))}]")}
          val andxorLabelled: LabelledAxo = andxor.asInstanceOf[LabelledAxo]

          ${iso(members, len)}
        }
      """
    }

    def prodInst[X: c.WeakTypeTag]: c.Expr[AndXorProdIso[X]] = {
      val tpe = weakTypeOf[X]

      productRepr(tpe).fold(fail(s"Could not identify primary constructor for $tpe")) { repr =>
        c.Expr[AndXorProdIso[X]](genInst(tq"_root_.andxor.AndXorProdIso", repr.paramLists.flatten)(
          _.name,
          _.typeTree,
          (members, len) => {
            val valueToProdName = if (len == 0) termNames.WILDCARD else TermName("v")
            val valueToProdParam = q"val $valueToProdName: $NoType"
            val valueToProd: Tree =
              if (len == 0)
                q"()"
              else if (len == 1)
                q"""_root_.andxor.types.${TermName(s"Prod$len")}.apply[..${catsIdType :: members.map(_.typeTree)}](
                  ..${members.map(m => q"$valueToProdName.${m.name}")})"""
              else
                q"""_root_.andxor.types.${TermName(s"Prod$len")}.apply[..${catsIdType :: members.map(_.typeTree)}](
                  _root_.scala.${TermName(s"Tuple$len")}.apply(..${members.map(m => q"$valueToProdName.${m.name}")}))"""

            val prodToValueName = if (len == 0) termNames.WILDCARD else TermName("p")
            val prodtoValueParam = q"val $prodToValueName: $NoType"
            val prodToValue: Tree =
              if (len == 0)
                repr.instantiate(Nil)
              else
                repr.instantiate(repr.paramLists.foldRight((len, List[List[Tree]]())) { case (ms, (i, acc)) =>
                  val (updI, trees) = ms.foldRight((i, List[Tree]())) { case (_, (j, acc)) =>
                    (j - 1, q"$prodToValueName.${TermName(s"t$j")}" :: acc)
                  }
                  (updI, trees :: acc)
                }._2)

            q"""val iso: _root_.monocle.Iso[$tpe, Prod[$catsIdType]] =
              _root_.monocle.Iso.apply[$tpe, Prod[$catsIdType]](
                $valueToProdParam => $valueToProd)(
                $prodtoValueParam => $prodToValue)"""
          }))
      }
    }

    private def allSubtypes(klass: ClassSymbol): List[Symbol] = {
      def go(syms: List[Symbol]): Eval[List[Symbol]] = syms match {
        case sym :: rest if sym.isClass && sym.asClass.isSealed => go(rest ++ sym.asClass.knownDirectSubclasses)
        case sym :: rest => go(rest).map(sym :: _)
        case Nil => Eval.now(Nil)
      }

      go(klass.knownDirectSubclasses.toList).value
    }

    def copInst[X: c.WeakTypeTag]: c.Expr[AndXorCopIso[X]] = {
      val tpe = weakTypeOf[X]

      Some(tpe.typeSymbol).collect { case s if s.isClass && s.asClass.isSealed => allSubtypes(s.asClass) } match {
        case None => fail(s"Could not prove that $tpe is a sealed trait/class")
        case Some(Nil) => fail(s"Unable to derive `AndXorCopIso[$tpe]` for zero-member coproduct")
        case Some(subclasses) =>
          c.Expr[AndXorCopIso[X]](genInst(tq"_root_.andxor.AndXorCopIso", subclasses)(
            _.name,
            s => tq"$s",
            (members, len) => {
              def mkEither(value: Tree, idx: Int): Tree =
                if (len == 1)
                  value
                else {
                  val inner = if (idx == len - 1) value else q"_root_.scala.Left.apply($value)"
                  1.to(idx).foldRight(inner)((_, acc) => q"_root_.scala.Right.apply($acc)")
                }

              def valueToCop(value: Tree): Tree = {
                val cases = members.zipWithIndex.map { case (s, i) => cq"x: $s => ${mkEither(q"x", i)}" }
                val matchStmt = q"$value match { case ..$cases }"
                q"_root_.andxor.types.${TermName(s"Cop$len")}.apply[..${catsIdType :: members.map(s => tq"$s")}]($matchStmt)"
              }

              def copToValue(cop: Tree): Tree = {
                val id = q"_root_.scala.Predef.identity[$tpe] _"
                val inner = (v: Tree) => if (len == 1) v else q"$v.fold($id, $id)"
                1.to(len - 2).foldRight(inner)((_, acc) =>
                  (v: Tree) => q"$v.fold($id, v => ${acc(q"v")})"
                ).apply(q"$cop.run")
              }

              q"""val iso: _root_.monocle.Iso[$tpe, Cop[$catsIdType]] =
                _root_.monocle.Iso.apply[$tpe, Cop[$catsIdType]](v => ${valueToCop(q"v")})(c => ${copToValue(q"c")})"""
            }))
      }
    }
  }

  class BlackboxMacros(val c: macros.blackbox.Context) extends GenMacros(c) {
    import c.universe._

    private def deriving[CopMapTC[_[_]], ProdMapTC[_[_]], DerivingTC[_], X](
      getAxo: Tree => Tree,
      getIso: Tree => Tree,
      copDerivingMethod: String,
      prodDerivingMethod: String,
      mapMethod: String,
      isoMethod: String,
    )(
      implicit copMapTC: c.WeakTypeTag[CopMapTC[Any]],
      prodMapTC: c.WeakTypeTag[ProdMapTC[Any]],
      derivingTC: c.WeakTypeTag[DerivingTC[Any]],
      X: c.WeakTypeTag[X]
    ): c.Expr[DerivingTC[X]] = {
      val xTpe = weakTypeOf[X]
      val isCop = xTpe.typeSymbol.isClass && xTpe.typeSymbol.asClass.isSealed
      val mapTCTpe = (if (isCop) weakTypeOf[CopMapTC[Any]] else weakTypeOf[ProdMapTC[Any]]).typeConstructor
      val derivingTCTpe = weakTypeOf[DerivingTC[Any]].typeConstructor
      val axoIsoName = TermName(c.freshName())
      c.Expr[DerivingTC[X]](q"""{
        val $axoIsoName = _root_.andxor.${TermName(s"AndXor${if (isCop) "Cop" else "Prod"}Iso")}.apply[$xTpe]

        ${mapTCTpe.typeSymbol.companion}.apply[$derivingTCTpe].${TermName(mapMethod)}(
          ${getAxo(Ident(axoIsoName))}.derivingId[$derivingTCTpe].${TermName(if (isCop) copDerivingMethod else prodDerivingMethod)}
        )(${getIso(Ident(axoIsoName))}.${TermName(isoMethod)})
      }""")
    }

    def derivingCovariant[TC[_], X](implicit derivingTC: c.WeakTypeTag[TC[Any]], X: c.WeakTypeTag[X]): c.Expr[TC[X]] =
      deriving[Alt, cats.Apply, TC, X](t => q"$t.andxor", t => q"$t.iso", "alt", "apply", "map", "reverseGet")

    def derivingContravariant[TC[_], X](implicit derivingTC: c.WeakTypeTag[TC[Any]], X: c.WeakTypeTag[X]): c.Expr[TC[X]] =
      deriving[Decidable, Divide, TC, X](t => q"$t.andxor", t => q"$t.iso", "choose", "divide", "contramap", "get")

    def derivingLabelledCovariant[TC[_], X](implicit derivingTC: c.WeakTypeTag[TC[Any]], X: c.WeakTypeTag[X]): c.Expr[TC[X]] =
      deriving[Alt, cats.Apply, TC, X](t => q"$t.andxorLabelled", t => q"$t.isoLabelled", "alt", "apply", "map", "reverseGet")

    def derivingLabelledContravariant[TC[_], X](implicit derivingTC: c.WeakTypeTag[TC[Any]], X: c.WeakTypeTag[X]): c.Expr[TC[X]] =
      deriving[Decidable, Divide, TC, X](t => q"$t.andxorLabelled", t => q"$t.isoLabelled", "choose", "divide", "contramap", "get")

    private object TCType {
      def unapply(companion: Tree): Option[(Tree, TypeName)] = companion match {
        case Ident(n @ TermName(_)) => Some((Ident(n.toTypeName), n.toTypeName))
        case i @ Ident(n @ TypeName(_)) => Some((i, n))
        case Select(base, n @ TermName(_)) => Some((Select(base, n.toTypeName), n.toTypeName))
        case s @ Select(_, n @ TypeName(_)) => Some((s, n))
        case _ => None
      }
    }

    private def isImplicitVal(v: ValDef): Boolean = v.mods.hasFlag(Flag.IMPLICIT)
    private def isImplicitVal(vs: List[ValDef]): Boolean = Some(vs).filter(_.nonEmpty).fold(false)(_.forall(isImplicitVal(_)))

    private def ctorParams(klass: ClassDef): (List[List[ValDef]], Option[List[ValDef]]) =
      klass.impl.body.collect { case d@DefDef(_, termNames.CONSTRUCTOR, _, _, _, _) => d } match {
        case DefDef(_, _, _, vs, _, _) :: Nil if vs.headOption.fold(true)(!isImplicitVal(_)) =>
          vs.span(!isImplicitVal(_)) match {
            case (e, i :: Nil) => (e, Some(i))
            case (e, Nil) => (e, None)
            case _ => fail(s"Found more than one implicit parameter group for ${klass.name}")
          }
        case ds =>
          fail(s"Failed to find exactly one constructor for class `${klass.name}`, found: `$ds`")
      }

    private def getTypeNames(tpe: Tree): Set[TypeName] = {
      def go(ts: List[Tree]): Eval[Set[TypeName]] = ts match {
        case Ident(t: TypeName) :: rest => go(rest).map(_ + t)
        case AppliedTypeTree(Ident(t: TypeName), tps) :: rest => go(tps ++ rest).map(_ + t)
        case _ :: rest => go(rest)
        case Nil => Eval.now(Set.empty[TypeName])
      }

      go(List(tpe)).value
    }

    @annotation.nowarn("msg=pattern var qq\\$macro.*is never used")
    def derivesAnnotation(annottees: Tree*): Tree = {
      val tcs = c.prefix.tree match {
        case Apply(Select(New(Ident(TypeName(_))), termNames.CONSTRUCTOR), args) => args
        case Apply(Select(New(Select(_, TypeName(_))), termNames.CONSTRUCTOR), args) => args
        case t => fail(s"Unexpected `derives` application: ${showRaw(t)}")
      }

      def insts(klass: ClassDef): List[Tree] = tcs.map {
        case tcCompanion @ TCType(tcTpe, tcTpeName) =>
          val tparams = klass.tparams.map(t => treeCopy.TypeDef(t, Modifiers(), t.name, t.tparams, t.rhs))
          val instName = c.freshName(tcTpeName).toTermName

          if (tparams.isEmpty) {
            val tpe = tq"${klass.name}"
            q"implicit val $instName: $tcTpe[$tpe] = $tcCompanion.derived[$tpe]"
          } else {
            val params = ctorParams(klass)._1.flatten
            val tparamNames = tparams.map(_.name)
            val tparamNamesSet = tparamNames.toSet
            val abstractParams = params.filter(p => getTypeNames(p.tpt).exists(tparamNamesSet.contains)).distinctBy(p => showRaw(p.tpt))

            val implicitParams = abstractParams.zipWithIndex.map { case (p, i) =>
              // TODO - why are these reported unused?
              q"@_root_.scala.annotation.unused val ${TermName(s"ev$i")}: $tcTpe[${p.tpt}]"
            }
            val tpe = tq"${klass.name}[..$tparamNames]"
            q"implicit def $instName[..$tparams](implicit ..$implicitParams): $tcTpe[$tpe] = $tcCompanion.derived[$tpe]"
          }

        case tc =>
          fail(s"Unexpected typeclass reference: `$tc`")
      }

      annottees match {
        case List(klass: ClassDef) =>
          q"""
          $klass
          object ${klass.name.toTermName} {
            ..${insts(klass)}
          }
          """
        case List(
          klass: ClassDef,
          q"object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }",
        ) =>
          q"""
          $klass
          object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
            ..$objDefs
            ..${insts(klass)}
          }
          """
        case _ => fail("Invalid `derives` annotation target: must be a class/trait")
      }
    }
  }
}

trait AndXorProdIso[X] extends AndXorIso {
  val iso: Iso[X, Prod[Id]]
  final lazy val isoLabelled: Iso[X, LabelledProd[Id]] = iso.asInstanceOf[Iso[X, LabelledProd[Id]]]

  final def derivingCovariant[TC[_]]: TC[X] = macro AndXorIso.BlackboxMacros.derivingCovariant[TC, X]
  final def derivingLabelledCovariant[TC[_]]: TC[X] = macro AndXorIso.BlackboxMacros.derivingLabelledCovariant[TC, X]
  final def derivingContravariant[TC[_]]: TC[X] = macro AndXorIso.BlackboxMacros.derivingContravariant[TC, X]
  final def derivingLabelledContravariant[TC[_]]: TC[X] = macro AndXorIso.BlackboxMacros.derivingLabelledContravariant[TC, X]
}

object AndXorProdIso {
  type Aux[X, A, LA] = AndXorProdIso[X] {
    type Axo = A
    type LabelledAxo = LA
  }

  @inline final def apply[X](implicit p: AndXorProdIso[X]): Aux[X, p.Axo, p.LabelledAxo] = p

  implicit def inst[X <: Product]: AndXorProdIso[X] = macro AndXorIso.WhiteboxMacros.prodInst[X]
}

trait AndXorCopIso[X] extends AndXorIso {
  val iso: Iso[X, Cop[Id]]
  final lazy val isoLabelled: Iso[X, LabelledCop[Id]] = iso.asInstanceOf[Iso[X, LabelledCop[Id]]]

  final def derivingCovariant[TC[_]]: TC[X] = macro AndXorIso.BlackboxMacros.derivingCovariant[TC, X]
  final def derivingLabelledCovariant[TC[_]]: TC[X] = macro AndXorIso.BlackboxMacros.derivingLabelledCovariant[TC, X]
  final def derivingContravariant[TC[_]]: TC[X] = macro AndXorIso.BlackboxMacros.derivingContravariant[TC, X]
  final def derivingLabelledContravariant[TC[_]]: TC[X] = macro AndXorIso.BlackboxMacros.derivingLabelledContravariant[TC, X]
}

object AndXorCopIso {
  type Aux[X, A, LA] = AndXorCopIso[X] {
    type Axo = A
    type LabelledAxo = LA
  }

  @inline final def apply[X](implicit c: AndXorCopIso[X]): Aux[X, c.Axo, c.LabelledAxo] = c

  implicit def inst[X]: AndXorCopIso[X] = macro AndXorIso.WhiteboxMacros.copInst[X]
}
