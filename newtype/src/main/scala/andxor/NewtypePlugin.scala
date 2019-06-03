package andxor

import scala.meta._
import scala.meta.contrib._
import scala.tools.nsc.Global

class NewtypePlugin(global: Global) extends AnnotationPlugin(global) { self =>
  private val newtype = "newtype"

  val name: String = newtype
  val triggers: List[String] = List(newtype)

  implicit class LocalScopeOps(scope: LocalScope) {
    def inObject: Boolean = scope.self match {
      case _: Defn.Object | _: Pkg.Object => true
      case _ => false
    }
  }

  override def update(
    anns: List[Mod.Annot],
    klass: Defn.Class,
    companionO: Option[Defn.Object]
  ): Reader[LocalScope, (Option[(Defn.Class, Option[Defn.Object])], Vector[Stat])] =
    genNewType(klass, companionO)(
      _.name,
      _.ctor.paramss match {
        case List(List(p)) => p.withMods(Mod.ValParam() :: p.extract[Mod])
        case _ => g.error("newtype class constructor must have exactly one parameter"); null
      },
      _.tparams).map { case (tpe, tr, companion) => (None, Vector(tpe, tr, companion)) }

  override def update(
    anns: List[Mod.Annot],
    tpeDef: Defn.Type,
    companionO: Option[Defn.Object]
  ): Reader[LocalScope, (Option[(Defn.Type, Option[Defn.Object])], Vector[Stat])] = {
    implicit val extractTpeStats: Extract[Defn.Type, Stat] = Extract(_ => Nil)

    genNewType(tpeDef, companionO)(
      _.name,
      t => Term.Param(List(Mod.ValParam()), Term.Name("value"), Some(t.body), None),
      _.tparams).map { case (upd, tr, companion) => (Some((upd, Some(companion))), Vector(tr)) }
  }

  private def maybeGenerateGetter(param: Term.Param): Option[Defn.Def] =
    if (param.mods.has[Mod.ValParam] && !param.mods.has[Mod.Private])
      Some(q"def ${Term.Name(param.name.value)}: ${param.decltpe.get} = $$this$$.asInstanceOf[${param.decltpe.get}]")
    else None

  private def genExtraMethods(extras: List[Stat]): List[Stat] =
    extras.flatMap(_ match {
      case d: Defn.Def => List(d)
      case v: Defn.Val =>
        error(v.pos, "val definitions not supported, use def instead")
        Nil
      case x =>
        error(x.pos, s"illegal definition in newtype: $x")
        Nil
    })

  private def generateOps(
    param: Term.Param,
    extras: List[Stat],
    tparams: List[Type.Param],
    tparamsNoMods: List[Type.Param],
    tparamNames: List[Type.Name]
  ): Reader[LocalScope, List[Stat]] = Reader { scope =>
    val extensionMethods = maybeGenerateGetter(param).toList ++ genExtraMethods(extras)
    val opsParent = Init(if (scope.inObject) t"_root_.scala.AnyVal" else t"_root_.scala.AnyRef", Name.Anonymous(), Nil)
    if (extensionMethods.isEmpty) Nil
    // else if scope.inObject
    else if (tparams.isEmpty) List(
      q"implicit final class Ops$$newtype(val $$this$$: Type) extends $opsParent { ..$extensionMethods }",
      q"implicit def opsThis(x: Ops$$newtype): Type = x.$$this$$"
    )
    else List(
      q"implicit final class Ops$$newtype[..$tparams](val $$this$$: Type[..$tparamNames]) extends $opsParent { ..$extensionMethods }",
      q"implicit def opsThis[..$tparamsNoMods](x: Ops$$newtype[..$tparamNames]): Type[..$tparamNames] = x.$$this$$"
    )
  }

  private def mkBaseTypeDef(tpeName: Type.Name): Defn.Type =
    q"type Base = _root_.scala.Any { type ${Type.Name(s"__${tpeName.value}__newtype")} } "

  private def mkTypeTypeDef(tparams: List[Type.Param], tparamNames: List[Type.Name]): Decl.Type =
    q"type Type[..$tparams] <: Base with ${if (tparams.isEmpty) t"Tag" else t"Tag[..$tparamNames]"}"

  private def genNewType[A <: Defn: Extract[?, Mod]: Extract[?, Stat]: Named](defn: A, companionO: Option[Defn.Object])(
    getName: A => Type.Name,
    getParam: A => Term.Param,
    getTparams: A => List[Type.Param],
  ): Reader[LocalScope, (Defn.Type, Defn.Trait, Defn.Object)] = Reader { scope =>
    val companion = companionO.getOrElse(genCompanion(defn))
    val tpeName: Type.Name = getName(defn)
    val param: Term.Param = getParam(defn)
    val reprTpe: Type = param.decltpe.get
    val tparams = getTparams(defn)
    val tparamNames: List[Type.Name] = tparams.map(p => Type.Name(p.name.value))
    val tparamsNoMods: List[Type.Param] = tparams.map(_.withMods(Nil))

    val applyMethod: Defn.Def =
      if (tparams.isEmpty) q"def apply(x: $reprTpe): $tpeName = x.asInstanceOf[$tpeName]"
      else q"def apply[..$tparamsNoMods](x: $reprTpe): $tpeName[..$tparamNames] = x.asInstanceOf[$tpeName[..$tparamNames]]"

    val unapplyMethod: Defn.Def =
      if (tparams.isEmpty) q"""def unapply(x: $tpeName): Some[$reprTpe] = Some(x.asInstanceOf[$reprTpe])"""
      else q"def unapply[..$tparamsNoMods](x: $tpeName[..$tparamNames]): Some[$reprTpe] = Some(x.asInstanceOf[$reprTpe])"

    val companionExtras =
      applyMethod ::
      unapplyMethod ::
      generateOps(param, defn.extract[Stat], tparams, tparamsNoMods, tparamNames).run(scope)
      // generateCoercibleInstances(tparamsNoVar, tparamNames, tparamsWild) :::
      // generateDerivingMethods(tparamsNoVar, tparamNames, tparamsWild)

    val typesTraitName = Type.Name(s"${tpeName.value}__Types")
    val objDef = companion.copy(templ = companion.templ.copy(
      inits = companion.templ.inits :+ Init(typesTraitName, Name.Anonymous(), Nil),
      stats = companion.templ.stats ++ companionExtras
    ))
    val objTpe = Type.Select(objDef.name, t"Type")

    // Note that we use an abstract type alias
    // `type Type <: Base with Tag` and not `type Type = ...` to prevent
    // scalac automatically expanding the type alias.
    // Also, Scala 2.10 doesn't support objects having abstract type members, so we have to
    // use some indirection by defining the abstract type in a trait then having
    // the companion object extend the trait.
    // See https://github.com/scala/bug/issues/10750
    val baseTpeDef = mkBaseTypeDef(tpeName)
    val tpeTpeDef = mkTypeTypeDef(tparams, tparamNames)

    (
      q"type $tpeName[..$tparams] = ${if (tparams.isEmpty) objTpe else t"$objTpe[..$tparamNames]"}",
      q"""
      trait $typesTraitName {
        ..${List(
          q"type Repr[..$tparams] = $reprTpe",
          baseTpeDef,
          q"trait Tag[..$tparams] extends _root_.scala.Any",
          tpeTpeDef
        )}
      }
      """,
      objDef
    )
  }
}
