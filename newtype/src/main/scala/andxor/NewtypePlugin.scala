package andxor

import scala.tools.nsc.Global

class NewtypePlugin(override val global: Global) extends AnnotationPlugin(global) { self =>
  import global._

  private val newtype = "newtype"

  val name: String = newtype
  val triggers: List[String] = List(newtype)

  implicit class LocalScopeOps(scope: LocalScope) {
    def inObject: Boolean = scope.self match {
      case _: g.ModuleDef => true
      case _ => false
    }
  }

  override def updateClass(
    anns: List[Tree],
    klass: ClassDef,
    companionO: Option[ModuleDef]
  ): Reader[LocalScope, (Option[(ClassDef, Option[ModuleDef])], Vector[Tree])] =
    genNewType(klass, companionO)(
      _.name,
      c => ctorParams(c) match {
        case (List(List(p)), _) => p
        case _ => g.error("newtype class constructor must have exactly one parameter"); null
      },
      _.tparams).map { case (tpe, tr, companion) => (None, Vector(tpe, tr, companion)) }

  override def updateType(
    anns: List[Tree],
    tpeDef: TypeDef,
    companionO: Option[ModuleDef]
  ): Reader[LocalScope, (Option[(TypeDef, Option[ModuleDef])], Vector[Tree])] = {
    implicit val getTpeStats: Get[TypeDef, List[Tree]] = Get(_ => Nil)

    genNewType(tpeDef, companionO)(
      _.name,
      t => ValDef(Modifiers(Flag.PARAM), TermName("value"), t.rhs, EmptyTree),
      _.tparams).map { case (upd, tr, companion) => (Some((upd, Some(companion))), Vector(tr)) }
  }

  private def maybeGenerateGetter(param: ValDef): Option[DefDef] =
    if (param.mods.hasFlag(Flag.PARAM) && !param.mods.isPrivate)
      Some(q"def ${param.name}: ${param.tpt.duplicate} = $$this$$.asInstanceOf[${param.tpt.duplicate}]")
    else None

  private def genExtraMethods(extras: List[Tree]): List[Tree] =
    extras.flatMap(_ match {
      case d: DefDef => List(d)
      case v: ValDef =>
        error(v.pos, "val definitions not supported, use def instead")
        Nil
      case x =>
        error(x.pos, s"illegal definition in newtype: $x")
        Nil
    })

  private def generateOps(
    param: ValDef,
    extras: List[Tree],
    tparams: () => List[TypeDef],
    tparamsNoMods: List[TypeDef] => List[TypeDef],
    tparamNames: () => List[Tree]
  ): Reader[LocalScope, List[Tree]] = Reader { scope =>
    val extensionMethods = maybeGenerateGetter(param).toList ++ genExtraMethods(extras)
    val opsParent = List(if (scope.inObject) tq"_root_.scala.AnyVal" else tq"_root_.scala.AnyRef")
    val ts = tparams()
    if (extensionMethods.isEmpty) Nil
    // else if scope.inObject
    else if (ts.isEmpty) List(
      q"implicit final class Ops$$newtype(val $$this$$: Type) extends ..$opsParent { ..$extensionMethods }",
      q"implicit def opsThis(x: Ops$$newtype): Type = x.$$this$$"
    )
    else List(
      q"implicit final class Ops$$newtype[..$ts](val $$this$$: Type[..${tparamNames()}]) extends ..$opsParent { ..$extensionMethods }",
      q"implicit def opsThis[..${tparamsNoMods(ts)}](x: Ops$$newtype[..${tparamNames()}]): Type[..${tparamNames()}] = x.$$this$$"
    )
  }

  private def mkBaseTypeDef(tpeName: TypeName): TypeDef =
    q"type Base = _root_.scala.Any { type ${TypeName(s"__${tpeName.decode}__newtype")} } "

  private def mkTypeTypeDef(tparams: () => List[TypeDef], tparamNames: () => List[Tree]): TypeDef = tparams() match {
    case Nil => q"type Type <: Base with Tag"
    case ts => q"type Type[..$ts] <: Base with Tag[..${tparamNames()}]"
  }

  private def genNewType[A <: NameTree: Get[?, Modifiers]: Get[?, List[Tree]]](defn: A, companionO: Option[ModuleDef])(
    getName: A => TypeName,
    getParam: A => ValDef,
    getTparams: A => List[TypeDef]
  ): Reader[LocalScope, (TypeDef, ClassDef, ModuleDef)] = Reader { scope =>
    val companion = companionO.getOrElse(genCompanion(defn))
    val tpeName: TypeName = getName(defn)
    val param: ValDef = getParam(defn)
    val tparams = () => getTparams(defn).map(_.duplicate)
    val tparamNames = () => tparams().map(t => Ident(t.name).duplicate)
    val tparamsNoMods = (_: List[TypeDef]).map(t => treeCopy.TypeDef(t, Modifiers(), t.name, t.tparams, t.rhs))
    val ts = tparams()

    val applyMethod: DefDef =
      if (ts.isEmpty) q"def apply(x: ${param.tpt.duplicate}): $tpeName = x.asInstanceOf[$tpeName]"
      else q"def apply[..${tparamsNoMods(ts)}](x: ${param.tpt.duplicate}): $tpeName[..${tparamNames()}] = x.asInstanceOf[$tpeName[..${tparamNames()}]]"

    val unapplyMethod: DefDef =
      if (ts.isEmpty) q"""def unapply(x: $tpeName): Some[${param.tpt.duplicate}] = Some(x.asInstanceOf[${param.tpt.duplicate}])"""
      else q"def unapply[..${tparamsNoMods(ts)}](x: $tpeName[..${tparamNames()}]): Some[${param.tpt.duplicate}] = Some(x.asInstanceOf[${param.tpt.duplicate}])"

    val companionExtras =
      applyMethod ::
      unapplyMethod ::
      generateOps(param, defn.get[List[Tree]], tparams, tparamsNoMods, tparamNames).run(scope)
      // generateCoercibleInstances(tparamsNoVar, tparamNames, tparamsWild) :::
      // generateDerivingMethods(tparamsNoVar, tparamNames, tparamsWild)

    val typesTraitName = TypeName(s"${tpeName.decode}__Types")
    val objDef = treeCopy.ModuleDef(companion, companion.mods, companion.name,
      treeCopy.Template(companion.impl, companion.impl.parents :+ Ident(typesTraitName), companion.impl.self, companion.impl.body ++ companionExtras))
    val objTpe = Select(Ident(objDef.name), TypeName("Type"))

    // Note that we use an abstract type alias
    // `type Type <: Base with Tag` and not `type Type = ...` to prevent
    // scalac automatically expanding the type alias.
    // Also, Scala 2.10 doesn't support objects having abstract type members, so we have to
    // use some indirection by defining the abstract type in a trait then having
    // the companion object extend the trait.
    // See https://github.com/scala/bug/issues/10750
    val baseTpeDef = mkBaseTypeDef(tpeName)
    val tpeTpeDef = mkTypeTypeDef(tparams, tparamNames)

    val (a, b, c) = (
      q"type $tpeName[..${ts}] = ${if (ts.isEmpty) objTpe else tq"$objTpe[..${tparamNames()}]"}",
      q"""
      trait $typesTraitName {
        ..${List(
          q"type Repr[..${ts}] = ${param.tpt.duplicate}",
          baseTpeDef,
          q"trait Tag[..${ts}] extends _root_.scala.Any",
          tpeTpeDef
        )}
      }
      """,
      objDef
    )
    // debug("TYPE", a)
    // debug("TRAIT", b)
    // debug("OB", c)
    (a, b, c)
  }
}
