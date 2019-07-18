package andxor

import andxor.compat.ParseNamedArg
import scala.collection.mutable.{Map => MMap}
import scala.tools.nsc.Global

class DerivingPlugin(override val global: Global) extends AnnotationPlugin(global) with ParseNamedArg { self =>
  import global._

  private val deriving = "deriving"

  private val varianceFlags: Long = (Flag.COVARIANT | Flag.CONTRAVARIANT).asInstanceOf[Long]

  val triggers: List[String] = List(deriving)

  case class ConfiguredTc(prod: (Variance[ProdParam], Boolean), cop: (Variance[CopParam], Boolean))
  val configuredTcs: MMap[String, ConfiguredTc] = MMap()

  override def processOptions(opts: List[String], error: String => Unit): Unit = {
    def configure(tcs: String, copOrProd: Either[Unit, Unit], tpe: String): Unit = {
      def upd(labelled: Boolean, vs: (Variance[CopParam], Variance[ProdParam])): Unit = {
        val (p, c): ((Variance[ProdParam], Boolean), (Variance[CopParam], Boolean)) =
          copOrProd match {
            case Left(_) => (null, (vs._1, labelled))
            case Right(_) => ((vs._2, labelled), null)
          }
        tcs.split('|').foreach(tc => configuredTcs.update(tc, configuredTcs.get(tc) match {
          case Some(conf) => conf.copy(prod = Option(p).getOrElse(conf.prod), cop = Option(c).getOrElse(conf.cop))
          case None => ConfiguredTc(p, c)
        }))
      }

      tpe.toLowerCase match {
        case "covariant" => upd(false, (CovariantCoproduct, CovariantProduct))
        case "labelledcovariant" => upd(true, (CovariantCoproduct, CovariantProduct))
        case "contravariant" => upd(false, (ContravariantCoproduct, ContravariantProduct))
        case "labelledcontravariant" => upd(true, (ContravariantCoproduct, ContravariantProduct))
      }
    }

    opts.foreach(opt => opt.split(":").toList match {
      case tpe :: x :: tcs :: Nil if x.toLowerCase == "cop" => configure(tcs, Left(()), tpe)
      case tpe :: x :: tcs :: Nil if x.toLowerCase == "prod" => configure(tcs, Right(()), tpe)
      case tpe :: tc :: Nil =>
        configure(tc, Left(()), tpe)
        configure(tc, Right(()), tpe)
      case _ => error(s"deriving: invalid option `$opt`")
    })
  }

  override def updateClass(
    anns: List[Tree],
    c: ClassDef,
    companionO: Option[ModuleDef]
  ): Reader[LocalScope, (Option[(ClassDef, Option[ModuleDef])], Vector[Tree])] =
    for {
      companion <- Reader((_: LocalScope) => companionO.getOrElse(genCompanion(c)))
      updatedCompanion <-
        if (c.mods.isSealed) CopTree.klass(c, companion).map(f => regenObject(companion, mkStats(f, anns)))
        else if (c.mods.isCase) Reader((_: LocalScope) => regenObject(companion, mkStats(ProdTree(c, _), anns)))
        else Reader((_: LocalScope) => companion)
    } yield (Some((c, Some(updatedCompanion))), Vector())

  override def updateTrait(
    anns: List[Tree],
    t: ClassDef,
    companionO: Option[ModuleDef]
  ): Reader[LocalScope, (Option[(ClassDef, Option[ModuleDef])], Vector[Tree])] =
    for {
      companion <- Reader((_: LocalScope) => companionO.getOrElse(genCompanion(t)))
      updatedCompanion <-
        if (t.mods.isSealed) CopTree.trait0(t, companion).map(f => regenObject(companion, mkStats(f, anns)))
        else Reader((_: LocalScope) => companion)
    } yield (Some((t, Some(updatedCompanion))), Vector())

  // Andxor types
  private val andxorPkg = q"_root_.andxor"
  private val andxorTpesPkg = q"_root_.andxor.types"
  private val labelledObj = q"$andxorPkg.Labelled"

  // Scala types
  private val scalaPkg = q"_root_.scala"

  // Scalaz types
  private val scalazPkg = q"_root_.scalaz"
  private val id = tq"$scalazPkg.Id.Id"
  private val isoSetObj = q"$scalazPkg.Isomorphism.IsoSet"
  private val isoSetTpe = tq"$scalazPkg.Isomorphism.IsoSet"
  private def mkAdtVal(inst: Tree): Tree = q"$andxorPkg.types.ADTValue($inst)"
  private def adtValTpe(tpe: Tree): Tree = tq"$andxorPkg.types.ADTValue[$tpe]"

  sealed trait Param {
    val name: Name
    val tpe: Tree
    final lazy val label: Label = Label(name)
    final lazy val labelledTpe: Tree = tq"_root_.andxor.Labelled.Aux[$tpe, ${label.singletonTpe}]"
    final lazy val termName: TermName = TermName(name.decode)
  }

  case class ProdParam(name: Name, tpeF: () => Tree) extends Param {
    final val tpe: Tree = tpeF().duplicate
  }
  object ProdParam {
    def apply(p: ValDef): ProdParam = ProdParam(p.name, () => p.tpt)
  }

  case class CopParam(member: Either[ClassDef, ModuleDef]) extends Param {
    final val name: Name = member.fold(_.name, _.name)
    final val memberTpe: Tree = member.fold(ProdTree(_, false).tpe, o => SingletonTypeTree(Ident(o.name)))
    final val tpe: Tree = member.fold(_ => memberTpe, _ => adtValTpe(memberTpe))
  }

  sealed abstract class Variance[+P <: Param] {
    val typeclass: Tree
    val derivationFunction: TermName
    val mapFunction: TermName
    val isoFunction: TermName
  }
  sealed abstract class Covariant[+P <: Param] extends Variance[P] {
    val mapFunction = TermName("map")
    val isoFunction = TermName("from")
  }
  case object CovariantProduct extends Covariant[ProdParam] {
    val typeclass = tq"_root_.scalaz.Apply"
    val derivationFunction = TermName("apply")
  }
  case object CovariantCoproduct extends Covariant[CopParam] {
    val typeclass = tq"_root_.andxor.Alt"
    val derivationFunction = TermName("alt")
  }
  sealed trait Contravariant[+P <: Param] extends Variance[P] {
    val mapFunction = TermName("contramap")
    val isoFunction = TermName("to")
  }
  case object ContravariantProduct extends Contravariant[ProdParam] {
    val typeclass = tq"_root_.andxor.Divide"
    val derivationFunction = TermName("divide")
  }
  case object ContravariantCoproduct extends Contravariant[CopParam] {
    val typeclass = tq"_root_.andxor.Decidable"
    val derivationFunction = TermName("choose")
  }

  def identOrSelect(term: Tree): Option[Either[Ident, Select]] =
    term match {
      case i@Ident(name) => Some(Left(i))
      case s@Select(qual, name) => Some(Right(s))
      case _ => None
    }

  def termToType(term: Tree): Tree =
    identOrSelect(term).map(_ match {
      case Left(Ident(n)) => Ident(n.toTypeName)
      case Right(Select(q, n)) => Select(q, n.toTypeName)
    }).getOrElse {
      error(term.pos, s"Unable to convert term `${showCode(term)}` to type")
      q""
    }

  def valOrDef(mods: Modifiers, name: TermName, tparams: List[TypeDef], params: List[List[ValDef]], tpe: Tree, body: Tree): Tree =
    (tparams, params) match {
      case (Nil, Nil) => q"$mods val $name: $tpe = $body"
      case (ts, ps)   => q"$mods def $name[..$ts](...$ps): $tpe = $body"
    }

  val name = "deriving"

  private[andxor] val andxorName = TermName("repr")
  private[andxor] val isoName = TermName("iso")
  private[andxor] val andxorLabelledName = TermName("labelledRepr")
  private[andxor] val labelledIsoName = TermName("labelledIso")

  case class Label(paramName: Name) {
    // Don't use `freshName` because label name needs to be deterministic
    final val valName = TermName(s"andxor_label_${paramName.decode}")
    final val singletonTpe: SingletonTypeTree = SingletonTypeTree(Ident(valName))
    private val implValName = TermName(s"${valName}_impl")
    final val defns: List[Tree] = List(
      q"val $valName: String = ${Literal(Constant(paramName.decode))}",
      q"implicit val $implValName: $singletonTpe = $valName"
    )
  }

  private def extendsTpe(tpeName: TypeName, parents: List[Tree]): Boolean =
    parents.collectFirst { case Ident(TypeName(n)) if n == tpeName.decode => () }.nonEmpty

  private def concreteChildOfTpe(tpeName: TypeName, tree: Tree): Option[Either[ClassDef, ModuleDef]] =
    tree match {
      case o @ ModuleDef(_, _, Template(parents, _, _)) if extendsTpe(tpeName, parents) => Some(Right(o))
      case c @ ClassDef(_, _, _, Template(parents, _, _)) if extendsTpe(tpeName, parents) => Some(Left(c))
      case _ => None
    }

  private def traitsExtendingTpe(tpeName: TypeName, allTraits: List[ClassDef]): List[ClassDef] =
    allTraits.flatMap(_ match {
      case t @ ClassDef(_, name, _, Template(parents, _, _)) if extendsTpe(tpeName, parents) =>
        t :: traitsExtendingTpe(name, allTraits)
      case _ => Nil
    })

  private def getChildrenOfTpe(tpeName: TypeName, companion: ModuleDef): Reader[LocalScope, List[Either[ClassDef, ModuleDef]]] =
    Reader { scope =>
      val tpeNames = tpeName :: traitsExtendingTpe(tpeName, scope.traits.values.toList).map(_.name)
      (companion.get[List[Tree]] ++ scope.objects.values.toList ++ scope.classes.values.toList)
        .flatMap(t => tpeNames.flatMap(concreteChildOfTpe(_, t)))
    }

  private def getTypeNames(tpe: Tree): Set[TypeName] =
    (tpe match {
      case Ident(t: TypeName) => Set(t)
      case _ => Set()
    }) ++ tpe.children.flatMap(getTypeNames(_))

  sealed abstract class GenTree[+P <: Param](
    val labelled: Boolean,
    val params: List[P],
    val name: TypeName,
    protected val tparams0: List[TypeDef],
    val copOrProd: String
  ) {
    final val tparams: List[TypeDef] = tparams0.map(_.duplicate)
    final val tparamsNoVariance: List[TypeDef] =
      tparams.map(t => treeCopy.TypeDef(t, t.mods & ~varianceFlags, t.name, t.tparams, t.rhs))
    final val tparamNames: List[Ident] = tparams0.map(t => Ident(t.name))

    final val arity: Int = params.length

    final val tpe: Tree = tq"$name[..$tparamNames]"
    final val tpes: List[Tree] =
      if (labelled) params.map(_.labelledTpe)
      else params.map(_.tpe)

    final val andxorTpes: List[Tree] = id :: tpes

    final val andxorName: TermName = if (labelled) andxorLabelledName else self.andxorName
    final val isoName: TermName = if (labelled) labelledIsoName else self.isoName

    final val andxorTpe: Tree = tq"$andxorPkg.${TypeName(s"AndXor$arity")}[..${andxorTpes.tail}]"

    final val reprName = s"${copOrProd}${arity}"
    final val reprObj: Tree = q"$andxorTpesPkg.${TermName(reprName)}"
    final val reprTpe: Tree = if (tpes.length <= 1) tpes.head else tq"$andxorTpesPkg.${TypeName(reprName)}[..$andxorTpes]"

    def iso: Tree

    final val isoTpe: Tree = tq"$isoSetTpe[$tpe, $reprTpe]"

    final val abstractParams: List[Param] = {
      val abstractTpeNames = tparams.map(_.name).toSet
      params.filter(param => getTypeNames(param.tpe).exists(abstractTpeNames.contains(_)))
    }

    def mkValue(inst: Tree, param: Param): Tree

    def normalizeValue(v: Tree): Tree = if (labelled) q"$v.value" else v
  }

  case class ProdTree(klass: ClassDef, override val labelled: Boolean) extends GenTree[ProdParam](
    labelled,
    ctorParams(klass)._1.flatMap(_.map(ProdParam(_))),
    klass.name,
    klass.tparams,
    "Prod"
  ) {
    final val paramss = ctorParams(klass)._1

    private def tupleAccess(idx: Int): TermName = TermName(s"t$idx")

    private val mkTuple: Tree =
      params match {
        case Nil      => error(klass.pos, "TODO - support 0 parameter case classes"); klass
        case p :: Nil => mkValue(q"x", p)
        case ps       => q"$scalaPkg.${TermName(s"Tuple${ps.length}")}.apply(..${ps.map(mkValue(q"x", _))})"
      }

    private val constructorArgs: List[List[Tree]] =
      if (tpes.length <= 1) List(List(normalizeValue(q"x")))
      else paramss.zipWithIndex.map { case (group, i) =>
        group.zipWithIndex.map { case (_, j) => normalizeValue(q"x.${tupleAccess(i + j + 1)}") } }

    final val iso: Tree =
      q"""
      $isoSetObj[$tpe, $reprTpe](
        (x: $tpe) => ${if (tpes.length <= 1) mkTuple else q"$reprObj[..$andxorTpes]($mkTuple)"},
        (x: $reprTpe) => ${if (isNewType) q"${klass.name.companionName}(...$constructorArgs)" else q"new $tpe(...$constructorArgs)"})
      """

    def mkValue(inst: Tree, param: Param): Tree =
      if (labelled) q"$labelledObj[${param.tpe}, ${param.label.singletonTpe}]($inst.${param.termName}, ${param.label.valName})"
      else          q"$inst.${param.termName}"

    private def isNewType: Boolean = klass.mods.annotations.exists(isAnnotationNamed(_, TypeName("newtype")))
  }

  case class CopTree(
    children: List[Either[ClassDef, ModuleDef]],
    override val name: TypeName,
    override val tparams0:  List[TypeDef],
    override val labelled: Boolean
  ) extends GenTree[CopParam](labelled, children.map(CopParam(_)), name, tparams0, "Cop") {
    def mkValue(inst: Tree, param: Param): Tree = {
      val v = param.asInstanceOf[CopParam].member.fold(_ => inst, _ => mkAdtVal(inst))
      if (labelled) q"$labelledObj[${param.tpe}, ${param.label.singletonTpe}]($v, ${param.label.valName})"
      else v
    }

    def maybeUnwrap(inst: Tree, paramIdx: Option[Int]): Tree =
      paramIdx.flatMap(children.lift(_) match {
        case Some(Right(x)) => Some(x)
        case _ => None
      }).fold(inst)(_ => q"$inst.value")

    def injInst(param: Param): Tree =
      if (params.length <= 1) mkValue(q"inst", param) else q"$andxorName.inj(${mkValue(q"inst", param)})"

    final val iso: Tree = q"""
      $isoSetObj[$tpe, $reprTpe](
        (x: $tpe) => x match {
          case ..${params.map(p => cq"inst: ${p.memberTpe} => ${injInst(p)}")}
        },
        (repr: $reprTpe) => {
          val x = ${if (params.length <= 1) q"repr" else q"repr.run"}
          ${params.zipWithIndex.tail.foldRight[Tree](maybeUnwrap(normalizeValue(q"x"), Some(params.length - 1)))(
            (t, acc) => q"""x.fold[$tpe](
              (x: ${tpes(t._2 - 1)}) => ${maybeUnwrap(normalizeValue(q"x"), Some(t._2 - 1))},
              x => $acc)""")}
        })
    """
  }

  object CopTree {
    def apply(name: TypeName, companion: ModuleDef, tparams: List[TypeDef]): Reader[LocalScope, Boolean => CopTree] =
      getChildrenOfTpe(name, companion).map(children => labelled => new CopTree(children, name, tparams, labelled))

    def klass(c: ClassDef, companion: ModuleDef): Reader[LocalScope, Boolean => CopTree] =
      apply(c.name, companion, c.tparams)

    def trait0(t: ClassDef, companion: ModuleDef): Reader[LocalScope, Boolean => CopTree] =
      apply(t.name, companion, t.tparams)
  }

  def memberName(t: Tree): TermName =
    TermName(freshName(s"andxor_${t.toString.toLowerCase.replace(".", "_")}"))

  def labels[P <: Param](tree: GenTree[P]): List[Tree] =
    tree.params.flatMap(_.label.defns)

  def mkAndxor[P <: Param](tree: GenTree[P]): Tree = q"$andxorPkg.AndXor[..${tree.tpes}]"

  def andxor[P <: Param](tree: GenTree[P]): Tree =
    valOrDef(Modifiers(), tree.andxorName, tree.tparamsNoVariance, Nil, tree.andxorTpe, mkAndxor(tree))

  def iso[P <: Param](tree: GenTree[P]): Tree =
    valOrDef(Modifiers(), tree.isoName, tree.tparamsNoVariance, Nil, tree.isoTpe, tree.iso)

  def implicits[P <: Param](tree: GenTree[P]): List[Tree] =
    tree match {
      case c: CopTree => c.children.zipWithIndex.flatMap { case (x, i) => x.fold(_ => Nil, o =>
        List(valOrDef(Modifiers(Flag.IMPLICIT), TermName(s"andxor_${o.name.decode}${if (c.labelled) "_labelled" else ""}_inst"),
          Nil, Nil, c.tpes(i), c.mkValue(Ident(o.name), c.params(i)))))
      }
      case p: ProdTree => Nil
    }

  case class Typeclass[P <: Param](
    tree: GenTree[P],
    typeclass: Tree,
    variance: Variance[P],
    memberName: TermName
  )

  def mkDerivedTypeclass[P <: Param](tc: Typeclass[P]): Tree =
    q"""
    $scalaPkg.Predef.implicitly[${tc.variance.typeclass}[${tc.typeclass}]].${tc.variance.mapFunction}(
      ${Ident(tc.tree.andxorName)}[..${tc.tree.tparamNames}].derivingId[${tc.typeclass}].${tc.variance.derivationFunction}
    )(${Ident(tc.tree.isoName)}[..${tc.tree.tparamNames}].${tc.variance.isoFunction})
    """

  def derivedTypeclass[P <: Param](tc: Typeclass[P]): Tree =
    valOrDef(Modifiers(Flag.IMPLICIT), tc.memberName, tc.tree.tparamsNoVariance,
      Some(tc.tree.abstractParams).filter(_.nonEmpty).fold(List[List[ValDef]]())(
        ps => List(ps.map(p => q"${Modifiers(Flag.IMPLICIT | Flag.PARAM | Flag.SYNTHETIC)} val ${TermName(freshName("ev"))}: ${tc.typeclass}[${p.tpe}]"))),
      tq"${tc.typeclass}[${tc.tree.tpe}]", mkDerivedTypeclass(tc))

  def getTypeclasses[P <: Param](tcs: List[Tree], tree: GenTree[P], variance: Variance[P]): List[Typeclass[P]] =
    tcs.map(tc => Typeclass(tree, termToType(tc), variance, memberName(tc)))

  def parseArgs[P <: Param](args: List[Tree], base: GenTree[P], labelled: GenTree[P]): List[Typeclass[Param]] = {
    val prod = Some(base).collect { case _: ProdTree => true }.getOrElse(false)
    val (co, contra) = if (prod) (CovariantProduct, ContravariantProduct) else  (CovariantCoproduct, ContravariantCoproduct)
    val tree = (l: Boolean) => if (l) labelled else base
    val err = (t: Tree, msg: String) => { error(t.pos, msg); Nil }

    setDebug(false)
    val (remArgs, tcs) = args.foldLeft((List[Tree](), List[Typeclass[Param]]())) { case ((remArgs, tcs), t) =>
      parseNamedArg(t, "debug") match {
        case Some(d) =>
          setDebug(d.equalsStructure(q"true"))
          (remArgs, tcs)
        case None => (parseNamedArg(t, "covariant").map(_ -> (co -> false)): Option[(Tree, (Variance[Param], Boolean))])
          .orElse(parseNamedArg(t, "labelledCovariant").map(_ -> (co -> true)))
          .orElse(parseNamedArg(t, "contravariant").map(_ -> (contra -> false)))
          .orElse(parseNamedArg(t, "labelledContravariant").map(_ -> (contra -> true))) match {
            case Some((q"List(..$xs)", (v, l))) => (remArgs, tcs ++ getTypeclasses[Param](xs, tree(l), v))
            case Some((q"Seq(..$xs)", (v, l))) => (remArgs, tcs ++ getTypeclasses[Param](xs, tree(l), v))
            case Some((q"Set(..$xs)", (v, l))) => (remArgs, tcs ++ getTypeclasses[Param](xs, tree(l), v))
            case Some((q"Vector(..$xs)", (v, l))) => (remArgs, tcs ++ getTypeclasses[Param](xs, tree(l), v))
            case None => (remArgs :+ t, tcs)
          }
      }
    }

    tcs ++ remArgs.flatMap(t => identOrSelect(t) match {
      case Some(x) =>
        val tc = x.fold(a => a, a => a)
        configuredTcs.get(showCode(tc)).flatMap(c => Option(if (prod) c.prod else c.cop)) match {
          case Some(x) => getTypeclasses(List(tc), tree(x._2), x._1)
          case None => err(t, s"No configuration found for deriving typeclass `${showCode(tc)}` over a ${if (prod) "" else "co"}product")
        }
      case None => err(t, s"Invalid argument to deriving annotation: ${showCode(t)}"); Nil
    })
  }

  def mkStats[P <: Param](mkTree: Boolean => GenTree[P], mods: List[Tree]): List[Tree] = {
    val modArgs = mods.flatMap(_ match {
      case Apply(Select(New(_), termNames.CONSTRUCTOR), args) => args
      case _ => Nil
    })
    val (base, labelled) = (mkTree(false), mkTree(true))
    val tcs = parseArgs(modArgs, base, labelled)
    val res = List(q"""
      object andxor {
        ..${labels(labelled) :::
            implicits(base) :::
            implicits(labelled) :::
            List(andxor(base), andxor(labelled), iso(base), iso(labelled))}
      }
    """) ::: Some(tcs).filter(_.nonEmpty).map(ts => q"import andxor._" :: ts.map(derivedTypeclass)).getOrElse(Nil)
    res
  }
}
