package andxor

import scala.tools.nsc.Global

class AndXorPlugin(override val global: Global) extends AnnotationPlugin(global) {
  private val derivableAnn = "derivable"
  private val covariantAnn = "deriveCovariant"
  private val labelledCovariantAnn = "deriveLabelledCovariant"
  private val contravariantAnn = "deriveContravariant"
  private val labelledContravariantAnn = "deriveLabelledContravariant"

  val name: String = "andxor"
  val triggers: List[String] = List(
    derivableAnn,
    covariantAnn,
    labelledCovariantAnn,
    contravariantAnn,
    labelledContravariantAnn
  )

  import global._
  import global.internal.constantType

  case class TreeTypeName(tree: Tree) {
    def toTermName: TreeTermName =
      TreeTermName(tree match {
        case Ident(name)        => Ident(name.toTermName)
        case Select(qual, name) => Select(qual, name.toTermName)
      })
  }
  case class TreeTermName(tree: Tree) {
    def toTypeName: TreeTypeName =
      TreeTypeName(tree match {
        case Ident(name)        => Ident(name.toTypeName)
        case Select(qual, name) => Select(qual, name.toTypeName)
      })
  }

  def memberName(t: Tree): TermName =
    TermName(s"_andxor_${t.toString.toLowerCase.replace(".", "_")}").encodedName.toTermName

  private val andxorPkg = q"_root_.andxor"
  private val labelledObj = q"$andxorPkg.Labelled"
  private val labelledTpe = Select(labelledObj, TypeName("Aux"))

  private val scalaPkg = q"_root_.scala"
  private def tupleN(n: Int): Tree = Select(Select(scalaPkg, TermName(s"Tuple$n")), TermName("apply"))
  // private val tuple2 = tupleN(2)

  private val scalazPkg = q"_root_.scalaz"
  private val idTpe = Select(q"$scalazPkg.Id", TypeName("Id"))
  private val isoPkg = q"$scalazPkg.Isomorphism"
  private val isoObj = q"$isoPkg.IsoSet"
  private val isoTpe = Select(isoPkg, TypeName("IsoSet"))

  trait GenClassDef {
    val klass: ClassDef
    def andxorName: TermName
    def isoName: TermName
    def tpes: List[Tree]

    def mkValue(inst: Tree, param: ValDef): Tree
    def normalizeValue(v: Tree): Tree

    def arity: Int = paramGroups.flatten.length

    private def isImplGroup(vs: List[ValDef]): Boolean =
      vs.exists(_.mods.hasFlag(Flag.IMPLICIT))

    private def getParams: (List[List[ValDef]], List[ValDef]) =
      klass.impl.collect { case d@DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => d } match {
        case DefDef(_, _, _, vh :: vt, _, _) :: Nil if !isImplGroup(vh) =>
          vt.span(!isImplGroup(_)) match {
            case (e, i :: Nil) => (vh :: e, i)
            case (e, Nil) => (vh :: e, Nil)
            case _ => abort(s"Found more than one implicit parameter group for ${klass.name}")
          }
        case _ => abort(s"Failed to find exactly one constructor for ${klass.name}")
      }

    def paramGroups: List[List[ValDef]] = getParams._1
    def implParams: List[ValDef] = getParams._2

    def classTpe: AppliedTypeTree = AppliedTypeTree(Ident(klass.name), klass.tparams.map(t => Ident(t.name)))

    def andxorTpes(F: Option[Tree]): List[Tree] = F.toList ++ tpes

    private def andxorGen(f: String => Name): Tree = Select(q"$andxorPkg", f(s"AndXor$arity"))
    def andxorObj: Tree = andxorGen(TermName(_))
    def andxorTpe: Tree = AppliedTypeTree(andxorGen(TypeName(_)), andxorTpes(None))

    private def prodGen(f: String => Name): Tree = Select(q"$andxorPkg.types", f(s"Prod$arity"))
    def prodObj: Tree = prodGen(TermName(_))
    def prodTpe: Tree = AppliedTypeTree(prodGen(TypeName(_)), andxorTpes(Some(idTpe)))

    def name: TypeName = klass.name
    def tparams: List[TypeDef] = klass.tparams
  }

  case class BaseClassDef(klass: ClassDef) extends GenClassDef {
    val andxorName = TermName("andxor")
    val isoName = TermName("andxorIso")
    def tpes: List[Tree] = paramGroups.flatten.map(_.tpt.duplicate)
    def mkValue(inst: Tree, param: ValDef): Tree = q"$inst.${param.name}"
    def normalizeValue(v: Tree): Tree = v
  }

  case class LabelledClassDef(klass: ClassDef) extends GenClassDef {
    val andxorName = TermName("andxorLabelled")
    val isoName = TermName("andxorLabelledIso")
    def tpes: List[Tree] =
      paramGroups.flatten.map(p => AppliedTypeTree(labelledTpe, List(p.tpt.duplicate, strLit(p.name.decodedName.toString))))
    def mkValue(inst: Tree, param: ValDef): Tree =
      q"$labelledObj.apply[${param.tpt.duplicate}]($inst.${param.name}, ${Literal(Constant(param.name.decodedName.toString))})"
    def normalizeValue(v: Tree): Tree = Select(v, TermName("value"))
  }

  def regenModule(comp: ModuleDef, extras: List[Tree]): ModuleDef =
    treeCopy.ModuleDef(
      comp,
      comp.mods,
      comp.name,
      treeCopy.Template(
        comp.impl,
        comp.impl.parents,
        comp.impl.self,
        comp.impl.body ::: extras.map(_.withAllPos(comp.pos))
      )
    )

  def updateClass(triggered: List[Tree], klass: ClassDef): ClassDef = klass

  def updateCompanion(triggered: List[Tree], klass: ClassDef, companion: ModuleDef): ModuleDef =
    regenModule(companion, List(
      genAndxor(BaseClassDef(klass)),
      genAndxor(LabelledClassDef(klass)),
      genIso(BaseClassDef(klass)),
      genIso(LabelledClassDef(klass))
    ) ++ triggered.flatMap(genDerivedTypeclasses(_, klass)))

  def updateModule(triggered: List[Tree], module: ModuleDef): ModuleDef = module

  def valOrDef[A](tparams: List[TypeDef], implParams: List[Tree])(forVal: => A, forDef: => A): A =
    (tparams, implParams) match {
      case (Nil, Nil) => forVal
      case _ => forDef
    }

  def valOrDef[A](klass: GenClassDef)(forVal: => A, forDef: => A): A =
    valOrDef(klass.tparams, klass.implParams)(forVal, forDef)

  def strLit(name: String): TypeTree = TypeTree(constantType(Constant(name)))

  def mkAndxor(klass: GenClassDef): Tree =
    TypeApply(klass.andxorObj, klass.andxorTpes(None))

  def andxorVal(klass: GenClassDef): Tree =
    ValDef(
      Modifiers(Flag.SYNTHETIC),
      klass.andxorName,
      klass.andxorTpe,
      mkAndxor(klass)
    )

  def andxorDef(klass: GenClassDef): DefDef =
    DefDef(
      Modifiers(Flag.SYNTHETIC),
      klass.andxorName,
      klass.tparams.map(_.duplicate),
      Nil,
      klass.andxorTpe,
      mkAndxor(klass)
    )

  def genAndxor(klass: GenClassDef): Tree = valOrDef(klass)(andxorVal(klass), andxorDef(klass))

  def mkTuple(klass: GenClassDef): List[Tree] = {
    val params = klass.paramGroups.flatten.map(klass.mkValue(Ident("x"), _))
    if (params.length <= 1) params else List(Apply(tupleN(klass.arity), params))
    // List(params.dropRight(1).foldRight(params.last)((p, acc) => Apply(tuple2, List(p, acc))))
  }

  def tupleAccess(idx: Int): Select = Select(Ident("x"), TermName(s"t$idx"))

  def mkIso(c: GenClassDef): Tree =
    if (isIde || isScaladoc) Literal(Constant(null))
    else Apply(
      Select(isoObj, TermName("apply")),
      List(
        Function(
          List(ValDef(Modifiers(Flag.PARAM | Flag.SYNTHETIC), TermName("x"), c.classTpe, EmptyTree)),
          Apply(
            TypeApply(Select(c.prodObj, TermName("apply")), c.andxorTpes(Some(idTpe))),
            mkTuple(c))),
        Function(
          List(ValDef(Modifiers(Flag.PARAM | Flag.SYNTHETIC), TermName("x"), c.prodTpe, EmptyTree)),
          c.paramGroups.foldLeft[(Int, Tree)]((1, Select(New(Ident(c.name)), nme.CONSTRUCTOR))) { case ((i, acc), group) =>
            (i + group.length, Apply(acc, group.zipWithIndex.map(t => c.normalizeValue(tupleAccess(i + t._2)))))
          }._2)))

  def isoVal(klass: GenClassDef): ValDef =
    ValDef(
      Modifiers(Flag.SYNTHETIC),
      klass.isoName,
      AppliedTypeTree(isoTpe, List(Ident(klass.name), klass.prodTpe)),
      mkIso(klass)
    )

  def isoDef(klass: GenClassDef): DefDef =
    DefDef(
      Modifiers(Flag.SYNTHETIC),
      klass.isoName,
      klass.tparams.map(_.duplicate),
      Some(klass.implParams).filter(_.nonEmpty).map(List(_)).getOrElse(Nil),
      AppliedTypeTree(isoTpe, List(klass.classTpe, klass.prodTpe)),
      mkIso(klass)
    )

  def genIso(klass: GenClassDef): Tree = valOrDef(klass)(isoVal(klass), isoDef(klass))

  sealed trait Variance {
    val typeclassTpe: Tree
    val derivationFunction: TermName
    val mapFunction: TermName
    val isoFunction: TermName
  }
  case object Covariant extends Variance {
    val typeclassTpe = Select(scalazPkg, TypeName("Apply"))
    val derivationFunction = TermName("apply")
    val mapFunction = TermName("map")
    val isoFunction = TermName("from")
  }
  case object Contravariant extends Variance {
    val typeclassTpe = Select(andxorPkg, TypeName("Divide"))
    val derivationFunction = TermName("divide")
    val mapFunction = TermName("contramap")
    val isoFunction = TermName("to")
  }
  case class Typeclass(
    variance: Variance,
    klass: GenClassDef,
    memberName: TermName,
    typeclass: TreeTypeName,
  )

  def getTypeclasses0(ann: Tree): List[(TermName, TreeTypeName)] =
    ann.children.collect {
      case s @ Select(_, t) if t != nme.CONSTRUCTOR => TreeTermName(s)
      case i @ Ident(_)                             => TreeTermName(i)
    }.map(ttn => memberName(ttn.tree) -> ttn.toTypeName)

  def getTypeclasses(ann: Tree, klass: ClassDef): List[Typeclass] = {
    val (baseKlass, labelledKlass) = (BaseClassDef(klass), LabelledClassDef(klass))
    val unlabelled = (v: Variance) => (t: (TermName, TreeTypeName)) => Typeclass(v, baseKlass, t._1, t._2)
    val labelled = (v: Variance) => (t: (TermName, TreeTypeName)) => Typeclass(v, labelledKlass, t._1, t._2)
    val f = (g: ((TermName, TreeTypeName)) => Typeclass) => getTypeclasses0(ann).map(g)

    annotationName(ann) match {
      case TermName(`derivableAnn`) => Nil
      case TermName(x @ (`covariantAnn` | `contravariantAnn`)) =>
        f(unlabelled(if (x == covariantAnn) Covariant else Contravariant))
      case TermName(x @ (`labelledCovariantAnn` | `labelledContravariantAnn`)) =>
        f(labelled(if (x == labelledCovariantAnn) Covariant else Contravariant))
      case t => abort(s"Unknown andxor annotation: $t")
    }
  }

  def derivedTypeclass(tc: Typeclass): Tree =
    Apply(
      Apply(
        Select(q"$scalaPkg.Predef.implicitly[${tc.variance.typeclassTpe}[${tc.typeclass.tree.duplicate}]]", tc.variance.mapFunction),
        List(
          Select(
            Apply(
              TypeApply(
                Select(
                  Ident(tc.klass.andxorName),
                  TermName("combine")
                ),
                List(tc.typeclass.tree.duplicate)
              ),
              // TODO - why do I have to call `implicitly` to get the labelled type to be picked up correctly?
              /*
              [error] AndXorPluginTest.scala:53:14: implicit error;
              [error] !I a0: Read[Labelled[String] {type L = Witness.Aux[String]}]
              [error]   case class Test2(
              */
              tc.klass.tpes.map(t => q"$scalaPkg.Predef.implicitly[${tc.typeclass.tree.duplicate}[$t]]")
            ),
            tc.variance.derivationFunction
          )
        )
      ),
      List(Select(Ident(tc.klass.isoName), tc.variance.isoFunction))
    )

  def derivedTypeclassVal(tc: Typeclass): ValDef =
    ValDef(
      Modifiers(Flag.IMPLICIT | Flag.SYNTHETIC),
      tc.memberName,
      AppliedTypeTree(tc.typeclass.tree.duplicate, List(tc.klass.classTpe)),
      derivedTypeclass(tc)
    )

  def derivedTypeclassDef(tc: Typeclass): DefDef =
    DefDef(
      Modifiers(Flag.IMPLICIT | Flag.SYNTHETIC),
      tc.memberName,
      tc.klass.tparams.map(_.duplicate),
      List(tc.klass.implParams ++ tc.klass.tparams.zipWithIndex.map { case (t, i) =>
        ValDef(
          Modifiers(Flag.IMPLICIT | Flag.PARAM | Flag.SYNTHETIC),
          TermName(s"evidence$$$i"),
          AppliedTypeTree(tc.typeclass.tree.duplicate, List(Ident(t.name))),
          EmptyTree
        )
      }),
      AppliedTypeTree(tc.typeclass.tree.duplicate, List(tc.klass.classTpe)),
      derivedTypeclass(tc)
    )

  def genDerivedTypeclasses(ann: Tree, klass: ClassDef): List[Tree] = {
    val gen: Typeclass => Tree = valOrDef(klass.tparams, BaseClassDef(klass).implParams)(derivedTypeclassVal _, derivedTypeclassDef _)
    getTypeclasses(ann, klass).map(gen)
  }
}
