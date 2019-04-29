package andxor

import scala.tools.nsc.Global

class AndXorPlugin(override val global: Global) extends AnnotationPlugin(global) {
  private val derivableAnn = "derivable"
  private val covariantAnn = "deriveCovariant"
  private val contravariantAnn = "deriveContravariant"

  val name: String = "andxor"
  val triggers: List[String] = List(derivableAnn, covariantAnn, contravariantAnn)

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

  private val andxorName = TermName("andxor")
  private val isoName = TermName("andxorIso")

  implicit class ClassDefOps(c: ClassDef) {
    def arity: Int = paramGroups.flatten.length

    private def isImplGroup(vs: List[ValDef]): Boolean =
      vs.exists(_.mods.hasFlag(Flag.IMPLICIT))

    private def getParams: (List[List[ValDef]], List[ValDef]) =
      c.impl.collect { case d@DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => d } match {
        case DefDef(_, _, _, vh :: vt, _, _) :: Nil if !isImplGroup(vh) =>
          vt.span(!isImplGroup(_)) match {
            case (e, i :: Nil) => (vh :: e, i)
            case (e, Nil) => (vh :: e, Nil)
            case _ => abort(s"Found more than one implicit parameter group for ${c.name}")
          }
        case _ => abort(s"Failed to find exactly one constructor for ${c.name}")
      }

    def paramGroups: List[List[ValDef]] = getParams._1
    def implParams: List[ValDef] = getParams._2

    def classTpe: AppliedTypeTree = AppliedTypeTree(Ident(c.name), c.tparams.map(t => Ident(t.name)))

    def labelledTpes: List[AppliedTypeTree] =
      paramGroups.flatten.map(p => AppliedTypeTree(labelledTpe, List(p.tpt.duplicate, strLit(p.name.decodedName.toString))))

    def andxorTpes(F: Option[Tree]): List[Tree] =
      F.toList ++ labelledTpes

    private def andxorGen(f: String => Name): Tree = Select(q"$andxorPkg", f(s"AndXor${c.arity}"))
    def andxorObj: Tree = andxorGen(TermName(_))
    def andxorTpe: Tree = AppliedTypeTree(andxorGen(TypeName(_)), andxorTpes(None))

    private def prodGen(f: String => Name): Tree = Select(q"$andxorPkg.types", f(s"Prod${c.arity}"))
    def prodObj: Tree = prodGen(TermName(_))
    def prodTpe: Tree = AppliedTypeTree(prodGen(TypeName(_)), andxorTpes(Some(idTpe)))

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

  def updateCompanion(triggered: List[Tree], klass: ClassDef, companion: ModuleDef): ModuleDef = {
    triggered.map(getTypeclasses(_))
    regenModule(companion, List(
      genAndxor(klass),
      genIso(klass)
    ) ++ triggered.flatMap(genDerivedTypeclasses(_, klass)))
  }

  def updateModule(triggered: List[Tree], module: ModuleDef): ModuleDef = module

  def valOrDef[A](klass: ClassDef)(forVal: ClassDef => A, forDef: ClassDef => A): A =
    (klass.tparams, klass.implParams) match {
      case (Nil, Nil) => forVal(klass)
      case _ => forDef(klass)
    }

  def strLit(name: String): TypeTree = TypeTree(constantType(Constant(name)))

  def mkAndxor(klass: ClassDef): Tree =
    TypeApply(klass.andxorObj, klass.andxorTpes(None))

  def andxorVal(klass: ClassDef): Tree =
    ValDef(
      Modifiers(Flag.SYNTHETIC),
      andxorName,
      klass.andxorTpe,
      mkAndxor(klass)
    )

  def andxorDef(klass: ClassDef): DefDef =
    DefDef(
      Modifiers(Flag.SYNTHETIC),
      andxorName,
      klass.tparams.map(_.duplicate),
      Nil,
      klass.andxorTpe,
      mkAndxor(klass)
    )

  def genAndxor(klass: ClassDef): Tree = valOrDef(klass)(andxorVal, andxorDef)

  def mkTuple(c: ClassDef): List[Tree] = {
    val params = c.paramGroups.flatten
      .map(p => q"$labelledObj.apply[${p.tpt.duplicate}](x.${p.name}, ${Literal(Constant(p.name.decodedName.toString))})")
    if (params.length <= 1) params else List(Apply(tupleN(c.arity), params))
    // List(params.dropRight(1).foldRight(params.last)((p, acc) => Apply(tuple2, List(p, acc))))
  }

  def tupleAccess(idx: Int): Select = Select(Ident("x"), TermName(s"t$idx"))

  def mkIso(c: ClassDef): Tree =
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
            (i + group.length, Apply(acc, group.zipWithIndex.map(t => Select(tupleAccess(i + t._2), TermName("value")))))
          }._2)))

  def isoVal(klass: ClassDef): ValDef =
    ValDef(
      Modifiers(Flag.SYNTHETIC),
      isoName,
      AppliedTypeTree(isoTpe, List(Ident(klass.name), klass.prodTpe)),
      mkIso(klass)
    )

  def isoDef(klass: ClassDef): DefDef =
    DefDef(
      Modifiers(Flag.SYNTHETIC),
      isoName,
      klass.tparams.map(_.duplicate),
      Some(klass.implParams).filter(_.nonEmpty).map(List(_)).getOrElse(Nil),
      AppliedTypeTree(isoTpe, List(klass.classTpe, klass.prodTpe)),
      mkIso(klass)
    )

  def genIso(klass: ClassDef): Tree = valOrDef(klass)(isoVal, isoDef)

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
  type Typeclass = (Variance, (TermName, TreeTypeName))

  def getTypeclasses0(ann: Tree): List[(TermName, TreeTypeName)] =
    ann.children.collect {
      case s @ Select(_, t) if t != nme.CONSTRUCTOR => TreeTermName(s)
      case i @ Ident(_)                             => TreeTermName(i)
    }.map(ttn => memberName(ttn.tree) -> ttn.toTypeName)

  def getTypeclasses(ann: Tree): List[Typeclass] =
    annotationName(ann) match {
      case TermName(`derivableAnn`) => Nil
      case TermName(`covariantAnn`) => getTypeclasses0(ann).map((Covariant, _))
      case TermName(`contravariantAnn`) => getTypeclasses0(ann).map((Contravariant, _))
      case t => abort(s"Unknown andxor annotation: $t")
    }

  def derivedTypeclass(variance: Variance, typeclass: TreeTypeName, klass: ClassDef): Tree =
    Apply(
      Apply(
        Select(q"$scalaPkg.Predef.implicitly[${variance.typeclassTpe}[${typeclass.tree.duplicate}]]", variance.mapFunction),
        List(
          Select(
            Apply(
              TypeApply(
                Select(
                  Ident(andxorName),
                  TermName("combine")
                ),
                List(typeclass.tree.duplicate)
              ),
              klass.labelledTpes.map(t => q"$scalaPkg.Predef.implicitly[${typeclass.tree.duplicate}[$t]]")
            ),
            variance.derivationFunction
          )
        )
      ),
      List(Select(Ident(isoName), variance.isoFunction))
    )

  def derivedTypeclassVal(klass: ClassDef): Typeclass => ValDef = { case (variance, (memberName, typeclass)) =>
    ValDef(
      Modifiers(Flag.IMPLICIT | Flag.SYNTHETIC),
      memberName,
      AppliedTypeTree(typeclass.tree.duplicate, List(klass.classTpe)),
      derivedTypeclass(variance, typeclass, klass)
    )
  }

  def derivedTypeclassDef(klass: ClassDef): Typeclass => DefDef = { case (variance, (memberName, typeclass)) =>
    DefDef(
      Modifiers(Flag.IMPLICIT | Flag.SYNTHETIC),
      memberName,
      klass.tparams.map(_.duplicate),
      List(klass.implParams ++ klass.tparams.zipWithIndex.map { case (t, i) =>
        ValDef(
          Modifiers(Flag.IMPLICIT | Flag.PARAM | Flag.SYNTHETIC),
          TermName(s"evidence$$$i"),
          AppliedTypeTree(typeclass.tree.duplicate, List(Ident(t.name))),
          EmptyTree
        )
      }),
      AppliedTypeTree(typeclass.tree.duplicate, List(klass.classTpe)),
      derivedTypeclass(variance, typeclass, klass)
    )
  }

  def genDerivedTypeclasses(ann: Tree, klass: ClassDef): List[Tree] = {
    val gen: Typeclass => Tree = valOrDef(klass)(derivedTypeclassVal _, derivedTypeclassDef _)
    getTypeclasses(ann).map(gen)
  }
}
