package andxor

import scala.tools.nsc.Global

class AndXorPlugin(override val global: Global) extends AnnotationPlugin(global) {
  val name: String = "andxor"
  val triggers: List[String] = List("andxor")

  import global._
  import global.internal.constantType

  private val andxorPkg = q"_root_.andxor"
  private val labelledObj = q"$andxorPkg.Labelled"
  private val labelledTpe = Select(labelledObj, TypeName("Aux"))

  private val scalaPkg = q"_root_.scala"
  private val tuple2 = q"$scalaPkg.Tuple2.apply"

  private val scalazPkg = q"_root_.scalaz"
  private val idTpe = Select(q"$scalazPkg.Id", TypeName("Id"))
  private val isoPkg = q"$scalazPkg.Isomorphism"
  private val isoObj = q"$isoPkg.IsoSet"
  private val isoTpe = Select(isoPkg, TypeName("IsoSet"))

  private val andxorIsoName = TermName("andxorIso")

  implicit class ClassDefOps(c: ClassDef) {
    def arity: Int = paramGroups.toList.flatten.length

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

    def prodTpes(F: Option[Tree]): List[Tree] =
      F.toList ++ c.paramGroups.toList.flatten.map(p =>
        AppliedTypeTree(labelledTpe, List(p.tpt.duplicate, strLit(p.name.decodedName.toString))))

    private def prodGen(f: String => Name): Tree = Select(q"$andxorPkg.types", f(s"Prod${c.arity}"))
    def prodObj: Tree = prodGen(TermName(_))
    def prodTpe: Tree = AppliedTypeTree(prodGen(TypeName(_)), c.prodTpes(Some(idTpe)))
  }

  def updateClass(triggered: List[Tree], klass: ClassDef): ClassDef = klass

  def updateCompanion(triggered: List[Tree], klass: ClassDef, companion: ModuleDef): ModuleDef =
    regenModule(companion, List(genIso(klass)))

  def updateModule(triggered: List[Tree], module: ModuleDef): ModuleDef = module

  def strLit(name: String): TypeTree = TypeTree(constantType(Constant(name)))

  def mkTuple(c: ClassDef): List[Tree] = {
    val params = c.paramGroups.toList.flatten
      .map(p => q"$labelledObj.apply[${p.tpt.duplicate}](x.${p.name}, ${Literal(Constant(p.name.decodedName.toString))})")
    List(params.dropRight(1).foldRight(params.last)((p, acc) => Apply(tuple2, List(p, acc))))
  }

  def tupleAccess(idx: Int): Select = Select(Ident("x"), TermName(s"t$idx"))

  def mkIso(c: ClassDef): Tree =
    if (isIde || isScaladoc) Literal(Constant(null))
    else Apply(
      Select(isoObj, TermName("apply")),
      List(
        Function(
          List(ValDef(Modifiers(), TermName("x"), c.classTpe, EmptyTree)),
          Apply(
            TypeApply(Select(c.prodObj, TermName("apply")), c.prodTpes(Some(idTpe))),
            mkTuple(c))),
        Function(
          List(ValDef(Modifiers(), TermName("x"), c.prodTpe, EmptyTree)),
          c.paramGroups.toList.foldLeft[(Int, Tree)]((1, Select(New(Ident(c.name)), nme.CONSTRUCTOR))) { case ((i, acc), group) =>
            (i + group.length, Apply(acc, group.zipWithIndex.map(t => Select(tupleAccess(i + t._2), TermName("value")))))
          }._2)))

  def isoValDef(klass: ClassDef): ValDef =
    ValDef(
      Modifiers(),
      andxorIsoName,
      AppliedTypeTree(isoTpe, List(Ident(klass.name), klass.prodTpe)),
      mkIso(klass)
    )

  def isoDefDef(klass: ClassDef): DefDef =
    DefDef(
      Modifiers(),
      andxorIsoName,
      klass.tparams.map(_.duplicate),
      Some(klass.implParams).filter(_.nonEmpty).map(List(_)).getOrElse(List()),
      AppliedTypeTree(isoTpe, List(klass.classTpe, klass.prodTpe)),
      mkIso(klass)
    )

  def genIso(klass: ClassDef): Tree =
    (klass.tparams, klass.implParams) match {
      case (Nil, Nil) => isoValDef(klass)
      case _ => isoDefDef(klass)
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
}
