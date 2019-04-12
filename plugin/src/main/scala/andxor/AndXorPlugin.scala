package andxor

import scala.tools.nsc.Global

class AndXorPlugin(override val global: Global) extends AnnotationPlugin(global) {
  val name: String = "andxor"
  val triggers: List[String] = List("andxor")

  import global._
  import global.internal.constantType

  val scalaPkg = Select(Ident(nme.ROOTPKG), nme.scala_)
  val tuple2Apply = Select(Select(scalaPkg, TermName("Tuple2")), TermName("apply"))

  val andxorPkg = Select(Ident(nme.ROOTPKG), TermName("andxor"))
  val labelledObj = Select(andxorPkg, TermName("Labelled"))
  val labelledTpe = Select(labelledObj, TypeName("Aux"))

  def strLit(name: String): TypeTree = TypeTree(constantType(Constant(name)))

  val scalazPkg = Select(Ident(nme.ROOTPKG), TermName("scalaz"))
  val isoPkg = Select(scalazPkg, TermName("Isomorphism"))
  val isoObj = Select(isoPkg, TermName("IsoSet"))
  val isoTpe = Select(isoPkg, TypeName("IsoSet"))

  val idTpe = Select(Select(scalazPkg, TermName("Id")), TypeName("Id"))

  def getTpes(c: ClassT, fTpe: Option[Tree]): List[Tree] =
    fTpe.toList ++ c.paramGroups.toList.flatten.map(p =>
      AppliedTypeTree(labelledTpe, List(p.tpt.duplicate, strLit(p.name.decodedName.toString))))

  def applyTpes(t: Tree, c: ClassT, fTpe: Option[Tree]): Tree =
    AppliedTypeTree(t, getTpes(c, fTpe))

  def andxorProdGen(c: ClassT, f: String => Name): Tree =
    Select(Select(andxorPkg, TermName("types")), f(s"Prod${c.arity}"))

  def andxorProdObj(c: ClassT): Tree = andxorProdGen(c, TermName(_))
  def andxorProdTpe(c: ClassT): Tree = applyTpes(andxorProdGen(c, TypeName(_)), c, Some(idTpe))

  def mkTuple(c: ClassT): List[Tree] = {
    val params = c.paramGroups.toList.flatten.map(p =>
      Apply(
        TypeApply(Select(labelledObj, TermName("apply")), List(p.tpt.duplicate)),
        List(
          Select(Ident("x"), p.name),
          Literal(Constant(p.name.decodedName.toString)))))
    List(params.dropRight(1).foldRight(params.last)((p, acc) => Apply(tuple2Apply, List(p, acc))))
  }

  def tupleAccess(idx: Int): Select = Select(Ident("x"), TermName(s"t$idx"))

  def toIso(c: ClassT): Tree =
    if (isIde || isScaladoc) Literal(Constant(null))
    else Apply(
      Select(isoObj, TermName("apply")),
      List(
        Function(
          List(ValDef(Modifiers(), TermName("x"), c.classTpe, EmptyTree)),
          Apply(
            TypeApply(Select(andxorProdObj(c), TermName("apply")), getTpes(c, Some(idTpe))),
            mkTuple(c))),
        Function(
          List(ValDef(Modifiers(), TermName("x"), andxorProdTpe(c), EmptyTree)),
          c.paramGroups.toList.foldLeft[(Int, Tree)]((1, Select(New(Ident(c.klass.name)), nme.CONSTRUCTOR))) { case ((i, acc), group) =>
            (i + group.length, Apply(acc, group.zipWithIndex.map(t => Select(tupleAccess(i + t._2), TermName("value")))))
          }._2)))

  def updateClass(triggered: List[Tree], klass: ClassT): ClassT = klass

  def updateCompanion(triggered: List[Tree], klass: ClassT, companion: ModuleDef): ModuleDef =
    regenModule(companion, List(genIso(TermName("andxorIso"), klass)))

  def updateModule(triggered: List[Tree], module: ModuleDef): ModuleDef = module

  def genIso(memberName: TermName, klass: ClassT): Tree = {
    val tpeParams = klass.tpeParams.map(_.toList).getOrElse(Nil)
    (tpeParams, klass.implParams) match {
      case (Nil, None) => ValDef(
        Modifiers(Flag.IMPLICIT | Flag.SYNTHETIC),
        memberName,
        AppliedTypeTree(isoTpe, List(Ident(klass.klass.name), andxorProdTpe(klass))),
        toIso(klass)
      )
      case _ => DefDef(
        Modifiers(),
        memberName,
        tpeParams,
        klass.implParams.map(ps => List(ps.toList)).getOrElse(Nil),
        AppliedTypeTree(isoTpe, List(klass.classTpe, andxorProdTpe(klass))),
        toIso(klass)
      )
    }
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
