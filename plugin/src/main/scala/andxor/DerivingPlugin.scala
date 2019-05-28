package andxor

import scala.meta._
import scala.meta.contrib._
import scala.meta.contrib.equality.Structurally
import scala.tools.nsc.Global
import scalaz.Reader

class DerivingPlugin(global: Global) extends AnnotationPlugin(global) { self =>
  private val deriving = "deriving"

  val triggers: List[String] = List(deriving)

  override def updateCompanion(anns: List[Mod.Annot], c: Defn.Class, companion: Defn.Object): Reader[LocalScope, Defn.Object] =
    if (c.mods.has[Mod.Sealed]) CopTree(c, companion).map(f => regenObject(companion, mkStats(f, anns)))
    else if (c.mods.has[Mod.Case]) Reader(_ => regenObject(companion, mkStats(ProdTree(c, _), anns)))
    else Reader(_ => companion)

  override def updateCompanion(anns: List[Mod.Annot], t: Defn.Trait, companion: Defn.Object): Reader[LocalScope, Defn.Object] =
    if (t.mods.has[Mod.Sealed]) CopTree(t, companion).map(f => regenObject(companion, mkStats(f, anns)))
    else Reader(_ => companion)

  // Andxor types
  private lazy val andxorPkg = q"_root_.andxor"
  private lazy val andxorTpesPkg = q"_root_.andxor.types"
  private lazy val labelledObj = q"$andxorPkg.Labelled"

  // Scala types
  private lazy val scalaPkg = q"_root_.scala"

  // Scalaz types
  private lazy val scalazPkg = q"_root_.scalaz"
  private lazy val id = t"$scalazPkg.Id.Id"
  private lazy val isoSetObj = q"$scalazPkg.Isomorphism.IsoSet"
  private lazy val isoSetTpe = t"$scalazPkg.Isomorphism.IsoSet"
  private lazy val tagObj = q"$scalazPkg.Tag"
  private lazy val tagTpe = t"_root_.scalaz.@@"
  private lazy val adtValTagTpe = t"$andxorPkg.tags.ADTValue"
  private lazy val adtValTagObj = q"$tagObj.of[$adtValTagTpe]"
  private def mkAdtVal(inst: Term): Term = q"$adtValTagObj($inst)"
  private def adtValTpe(tpe: Type): Type = t"$tagTpe[$tpe, $adtValTagTpe]"

  sealed trait Param {
    val name: Name
    val tpe: Type
    lazy val label: Label = Label(name)
    lazy val labelledTpe: Type = t"_root_.andxor.Labelled.Aux[$tpe, ${label.singletonTpe}]"
    lazy val termName: Term.Name = Term.Name(name.value)
  }

  case class ProdParam(name: Name, tpe: Type) extends Param
  object ProdParam {
    def apply(p: Term.Param): ProdParam =
      p.decltpe match {
        case Some(tpe) => ProdParam(p.name, tpe)
        case None => g.abort(s"Failed to generate code for $name because parameter ${p.name} has no declared type")
      }
  }

  case class CopParam(member: Either[Defn.Class, Defn.Object]) extends Param {
    lazy val name: Name = member.fold(_.name, _.name)
    lazy val tpe: Type = member.fold(_ => memberTpe, _ => adtValTpe(memberTpe))
    lazy val memberTpe: Type = member.fold(ProdTree(_, false).tpe, o => Type.Singleton(o.name))
  }

  sealed abstract class Variance[+P <: Param] {
    val typeclass: Type
    val derivationFunction: Term.Name
    val mapFunction: Term.Name
    val isoFunction: Term.Name
  }
  sealed abstract class Covariant[+P <: Param] extends Variance[P] {
    val mapFunction = q"map"
    val isoFunction = q"from"
  }
  case object CovariantProduct extends Covariant[ProdParam] {
    val typeclass = t"_root_.scalaz.Apply"
    val derivationFunction = q"apply"
  }
  case object CovariantCoproduct extends Covariant[CopParam] {
    val typeclass = t"_root_.andxor.Alt"
    val derivationFunction = q"alt"
  }
  sealed trait Contravariant[+P <: Param] extends Variance[P] {
    val mapFunction = q"contramap"
    val isoFunction = q"to"
  }
  case object ContravariantProduct extends Contravariant[ProdParam] {
    val typeclass = t"_root_.andxor.Divide"
    val derivationFunction = q"divide"
  }
  case object ContravariantCoproduct extends Contravariant[CopParam] {
    val typeclass = t"_root_.andxor.Decidable"
    val derivationFunction = q"choose"
  }

  def termToType(term: Term): Type =
    term match {
      case Term.Name(name) => Type.Name(name)
      case Term.Select(qual, Term.Name(name)) => Type.Select(qual.asInstanceOf[Term.Ref], Type.Name(name))
      case _ => g.abort(s"Unable to convert term `${term.syntax}` to type")
    }

  def maybeTpeParams[T](tparams: List[Type.Param])(empty: => T, nonEmpty: List[Type] => T): T =
    Some(tparams).filter(_.nonEmpty).fold(empty)(ts => nonEmpty(ts.map(t => Type.Name(t.name.value))))

  def getTypeNames(tpe: Tree): Set[Type.Name] =
    (tpe match {
      case t @ Type.Name(_) => Set(t)
      case _ => Set()
    }) ++ tpe.children.flatMap(getTypeNames(_))

  def valOrDef(mods: List[Mod], name: Term.Name, tparams: List[Type.Param], params: List[List[Term.Param]], tpe: Type, body: Term): Defn =
    (tparams, params) match {
      case (Nil, Nil) => q"..$mods val ${Pat.Var(name)}: $tpe = $body"
      case _   =>        q"..$mods def $name[..${tparams}](...$params): $tpe = $body"
    }

  lazy val name = "deriving"

  private[andxor] lazy val andxorName = Term.Name("repr")
  private[andxor] lazy val isoName = Term.Name("iso")
  private[andxor] lazy val andxorLabelledName = Term.Name("labelledRepr")
  private[andxor] lazy val labelledIsoName = Term.Name("labelledIso")

  case class Label(paramName: Name) {
    // Don't use `Term.fresh` because label name needs to be deterministic
    lazy val valName = Term.Name(s"andxor_label_${paramName.value}")
    private lazy val implValName = Term.Name(s"${valName}_impl")
    lazy val defns: List[Defn.Val] = List(
      q"val ${Pat.Var(valName)}: String = ${Lit.String(paramName.value)}",
      q"implicit val ${Pat.Var(implValName)}: $singletonTpe = $valName"
    )
    lazy val singletonTpe: Type.Singleton = Type.Singleton(valName)
  }

  def extendsTpe(tpeName: Type.Name, inits: List[Init]): Boolean =
    inits.exists(_.tpe match {
      case Type.Name(name) if name == tpeName.value => true
      case _ => false
    })

  def childOfTpe(tpeName: Type.Name, tree: Tree): Option[Either[Defn.Class, Defn.Object]] =
    tree match {
      case o @ Defn.Object(_, _, Template(_, inits, _, _)) if extendsTpe(tpeName, inits) => Some(Right(o))
      case c @ Defn.Class(_, _, _, _, Template(_, inits, _, _)) if extendsTpe(tpeName, inits) => Some(Left(c))
      case _ => None
    }

  def getChildrenOfTpe(tpeName: Type.Name, companion: Defn.Object): Reader[LocalScope, List[Either[Defn.Class, Defn.Object]]] =
    Reader(scope => (companion.extract[Stat] ++
      scope.objects.values.toList ++
      scope.classes.values.toList).flatMap(childOfTpe(tpeName, _)))

  sealed abstract class GenTree[+P <: Param](
    val params: List[P],
    val name: Type.Name,
    val tparams: List[Type.Param],
    val copOrProd: String,
  ) {
    val labelled: Boolean

    lazy val tpe: Type = maybeTpeParams(tparams)(t"$name", ts => t"$name[..$ts]")
    lazy val tpes: List[Type] =
      if (labelled) params.map(_.labelledTpe)
      else params.map(_.tpe)

    lazy val andxorTpes: List[Type] = id :: tpes

    private lazy val andxorNName = s"AndXorK$arity"
    lazy val andxorObj: Term = q"$andxorPkg.${Term.Name(andxorNName)}"
    lazy val andxorTpe: Type = t"$andxorPkg.${Type.Name(andxorNName)}[..$andxorTpes]"

    private lazy val reprName = s"${copOrProd}${arity}"
    lazy val reprObj: Term = q"$andxorTpesPkg.${Term.Name(reprName)}"
    lazy val reprTpe: Type = t"$andxorTpesPkg.${Type.Name(reprName)}[..$andxorTpes]"

    def iso: Term

    lazy val isoTpe: Type = t"$isoSetTpe[$tpe, $reprTpe]"

    lazy val arity: Int = params.length

    lazy val abstractParams: List[Param] = {
      val abstractTpeNames = tparams.map(t => Structurally(t.name)).toSet
      params.filter(param => getTypeNames(param.tpe).exists(abstractTpeNames.contains(_)))
    }

    lazy val andxorName: Term.Name = if (labelled) andxorLabelledName else self.andxorName
    lazy val isoName: Term.Name = if (labelled) labelledIsoName else self.isoName

    def mkValue(inst: Term, param: Param): Term

    def normalizeValue(v: Term): Term = if (labelled) q"$v.value" else v
  }

  case class ProdTree(klass: Defn.Class, labelled: Boolean) extends GenTree[ProdParam](
    klass.ctor.paramss.flatMap(_.map(ProdParam(_))),
    klass.name,
    klass.tparams,
    "Prod"
  ) {
    lazy val paramss = klass.ctor.paramss

    private def tupleAccess(idx: Int): Term.Name = Term.Name(s"t$idx")

    private lazy val mkTuple: Term =
      params match {
        case Nil      => g.abort("TODO - support 0 parameter case classes")
        case p :: Nil => mkValue(q"x", p)
        case ps       => Term.Tuple(ps.map(mkValue(q"x", _)))
      }

    private lazy val constructorArgs: List[List[Term]] =
      paramss.zipWithIndex.map { case (group, i) =>
        group.zipWithIndex.map { case (_, j) => normalizeValue(q"x.${tupleAccess(i + j + 1)}") } }

    lazy val iso: Term =
      q"""
      $isoSetObj[$tpe, $reprTpe](
        (x: $tpe) => $reprObj[..$andxorTpes]($mkTuple),
        (x: $reprTpe) => new $tpe(...$constructorArgs))
      """

    def mkValue(inst: Term, param: Param): Term =
      if (labelled) q"$labelledObj[${param.tpe}, ${param.label.singletonTpe}]($inst.${param.termName}, ${param.label.valName})"
      else          q"$inst.${param.termName}"
  }

  case class CopTree(
    children: List[Either[Defn.Class, Defn.Object]],
    override val name: Type.Name,
    override val tparams: List[Type.Param],
    override val labelled: Boolean
  ) extends GenTree[CopParam](children.map(CopParam(_)), name, tparams, "Cop") {
    def mkValue(inst: Term, param: Param): Term = {
      val v = param.asInstanceOf[CopParam].member.fold(_ => inst, _ => mkAdtVal(inst))
      if (labelled) q"$labelledObj[${param.tpe}, ${param.label.singletonTpe}]($v, ${param.label.valName})"
      else v
    }

    def maybeUnwrap(inst: Term, paramIdx: Option[Int]): Term =
      paramIdx.flatMap(children(_).toOption).fold(inst)(_ => q"$adtValTagObj.unwrap($inst)")

    lazy val iso: Term = q"""
      $isoSetObj[$tpe, $reprTpe](
        (x: $tpe) => x match {
          ..case ${params.map(p => p"case inst: ${p.memberTpe} => $andxorName.inj(${mkValue(q"inst", p)})")}
        },
        (x: $reprTpe) => ${params.zipWithIndex.tail.foldRight[Term](
          if (params.length == 1 && labelled) maybeUnwrap(q"x.run.value", Some(0)) else q"x.run")(
          (t, acc) => q"""$acc.bimap(
            x => ${maybeUnwrap(normalizeValue(q"x"), Some(t._2 - 1))},
            x => ${maybeUnwrap(normalizeValue(q"x"), if (t._2 == params.length - 1) Some(t._2) else None)}).merge[$id[$tpe]]""")})
    """
  }

  object CopTree {
    def apply(name: Type.Name, companion: Defn.Object, tparams: List[Type.Param]): Reader[LocalScope, Boolean => CopTree] =
      getChildrenOfTpe(name, companion).map(children => labelled => new CopTree(children, name, tparams, labelled))

    def apply(c: Defn.Class, companion: Defn.Object): Reader[LocalScope, Boolean => CopTree] =
      apply(c.name, companion, c.tparams)

    def apply(t: Defn.Trait, companion: Defn.Object): Reader[LocalScope, Boolean => CopTree] =
      apply(t.name, companion, t.tparams)
  }

  def memberName(t: Tree): Term.Name =
    Term.fresh(s"andxor_${t.toString.toLowerCase.replace(".", "_")}")

  def labels[P <: Param](tree: GenTree[P]): List[Defn.Val] =
    tree.params.flatMap(_.label.defns)

  def mkAndxor[P <: Param](tree: GenTree[P]): Term = q"${tree.andxorObj}[..${tree.andxorTpes}]"

  def andxor[P <: Param](tree: GenTree[P]): Defn =
    valOrDef(Nil, tree.andxorName, tree.tparams, Nil, tree.andxorTpe, mkAndxor(tree))

  def iso[P <: Param](tree: GenTree[P]): Defn =
    valOrDef(Nil, tree.isoName, tree.tparams, Nil, tree.isoTpe, tree.iso)

  def implicits[P <: Param](tree: GenTree[P]): List[Defn] =
    tree match {
      case c: CopTree => c.children.zipWithIndex.flatMap { case (x, i) => x.fold(_ => Nil, o =>
        List(valOrDef(List(Mod.Implicit()), Term.Name(s"andxor_${o.name.value}${if (c.labelled) "_labelled" else ""}_inst"),
          Nil, Nil, c.tpes(i), c.mkValue(o.name, c.params(i)))))
      }
      case p: ProdTree => Nil
    }

  case class Typeclass[P <: Param](
    tree: GenTree[P],
    typeclass: Type,
    variance: Variance[P],
    memberName: Term.Name
  )

  def getTypeclasses0[P <: Param](tcs: List[Term], tree: GenTree[P], variance: Variance[P]): List[Typeclass[P]] =
    tcs.map(tc => Typeclass(tree, termToType(tc), variance, memberName(tc)))

  def getTypeclasses[P <: Param](args: List[List[Term]], base: GenTree[P], labelled: GenTree[P]): List[Typeclass[Param]] = {
    val (co, contra): (Covariant[Param], Contravariant[Param]) = base match {
      case _: ProdTree => (CovariantProduct, ContravariantProduct)
      case _: CopTree  => (CovariantCoproduct, ContravariantCoproduct)
    }
    List[(String, Variance[Param], Boolean)](
      ("covariant", co, false),
      ("labelledCovariant", co, true),
      ("contravariant", contra, false),
      ("labelledContravariant", contra, true)
    ).flatMap { case (term, variance, l) =>
      val tree = if (l) labelled else base
      args.flatten.flatMap(_ match {
        case Term.Assign(Term.Name(`term`), q"List(..$tcs)") => getTypeclasses0[Param](tcs, tree, variance)
        case Term.Assign(Term.Name(`term`), q"Set(..$tcs)") => getTypeclasses0[Param](tcs, tree, variance)
        case Term.Assign(Term.Name(`term`), q"Set(..$tcs)") => getTypeclasses0[Param](tcs, tree, variance)
        case Term.Assign(Term.Name(`term`), q"Vector(..$tcs)") => getTypeclasses0[Param](tcs, tree, variance)
        case _ => Nil
      })
    }
  }

  def mkDerivedTypeclass[P <: Param](tc: Typeclass[P]): Term =
    q"""
    $scalaPkg.Predef.implicitly[${tc.variance.typeclass}[${tc.typeclass}]]
      .${tc.variance.mapFunction}(
        ${maybeTpeParams(tc.tree.tparams)(tc.tree.andxorName, ts => q"${tc.tree.andxorName}[..$ts]")}
          .combineId[${tc.typeclass}].${tc.variance.derivationFunction}
      )(${tc.tree.isoName}.${tc.variance.isoFunction})
    """

  def derivedTypeclass[P <: Param](tc: Typeclass[P]): Defn =
    valOrDef(List(Mod.Implicit()), tc.memberName, tc.tree.tparams,
      Some(tc.tree.abstractParams).filter(_.nonEmpty).fold(List[List[Term.Param]]())(
        ps => List(ps.map(p => param"implicit ${Term.fresh("ev")}: ${tc.typeclass}[${p.tpe}]"))),
      t"${tc.typeclass}[${tc.tree.tpe}]", mkDerivedTypeclass(tc))

  def mkStats[P <: Param](mkTree: Boolean => GenTree[P], mods: List[Mod.Annot]): List[Stat] = {
    val (base, labelled) = (mkTree(false), mkTree(true))
    val tcs = mods.flatMap(m => getTypeclasses(m.init.argss, base, labelled))
    List(q"""
      object andxor {
        ..${labels(labelled) :::
            implicits(base) :::
            implicits(labelled) :::
            List(andxor(base), andxor(labelled), iso(base), iso(labelled))}
      }
    """) ::: (if (tcs.nonEmpty) List(q"import andxor._") else Nil) ::: tcs.map(derivedTypeclass)
  }
}
