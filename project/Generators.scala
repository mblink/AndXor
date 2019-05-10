package andxor

import scala.meta._
import scala.meta.prettyprinters._
import scala.meta.gen._

object generators {
  // Andxor types
  private lazy val andxorPkg = q"_root_.andxor"
  private lazy val labelled = q"$andxorPkg.Labelled"

  // Scala types
  private lazy val scalaPkg = q"_root_.scala"

  // Scalaz types
  private lazy val scalazPkg = q"_root_.scalaz"
  private lazy val id = t"$scalazPkg.Id.Id"
  private lazy val isoSetObj = q"$scalazPkg.Isomorphism.IsoSet"
  private lazy val isoSetTpe = t"$scalazPkg.Isomorphism.IsoSet"

  lazy val all: Set[Generator] = Set(Deriving)

  sealed trait Variance {
    val typeclass: Type
    val derivationFunction: Term.Name
    val mapFunction: Term.Name
    val isoFunction: Term.Name
  }
  case object Covariant extends Variance {
    val typeclass = t"_root_.scalaz.Apply"
    val derivationFunction = q"apply"
    val mapFunction = q"map"
    val isoFunction = q"from"
  }
  case object Contravariant extends Variance {
    val typeclass = t"_root_.andxor.Divide"
    val derivationFunction = q"divide"
    val mapFunction = q"contramap"
    val isoFunction = q"to"
  }

  private case class PrettyPrinter(level: Int, inQuotes: Boolean, backslashed: Boolean) {
    val indent = List.fill(level)("  ").mkString

    def transform(char: Char): (PrettyPrinter, String) = {
      val (pp, f): (PrettyPrinter, PrettyPrinter => String) = char match {
        case '"' if inQuotes && !backslashed => (copy(inQuotes = false), _ => s"$char")
        case '"' if !inQuotes => (copy(inQuotes = true), _ => s"$char")
        case '\\' if inQuotes && !backslashed => (copy(backslashed = true), _ => s"$char")

        case ',' if !inQuotes => (this, p => s",\n${p.indent}")
        case '(' if !inQuotes => (copy(level = level + 1), p => s"(\n${p.indent}")
        case ')' if !inQuotes => (copy(level = level - 1), p => s"\n${p.indent})")
        case _ => (this, _ => s"$char")
      }
      (pp, f(pp))
    }
  }

  private def prettyPrint(raw: String): String =
    raw.foldLeft((PrettyPrinter(0, false, false), new StringBuilder(""))) { case ((pp, sb), char) =>
      val (newPP, res) = pp.transform(char)
      (newPP, sb.append(res))
    }._2.toString.replaceAll("""\(\s+\)""", "()")

  private def treeStructure(tree: Tree, pretty: Boolean): String =
    if (pretty) prettyPrint(tree.structure) else tree.structure

  def debug(name: String, tree: Tree, pretty: Boolean = true): Unit =
    println(s"====\n$name ${tree.pos}:\n${tree.syntax}\n${treeStructure(tree, pretty)}")

  def termToType(term: Term): Type =
    term match {
      case Term.Name(name) => Type.Name(name)
      case Term.Select(qual, Term.Name(name)) => Type.Select(qual.asInstanceOf[Term.Ref], Type.Name(name))
      case _ => abort(s"Unable to convert term `${term.syntax}` to type")
    }

  def maybeTpeParams[T](tparams: List[Type.Param])(empty: => T, nonEmpty: List[Type] => T): T =
    Some(tparams).filter(_.nonEmpty).fold(empty)(ts => nonEmpty(ts.map(t => Type.Name(t.name.value))))

  def getTypeNames(tpe: Type): Set[Type.Name] = {
    debug("tpe", tpe)
    // TODO
    Set()
  }

  def valOrDef(mods: List[Mod], name: Term.Name, tparams: List[Type.Param], params: List[List[Term.Param]], tpe: Type, body: Term): Defn =
    (tparams, params) match {
      case (Nil, Nil) => q"val ${Pat.Var(name)}: $tpe = $body"
      case _   => q"def $name[..${tparams}](...$params): $tpe = $body"
    }

  lazy val name = "deriving"

  case object Deriving extends CompanionGenerator(name) { self =>
    // Names
    private lazy val andxorName = Term.fresh("andxor")
    private lazy val isoName = Term.fresh("andxorIso")
    private lazy val andxorLabelledName = Term.fresh("andxorLabelled")
    private lazy val labelledIsoName = Term.fresh("andxorLabelledIso")

    case class Param(param: Term.Param, tpe: Type) {
      lazy val termName: Term.Name = Term.Name(param.name.value)
    }

    trait GenClassDef {
      val klass: Defn.Class

      lazy val name: Type.Name = klass.name
      lazy val tparams: List[Type.Param] = klass.tparams
      lazy val params: List[List[Param]] = klass.ctor.paramss.map(_.map(x => x.decltpe match {
        case Some(tpe) => Param(x, tpe)
        case None => abort(s"Failed to generate code for $name because parameter ${x.name} has no declared type")
      }))
      lazy val arity: Int = params.flatten.length

      def andxorName: Term.Name
      def isoName: Term.Name
      def tpes: List[Type]

      def mkValue(inst: Term, param: Param): Term
      def normalizeValue(v: Term): Term

      lazy val abstractParams: List[Param] = {
        val abstractTpeNames = klass.tparams.map(_.name).toSet
        params.flatten.filter(param => getTypeNames(param.tpe).exists(abstractTpeNames.contains(_)))
      }

      lazy val classTpe: Type = maybeTpeParams(tparams)(t"$name", ts => t"$name[..$ts]")

      lazy val andxorTpes: List[Type] = id :: tpes

      private lazy val andxorNName = s"AndXorK$arity"
      lazy val andxorObj: Term = q"$andxorPkg.${Term.Name(andxorNName)}"
      lazy val andxorTpe: Type = t"$andxorPkg.${Type.Name(andxorNName)}[..${andxorTpes}]"

      lazy val prodObj: Term = q"$andxorName.Prod"
      lazy val prodTpe: Type = t"${Type.Singleton(andxorName)}#Prod"

      lazy val isoTpe: Type = t"$isoSetTpe[$classTpe, $prodTpe]"
    }

    case class BaseClassDef(klass: Defn.Class) extends GenClassDef {
      val andxorName = self.andxorName
      val isoName = self.isoName
      def tpes: List[Type] = params.flatten.map(_.tpe)
      def mkValue(inst: Term, param: Param): Term = q"$inst.${param.termName}"
      def normalizeValue(v: Term): Term = v
    }

    case class LabelledClassDef(klass: Defn.Class) extends GenClassDef {
      val andxorName = andxorLabelledName
      val isoName = labelledIsoName
      def tpes: List[Type] = params.flatten.map(p => t"""_root_.andxor.Labelled.Aux[${p.tpe},
        ${termToType(q"_root_.shapeless.Witness.${Term.Name(s""""${p.param.name.value}"""")}.T")}]""")
      def mkValue(inst: Term, param: Param): Term =
        q"$labelled[${param.tpe}]($inst.${param.termName}, ${Lit.String(param.param.name.value)})"
      def normalizeValue(v: Term): Term = q"$v.value"
    }

    def memberName(t: Tree): Term.Name =
      Term.fresh(s"andxor_${t.toString.toLowerCase.replace(".", "_")}")

    def mkAndxor(klass: GenClassDef): Term = q"${klass.andxorObj}[..${klass.andxorTpes}]"

    def andxor(klass: GenClassDef): Defn =
      valOrDef(Nil, klass.andxorName, klass.tparams, Nil, klass.andxorTpe, mkAndxor(klass))

    def mkTupleFromClass(klass: GenClassDef): Term =
      klass.params.flatten match {
        case Nil      => abort("TODO - support 0 parameter case classes")
        case p :: Nil => klass.mkValue(q"x", p)
        case ps       => Term.Tuple(ps.map(klass.mkValue(q"x", _)))
      }
      // List(params.dropRight(1).foldRight(params.last)((p, acc) => Apply(tuple2, List(p, acc))))

    def tupleAccess(idx: Int): Term.Name = Term.Name(s"t$idx")

    def constructorArgs(klass: GenClassDef): List[List[Term]] =
      klass.params.zipWithIndex.map { case (group, i) =>
        group.zipWithIndex.map { case (_, j) => klass.normalizeValue(q"x.${tupleAccess(i + j + 1)}") } }

    def mkIso(klass: GenClassDef): Term =
      q"""
      $isoSetObj[${klass.classTpe}, ${klass.prodTpe}](
        (x: ${klass.classTpe}) => ${klass.prodObj}(${mkTupleFromClass(klass)}),
        (x: ${klass.prodTpe}) => new ${klass.classTpe}(...${constructorArgs(klass)}))
      """

    def iso(klass: GenClassDef): Defn =
      valOrDef(Nil, klass.isoName, klass.tparams, Nil, klass.isoTpe, mkIso(klass))

    case class Typeclass(
      klass: GenClassDef,
      typeclass: Type,
      variance: Variance,
      memberName: Term.Name
    )

    def getTypeclasses0(tcs: List[Term], klass: GenClassDef, variance: Variance): List[Typeclass] =
      tcs.map(tc => Typeclass(klass, termToType(tc), variance, memberName(tc)))

    def getTypeclasses(args: List[List[Term]], _klass: Defn.Class): List[Typeclass] =
      List[(String, Variance, Boolean)](
        ("covariant", Covariant, false),
        ("labelledCovariant", Covariant, true),
        ("contravariant", Contravariant, false),
        ("labelledContravariant", Contravariant, true)
      ).flatMap { case (term, variance, labelled) =>
        val klass = if (labelled) LabelledClassDef(_klass) else BaseClassDef(_klass)
        args.flatten.flatMap(_ match {
          case Term.Assign(Term.Name(`term`), q"List(..$tcs)") => getTypeclasses0(tcs, klass, variance)
          case Term.Assign(Term.Name(`term`), q"Set(..$tcs)") => getTypeclasses0(tcs, klass, variance)
          case Term.Assign(Term.Name(`term`), q"Set(..$tcs)") => getTypeclasses0(tcs, klass, variance)
          case Term.Assign(Term.Name(`term`), q"Vector(..$tcs)") => getTypeclasses0(tcs, klass, variance)
          case _ => Nil
        })
      }

    def mkDerivedTypeclass(tc: Typeclass): Term =
      q"""
      $scalaPkg.Predef.implicitly[${tc.variance.typeclass}[${tc.typeclass}]]
        .${tc.variance.mapFunction}(
          ${maybeTpeParams(tc.klass.tparams)(tc.klass.andxorName, ts => q"${tc.klass.andxorName}[..$ts]")}
            .combineId[${tc.typeclass}].${tc.variance.derivationFunction}
        )(${tc.klass.isoName}.${tc.variance.isoFunction})
      """

    def derivedTypeclass(tc: Typeclass): Defn =
      // TODO - add implicit params
      valOrDef(List(Mod.Implicit()), tc.memberName, tc.klass.tparams, Nil, t"${tc.typeclass}[${tc.klass.classTpe}]", mkDerivedTypeclass(tc))

    override def extendCompanion(klass: Defn.Class): List[Stat] = {
      val annots = klass.mods.flatMap(_ match {
        case Mod.Annot(Init(Type.Name(`name`), _, args)) => getTypeclasses(args, klass)
        case _ => Nil
      })
      List(
        andxor(BaseClassDef(klass)),
        andxor(LabelledClassDef(klass)),
        iso(BaseClassDef(klass)),
        iso(LabelledClassDef(klass)),
      ) ++ annots.map(derivedTypeclass)
    }
  }
}
