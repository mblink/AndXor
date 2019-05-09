package andxor

import scala.meta._
import scala.meta.prettyprinters._
import scala.meta.gen.{CompanionGenerator, Generator}

object generators {
  lazy val all: Set[Generator] = Set(Deriving)

  sealed trait Variance {
    val typeclass: Term.Select
    val derivationFunction: Term.Name
    val mapFunction: Term.Name
    val isoFunction: Term.Name
  }
  case object Covariant extends Variance {
    val typeclass = q"_root_.scalaz.Apply"
    val derivationFunction = q"apply"
    val mapFunction = q"map"
    val isoFunction = q"from"
  }
  case object Contravariant extends Variance {
    val typeclass = q"_root_.andxor.Divide"
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

  def getTypeNames(tpe: Type): Set[Type.Name] = {
    debug("tpe", tpe)
    // TODO
    Set()
  }

  lazy val name = "deriving"

  case object Deriving extends CompanionGenerator(name) { self =>
    // Andxor types
    private lazy val andxor = q"_root_.andxor"
    private lazy val andxorObj = q"$andxor.Labelled"
    private lazy val labelledAux = t"$andxor.Labelled.Aux"

    // Scalaz types
    private lazy val scalaz = q"_root_.scalaz"
    private lazy val id = t"$scalaz.Id.Id"
    private lazy val isoSetObj = q"$scalaz.Isomorphism.IsoSet"
    private lazy val isoSetTpe = t"$scalaz.Isomorphism.IsoSet"

    // Names
    private lazy val andxorName = Term.fresh("andxor")
    private lazy val isoName = Term.fresh("andxorIso")
    private lazy val andxorLabelledName = Term.fresh("andxorLabelled")
    private lazy val labelledIsoName = Term.fresh("andxorLabelledIso")

    case class Param(param: Term.Param, tpe: Type)

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

      lazy val classTpe: Type = tparams match {
        case Nil => t"$name"
        case ts  => t"$name[..$ts]"
      }

      def andxorTpes(F: Option[Type]): List[Type] = F.toList ++ tpes

      private lazy val andxorNName = s"AndXor$arity"
      lazy val andxorObj: Term = q"$andxor.${Term.Name(andxorNName)}"
      lazy val andxorTpe: Type = t"$andxor.${Type.Name(andxorNName)}[..${andxorTpes(None)}]"

      private lazy val prodNName = s"Prod$arity"
      lazy val prodObj: Term = q"$andxor.types.${Term.Name(prodNName)}"
      lazy val prodTpe: Type = t"$andxor.types.${Type.Name(prodNName)}[..${andxorTpes(Some(id))}]"
    }

    case object BaseClassDef extends GenClassDef {
      val andxorName = self.andxorName
      val isoName = self.isoName
      def tpes: List[Type] = params.flatten.map(_.tpe)
      def mkValue(inst: Term, param: Param): Term = q"$inst.${param.param.name}"
      def normalizeValue(v: Term): Term = v
    }

    case object LabelledClassDef extends GenClassDef {
      val andxorName = andxorLabelledName
      val isoName = labelledIsoName
      def tpes: List[Type] = params.flatten.map(p => t"$labelledAux[${p.tpe}, ${Lit.String(p.param.name.value)}]")
      def mkValue(inst: Term, param: Param): Term =
        q"$andxorObj.apply[${param.tpe}]($inst.${param.param.name}, ${Lit.String(param.param.name.value)})"
      def normalizeValue(v: Term): Term = q"$v.value"
    }


    case class Typeclass(
      typeclass: Tree,
      variance: Variance,
      labelled: Boolean,
      memberName: Term.Name
    )

    override def extendCompanion(klass: Defn.Class): List[Stat] = {
      def memberName(t: Tree): Term.Name =
        Term.fresh(s"andxor_${t.toString.toLowerCase.replace(".", "_")}")

      def getTypeclasses0(tcs: List[Tree], variance: Variance, labelled: Boolean): List[Typeclass] =
        tcs.map(tc => Typeclass(tc, variance, labelled, memberName(tc)))

      def getTypeclasses(args: List[List[Term]]): List[Typeclass] =
        args.flatten.flatMap(_ match {
          case q"covariant = List(..$tcs)" => getTypeclasses0(tcs, Covariant, false)
          case q"covariant = Set(..$tcs)" => getTypeclasses0(tcs, Covariant, false)
          case q"covariant = Seq(..$tcs)" => getTypeclasses0(tcs, Covariant, false)
          case q"covariant = Vector(..$tcs)" => getTypeclasses0(tcs, Covariant, false)

          case q"labelledCovariant = List(..$tcs)" => getTypeclasses0(tcs, Covariant, true)
          case q"labelledCovariant = Set(..$tcs)" => getTypeclasses0(tcs, Covariant, true)
          case q"labelledCovariant = Seq(..$tcs)" => getTypeclasses0(tcs, Covariant, true)
          case q"labelledCovariant = Vector(..$tcs)" => getTypeclasses0(tcs, Covariant, true)

          case q"contravariant = List(..$tcs)" => getTypeclasses0(tcs, Contravariant, false)
          case q"contravariant = Set(..$tcs)" => getTypeclasses0(tcs, Contravariant, false)
          case q"contravariant = Seq(..$tcs)" => getTypeclasses0(tcs, Contravariant, false)
          case q"contravariant = Vector(..$tcs)" => getTypeclasses0(tcs, Contravariant, false)

          case q"labelledContravariant = List(..$tcs)" => getTypeclasses0(tcs, Contravariant, true)
          case q"labelledContravariant = Set(..$tcs)" => getTypeclasses0(tcs, Contravariant, true)
          case q"labelledContravariant = Seq(..$tcs)" => getTypeclasses0(tcs, Contravariant, true)
          case q"labelledContravariant = Vector(..$tcs)" => getTypeclasses0(tcs, Contravariant, true)

          case _ => Nil
        })

      def valOrDef(name: Term.Name, klass: GenClassDef, tpe: Type, body: Term): Defn =
        (klass.tparams, klass.params.flatten) match {
          case (Nil, Nil) => q"val $name: $tpe = $body"
          case _          => q"def $name[..${klass.tparams}](...${klass.params.map(_.map(_.param))}): $tpe = $body"
        }

      // def andxor(klass: GenClassDef): Defn =
      //   valOrDef(andxorName, klass, )
      // //   ValDef(
      //     Modifiers(Flag.SYNTHETIC),
      //     klass.andxorName,
      //     klass.andxorTpe,
      //     mkAndxor(klass)
      //   )

      // def andxorDef(klass: GenClassDef): DefDef =
      //   DefDef(
      //     Modifiers(Flag.SYNTHETIC),
      //     klass.andxorName,
      //     klass.tparams.map(_.duplicate),
      //     Nil,
      //     klass.andxorTpe,
      //     mkAndxor(klass)
      //   )

      val annots = klass.mods.flatMap(_ match {
        case Mod.Annot(Init(Type.Name(`name`), _, args)) => getTypeclasses(args)
        case _ => Nil
      })
      println(s"""HERE: $annots""")


      val hi: Lit.String = Lit.String("hi")
      val hiMethod: Defn.Def = q"def hi = println($hi)"

      hiMethod :: Nil
    }
  }
}
