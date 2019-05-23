// package andxor

// import java.nio.charset.StandardCharsets
// import scala.meta._
// import scala.meta.contrib._
// import scala.meta.contrib.equality.Structurally
// import scala.meta.gen._
// import scala.meta.prettyprinters._

// object generators {
//   // Andxor types
//   private lazy val andxorPkg = q"_root_.andxor"
//   private lazy val andxorTpesPkg = q"_root_.andxor.types"
//   private lazy val labelledObj = q"$andxorPkg.Labelled"

//   // Scala types
//   private lazy val scalaPkg = q"_root_.scala"

//   // Scalaz types
//   private lazy val scalazPkg = q"_root_.scalaz"
//   private lazy val id = t"$scalazPkg.Id.Id"
//   private lazy val isoSetObj = q"$scalazPkg.Isomorphism.IsoSet"
//   private lazy val isoSetTpe = t"$scalazPkg.Isomorphism.IsoSet"
//   private lazy val tagObj = q"$scalazPkg.Tag"
//   private lazy val tagTpe = t"_root_.scalaz.@@"
//   private lazy val adtValTagTpe = t"$andxorPkg.tags.ADTValue"
//   private lazy val adtValTagObj = q"$tagObj.of[$adtValTagTpe]"
//   private def mkAdtVal(inst: Term): Term = q"$adtValTagObj($inst)"
//   private def adtValTpe(tpe: Type): Type = t"$tagTpe[$tpe, $adtValTagTpe]"

//   lazy val all: Set[Generator] = Set(Deriving)

//   sealed trait Variance {
//     val typeclass: Type
//     val derivationFunction: Term.Name
//     val mapFunction: Term.Name
//     val isoFunction: Term.Name
//   }
//   sealed trait Covariant extends Variance {
//     val derivationFunction = q"apply"
//     val mapFunction = q"map"
//     val isoFunction = q"from"
//   }
//   case object CovariantProduct extends Covariant {
//     val typeclass = t"_root_.scalaz.Apply"
//   }
//   case object CovariantCoproduct extends Covariant {
//     val typeclass = t"_root_.andxor.Alt"
//   }
//   sealed trait Contravariant extends Variance {
//     val derivationFunction = q"divide"
//     val mapFunction = q"contramap"
//     val isoFunction = q"to"
//   }
//   case object ContravariantProduct extends Contravariant {
//     val typeclass = t"_root_.andxor.Divide"
//   }
//   case object ContravariantCoproduct extends Contravariant {
//     val typeclass = t"_root_.andxor.Decidable"
//   }

//   private case class PrettyPrinter(level: Int, inQuotes: Boolean, backslashed: Boolean) {
//     val indent = List.fill(level)("  ").mkString

//     def transform(char: Char): (PrettyPrinter, String) = {
//       val (pp, f): (PrettyPrinter, PrettyPrinter => String) = char match {
//         case '"' if inQuotes && !backslashed => (copy(inQuotes = false), _ => s"$char")
//         case '"' if !inQuotes => (copy(inQuotes = true), _ => s"$char")
//         case '\\' if inQuotes && !backslashed => (copy(backslashed = true), _ => s"$char")

//         case ',' if !inQuotes => (this, p => s",\n${p.indent}")
//         case '(' if !inQuotes => (copy(level = level + 1), p => s"(\n${p.indent}")
//         case ')' if !inQuotes => (copy(level = level - 1), p => s"\n${p.indent})")
//         case _ => (this, _ => s"$char")
//       }
//       (pp, f(pp))
//     }
//   }

//   private def prettyPrint(raw: String): String =
//     raw.foldLeft((PrettyPrinter(0, false, false), new StringBuilder(""))) { case ((pp, sb), char) =>
//       val (newPP, res) = pp.transform(char)
//       (newPP, sb.append(res))
//     }._2.toString.replaceAll("""\(\s+\)""", "()")

//   private def treeStructure(tree: Tree, pretty: Boolean): String =
//     if (pretty) prettyPrint(tree.structure) else tree.structure

//   def debug(name: String, tree: Tree, pretty: Boolean = true): Unit =
//     println(s"====\n$name ${tree.pos}:\n${tree.syntax}\n${treeStructure(tree, pretty)}")

//   def termToType(term: Term): Type =
//     term match {
//       case Term.Name(name) => Type.Name(name)
//       case Term.Select(qual, Term.Name(name)) => Type.Select(qual.asInstanceOf[Term.Ref], Type.Name(name))
//       case _ => abort(s"Unable to convert term `${term.syntax}` to type")
//     }

//   def maybeTpeParams[T](tparams: List[Type.Param])(empty: => T, nonEmpty: List[Type] => T): T =
//     Some(tparams).filter(_.nonEmpty).fold(empty)(ts => nonEmpty(ts.map(t => Type.Name(t.name.value))))

//   def getTypeNames(tpe: Tree): Set[Type.Name] =
//     (tpe match {
//       case t @ Type.Name(_) => Set(t)
//       case _ => Set()
//     }) ++ tpe.children.flatMap(getTypeNames(_))

//   def valOrDef(mods: List[Mod], name: Term.Name, tparams: List[Type.Param], params: List[List[Term.Param]], tpe: Type, body: Term): Defn =
//     (tparams, params) match {
//       case (Nil, Nil) => q"..$mods val ${Pat.Var(name)}: $tpe = $body"
//       case _   =>        q"..$mods def $name[..${tparams}](...$params): $tpe = $body"
//     }

//   lazy val name = "deriving"

//   private lazy val andxorName = Term.fresh("andxor")
//   private lazy val isoName = Term.fresh("andxorIso")
//   private lazy val andxorLabelledName = Term.fresh("andxorLabelled")
//   private lazy val labelledIsoName = Term.fresh("andxorLabelledIso")

//   case class Label(paramName: Name) {
//     // Don't use `Term.fresh` because label name needs to be deterministic
//     lazy val valName = Term.Name(s"andxor_label_${paramName.value}")
//     private lazy val implValName = Term.Name(s"${valName}_impl")
//     lazy val defns: List[Defn.Val] = List(
//       q"val ${Pat.Var(valName)}: String = ${Lit.String(paramName.value)}",
//       q"implicit val ${Pat.Var(implValName)}: $singletonTpe = $valName"
//     )
//     lazy val singletonTpe: Type.Singleton = Type.Singleton(valName)
//   }

//   case class Param(name: Name, tpe: Type) {
//     lazy val label: Label = Label(name)
//     lazy val labelledTpe: Type = t"_root_.andxor.Labelled.Aux[$tpe, ${label.singletonTpe}]"
//     lazy val termName: Term.Name = Term.Name(name.value)
//   }

//   def mkParam(p: Term.Param): Param =
//     p.decltpe match {
//       case Some(tpe) => Param(p.name, tpe)
//       case None => abort(s"Failed to generate code for $name because parameter ${p.name} has no declared type")
//     }

//   def extendsTpe(tpeName: Type.Name, inits: List[Init]): Boolean =
//     inits.exists(_.tpe match {
//       case Type.Name(name) if name == tpeName.value => true
//       case _ => false
//     })

//   implicit class ClassOps(klass: Defn.Class) {
//     def tpe: Type = ProdTree(klass, false).tpe
//   }

//   implicit class ObjectOps(obj: Defn.Object) {
//     def tpe: Type = Type.Singleton(obj.name)
//   }

//   implicit class ChildOps(child: Either[Defn.Class, Defn.Object]) {
//     def name: Name = child.fold(_.name, _.name)
//     def tpe: Type = child.fold(_.tpe, _.tpe)
//   }

//   def childOfTpe(tpeName: Type.Name, tree: Tree): Option[Either[Defn.Class, Defn.Object]] =
//     tree match {
//       case o @ Defn.Object(_, _, Template(_, inits, _, _)) if extendsTpe(tpeName, inits) => Some(Right(o))
//       case c @ Defn.Class(_, _, _, _, Template(_, inits, _, _)) if extendsTpe(tpeName, inits) => Some(Left(c))
//       case _ => None
//     }

//   def getChildrenOfTpe(tpeName: Type.Name, companion: Option[Defn.Object], owner: Option[Tree]): List[Either[Defn.Class, Defn.Object]] =
//     (companion.map(_.extract[Stat]).getOrElse(Nil) ++ owner.map(_ match {
//       case x: Defn.Object => x.extract[Stat]
//       case x: Pkg => x.extract[Stat]
//       case x: Source => x.extract[Stat]
//       case x: Template => x.extract[Stat]
//       case x: Term.Block => x.extract[Stat]
//       case _ => Nil
//     }).getOrElse(Nil)).flatMap(childOfTpe(tpeName, _))

//   def childrenToParams(children: List[Either[Defn.Class, Defn.Object]]): List[Param] =
//     children.map(c => Param(c.name, adtValTpe(c.tpe)))

//   sealed abstract class GenTree(
//     val params: List[List[Param]],
//     val name: Type.Name,
//     val tparams: List[Type.Param],
//     val copOrProd: String,
//   ) {
//     val labelled: Boolean

//     lazy val tpe: Type = maybeTpeParams(tparams)(t"$name", ts => t"$name[..$ts]")
//     lazy val tpes: List[Type] =
//       if (labelled) params.flatten.map(_.labelledTpe)
//       else params.flatten.map(_.tpe)

//     lazy val andxorTpes: List[Type] = id :: tpes

//     private lazy val andxorNName = s"AndXorK$arity"
//     lazy val andxorObj: Term = q"$andxorPkg.${Term.Name(andxorNName)}"
//     lazy val andxorTpe: Type = t"$andxorPkg.${Type.Name(andxorNName)}[..$andxorTpes]"

//     private lazy val reprName = s"${copOrProd}${arity}"
//     lazy val reprObj: Term = q"$andxorTpesPkg.${Term.Name(reprName)}"
//     lazy val reprTpe: Type = t"$andxorTpesPkg.${Type.Name(reprName)}[..$andxorTpes]"

//     def iso: Term

//     lazy val isoTpe: Type = t"$isoSetTpe[$tpe, $reprTpe]"

//     lazy val arity: Int = params.flatten.length

//     lazy val abstractParams: List[Param] = {
//       val abstractTpeNames = tparams.map(t => Structurally(t.name)).toSet
//       params.flatten.filter(param => getTypeNames(param.tpe).exists(abstractTpeNames.contains(_)))
//     }

//     lazy val andxorName: Term.Name = if (labelled) andxorLabelledName else generators.andxorName
//     lazy val isoName: Term.Name = if (labelled) labelledIsoName else generators.isoName

//     def mkValue(inst: Term, param: Param): Term

//     def normalizeValue(v: Term): Term = if (labelled) q"$v.value" else v
//   }

//   case class ProdTree(klass: Defn.Class, labelled: Boolean) extends GenTree(
//     klass.ctor.paramss.map(_.map(mkParam)),
//     klass.name,
//     klass.tparams,
//     "Prod"
//   ) {
//     private def tupleAccess(idx: Int): Term.Name = Term.Name(s"t$idx")

//     private lazy val mkTuple: Term =
//       params.flatten match {
//         case Nil      => abort("TODO - support 0 parameter case classes")
//         case p :: Nil => mkValue(q"x", p)
//         case ps       => Term.Tuple(ps.map(mkValue(q"x", _)))
//       }

//     private lazy val constructorArgs: List[List[Term]] =
//       params.zipWithIndex.map { case (group, i) =>
//         group.zipWithIndex.map { case (_, j) => normalizeValue(q"x.${tupleAccess(i + j + 1)}") } }

//     lazy val iso: Term =
//       q"""
//       $isoSetObj[$tpe, $reprTpe](
//         (x: $tpe) => $reprObj[..$andxorTpes]($mkTuple),
//         (x: $reprTpe) => new $tpe(...$constructorArgs))
//       """

//     def mkValue(inst: Term, param: Param): Term =
//       if (labelled) q"$labelledObj[${param.tpe}, ${param.label.singletonTpe}]($inst.${param.termName}, ${param.label.valName})"
//       else          q"$inst.${param.termName}"
//   }

//   case class CopTree(
//     children: List[Either[Defn.Class, Defn.Object]],
//     override val name: Type.Name,
//     override val tparams: List[Type.Param],
//     override val labelled: Boolean
//   ) extends GenTree(List(childrenToParams(children)), name, tparams, "Cop") {
//     val ps: List[Param] = params.flatten

//     def mkValue(inst: Term, param: Param): Term =
//       if (labelled) q"$labelledObj[${param.tpe}, ${param.label.singletonTpe}](${mkAdtVal(inst)}, ${param.label.valName})"
//       else          mkAdtVal(inst)

//     lazy val iso: Term =
//       q"""
//       $isoSetObj[$tpe, $reprTpe](
//         (x: $tpe) => x match {
//           ..case ${children.zip(ps).map(t => p"case inst: ${t._1.tpe} => $andxorName.inj(${mkValue(q"inst", t._2)})")}
//         },
//         (x: $reprTpe) => ${ps.tail.foldRight[Term](if (ps.length == 1 && labelled) q"$adtValTagObj.unwrap(x.run.value)" else q"x.run")(
//           (_, acc) => q"${if (labelled) q"$acc.bimap(x => $adtValTagObj.unwrap(x.value), x => $adtValTagObj.unwrap(x.value))"
//                           else q"$acc.bimap($adtValTagObj.unwrap(_), $adtValTagObj.unwrap(_))"}.merge[$id[$tpe]]")})
//       """
//   }

//   object CopTree {
//     def apply(name: Type.Name, companion: Option[Defn.Object], owner: Option[Tree], tparams: List[Type.Param], labelled: Boolean): CopTree =
//       new CopTree(getChildrenOfTpe(name, companion, owner), name, tparams, labelled)

//     def apply(c: Defn.Class, l: Boolean): GenTree =
//       apply(c.name, c.companionObject, c.owner, c.tparams, l)

//     def apply(t: Defn.Trait, l: Boolean): GenTree =
//       apply(t.name, t.companionObject, t.owner, t.tparams, l)
//   }

//   def memberName(t: Tree): Term.Name =
//     Term.fresh(s"andxor_${t.toString.toLowerCase.replace(".", "_")}")

//   def labels(tree: GenTree): List[Defn.Val] =
//     tree.params.flatten.flatMap(_.label.defns)

//   def mkAndxor(tree: GenTree): Term = q"${tree.andxorObj}[..${tree.andxorTpes}]"

//   def andxor(tree: GenTree): Defn =
//     valOrDef(Nil, tree.andxorName, tree.tparams, Nil, tree.andxorTpe, mkAndxor(tree))

//   def iso(tree: GenTree): Defn =
//     valOrDef(Nil, tree.isoName, tree.tparams, Nil, tree.isoTpe, tree.iso)

//   case class Typeclass(
//     tree: GenTree,
//     typeclass: Type,
//     variance: Variance,
//     memberName: Term.Name
//   )

//   def getTypeclasses0(tcs: List[Term], tree: GenTree, variance: Variance): List[Typeclass] =
//     tcs.map(tc => Typeclass(tree, termToType(tc), variance, memberName(tc)))

//   def getTypeclasses(args: List[List[Term]], base: GenTree, labelled: GenTree): List[Typeclass] = {
//     val (co, contra): (Covariant, Contravariant) = base match {
//       case _: ProdTree => (CovariantProduct, ContravariantProduct)
//       case _: CopTree  => (CovariantCoproduct, ContravariantCoproduct)
//     }
//     List[(String, Variance, Boolean)](
//       ("covariant", co, false),
//       ("labelledCovariant", co, true),
//       ("contravariant", contra, false),
//       ("labelledContravariant", contra, true)
//     ).flatMap { case (term, variance, l) =>
//       val tree = if (l) labelled else base
//       args.flatten.flatMap(_ match {
//         case Term.Assign(Term.Name(`term`), q"List(..$tcs)") => getTypeclasses0(tcs, tree, variance)
//         case Term.Assign(Term.Name(`term`), q"Set(..$tcs)") => getTypeclasses0(tcs, tree, variance)
//         case Term.Assign(Term.Name(`term`), q"Set(..$tcs)") => getTypeclasses0(tcs, tree, variance)
//         case Term.Assign(Term.Name(`term`), q"Vector(..$tcs)") => getTypeclasses0(tcs, tree, variance)
//         case _ => Nil
//       })
//     }
//   }

//   def mkDerivedTypeclass(tc: Typeclass): Term =
//     q"""
//     $scalaPkg.Predef.implicitly[${tc.variance.typeclass}[${tc.typeclass}]]
//       .${tc.variance.mapFunction}(
//         ${maybeTpeParams(tc.tree.tparams)(tc.tree.andxorName, ts => q"${tc.tree.andxorName}[..$ts]")}
//           .combineId[${tc.typeclass}].${tc.variance.derivationFunction}
//       )(${tc.tree.isoName}.${tc.variance.isoFunction})
//     """

//   def derivedTypeclass(tc: Typeclass): Defn =
//     valOrDef(List(Mod.Implicit()), tc.memberName, tc.tree.tparams,
//       Some(tc.tree.abstractParams).filter(_.nonEmpty).fold(List[List[Term.Param]]())(
//         ps => List(ps.map(p => param"implicit ${Term.fresh("ev")}: ${tc.typeclass}[${p.tpe}]"))),
//       t"${tc.typeclass}[${tc.tree.tpe}]", mkDerivedTypeclass(tc))

//   def mkStats(base: GenTree, labelled: GenTree, mods: List[Mod]): List[Stat] =
//     labels(labelled) ::: List(
//       andxor(base),
//       andxor(labelled),
//       iso(base),
//       iso(labelled),
//     ) ++ mods.flatMap(_ match {
//       case Mod.Annot(Init(Type.Name(`name`), _, args)) => getTypeclasses(args, base, labelled)
//       case _ => Nil
//     }).map(derivedTypeclass)

//   case object Deriving extends CompanionGenerator(name) { self =>
//     override def extendCompanion(c: Defn.Class): List[Stat] =
//       if (c.mods.has[Mod.Sealed]) mkStats(CopTree(c, false), CopTree(c, true), c.mods)
//       else if (c.mods.has[Mod.Case]) mkStats(ProdTree(c, false), ProdTree(c, true), c.mods)
//       else Nil

//     override def extendCompanion(t: Defn.Trait): List[Stat] = {
//       if (t.mods.has[Mod.Sealed]) mkStats(CopTree(t, false), CopTree(t, true), t.mods) else Nil
//     }
//   }
// }
