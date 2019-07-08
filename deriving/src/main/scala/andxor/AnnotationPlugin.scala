package andxor

import scala.annotation.tailrec
import scala.{meta => m}
import scala.meta.contrib._
import scala.meta.contrib.equality.Structurally
import scala.meta.parsers._
import scala.meta.quasiquotes._
import scala.reflect.internal.{util => r}
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.transform.TypingTransformers

private[andxor] final case class Reader[A, B](run: A => B) {
  def map[C](f: B => C): Reader[A, C] = Reader(run.andThen(f))
  def flatMap[C](f: B => Reader[A, C]): Reader[A, C] = Reader(a => f(run(a)).run(a))
}

abstract class AnnotationPlugin(override val global: Global) extends Plugin { self =>
  val g: global.type = global

  override lazy val description: String =
    s"Generates code for annotations $triggers"

  /** Annotations that trigger the plugin */
  def triggers: List[String]

  case class LocalScope(
    self: g.Tree,
    classes: Map[String, (g.ClassDef, m.Defn.Class)],
    objects: Map[String, (g.ModuleDef, m.Defn.Object)],
    traits: Map[String, (g.ClassDef, m.Defn.Trait)],
    types: Map[String, (g.TypeDef, m.Defn.Type)]
  )

  def error(pos: m.inputs.Position, msg: String): Unit = pos.input match {
    case m.Input.File(path, _) => g.globalError(r.Position.range(g.getSourceFile(path.toString), pos.start, pos.start, pos.end), msg)
    case _ => g.error(msg)
  }

  def update(
    @deprecated("unused", "") anns: List[m.Mod.Annot],
    klass: m.Defn.Class,
    companion: Option[m.Defn.Object]
  ): Reader[LocalScope, (Option[(m.Defn.Class, Option[m.Defn.Object])], Vector[m.Stat])] =
    Reader(_ => (Some((klass, companion)), Vector()))

  def update(
    @deprecated("unused", "") anns: List[m.Mod.Annot],
    obj: m.Defn.Object
  ): Reader[LocalScope, (Option[m.Defn.Object], Vector[m.Stat])] =
    Reader(_ => (Some(obj), Vector()))

  def update(
    @deprecated("unused", "") anns: List[m.Mod.Annot],
    tr: m.Defn.Trait,
    companion: Option[m.Defn.Object]
  ): Reader[LocalScope, (Option[(m.Defn.Trait, Option[m.Defn.Object])], Vector[m.Stat])] =
    Reader(_ => (Some((tr, companion)), Vector()))

  def update(
    @deprecated("unused", "") anns: List[m.Mod.Annot],
    tpe: m.Defn.Type,
    companion: Option[m.Defn.Object]
  ): Reader[LocalScope, (Option[(m.Defn.Type, Option[m.Defn.Object])], Vector[m.Stat])] =
    Reader(_ => (Some((tpe, companion)), Vector()))

  trait Named[A] { def name(a: A): m.Name }
  object Named {
    implicit val namedClass: Named[m.Defn.Class] = new Named[m.Defn.Class] { def name(a: m.Defn.Class): m.Name = a.name }
    implicit val namedObject: Named[m.Defn.Object] = new Named[m.Defn.Object] { def name(a: m.Defn.Object): m.Name = a.name }
    implicit val namedPkg: Named[m.Pkg] = new Named[m.Pkg] { def name(a: m.Pkg): m.Name = a.name }
    implicit val namedTrait: Named[m.Defn.Trait] = new Named[m.Defn.Trait] { def name(a: m.Defn.Trait): m.Name = a.name }
    implicit val namedType: Named[m.Defn.Type] = new Named[m.Defn.Type] { def name(a: m.Defn.Type): m.Name = a.name }
  }
  implicit class NamedOps[A](a: A)(implicit n: Named[A]) { def name: m.Name = n.name(a) }

  def genCompanion[A <: m.Tree: Extract[?, m.Mod]: Named](tree: A): m.Defn.Object = {
    val (name, mods) = (tree.name, tree.extract[m.Mod])
    val objMods = mods.collectFirst { case p: m.Mod.Private => p }
      .orElse(mods.collectFirst { case p: m.Mod.Protected => p })
      .toList

    val isCase = mods.collectFirst { case _: m.Mod.Case => () }.nonEmpty

    def toString_ =
      q"override def toString: _root_.java.lang.String = ${m.Lit.String(name.value)}"

    q"""..$objMods object ${m.Term.Name(name.value)} extends _root_.scala.AnyRef {
      ..${if (isCase) List(toString_) else Nil}
    }"""
  }

  def regenObject(obj: m.Defn.Object, extras: List[m.Stat]): m.Defn.Object =
    extras match {
      case Nil => obj
      case x   => obj.withStats(obj.extract[m.Stat] ::: x)
    }

  implicit class TreeOps(tree: m.Tree) {
    private def isOwner(t: m.Tree): Boolean =
      t match {
        case _: m.Source | _: m.Pkg | _: m.Defn => true
        case _ => false
      }

    def owner: Option[m.Tree] =
      tree.parent.flatMap {
        case x: m.Term.Block => x.owner
        case x: m.Template => x.owner
        case x if isOwner(x) => Some(x)
        case _ => None
      }
  }

  implicit val replacePkgObjectStats: Replace[m.Pkg, m.Stat] =
    Replace((a, bs) => a.copy(stats = bs))

  implicit val extractTermBlockStats: Extract[m.Term.Block, m.Stat] =
    Extract(_.stats)

  implicit val replaceTermBlockStats: Replace[m.Term.Block, m.Stat] =
    Replace((a, bs) => a.copy(stats = bs))

  private case class PrettyPrinter(level: Int, inQuotes: Boolean, backslashed: Boolean) {
    val indent = List.fill(level)("  ").mkString

    def transform(char: Char): (PrettyPrinter, String) = {
      val woSlash = copy(backslashed = false)
      val (pp, f): (PrettyPrinter, PrettyPrinter => String) = char match {
        case '"' if inQuotes && !backslashed => (woSlash.copy(inQuotes = false), _ => s"$char")
        case '"' if !inQuotes => (woSlash.copy(inQuotes = true), _ => s"$char")
        case '\\' if inQuotes && !backslashed => (copy(backslashed = true), _ => s"$char")

        case ',' if !inQuotes => (woSlash, p => s",\n${p.indent}")
        case '(' if !inQuotes => (woSlash.copy(level = level + 1), p => s"(\n${p.indent}")
        case ')' if !inQuotes => (woSlash.copy(level = level - 1), p => s"\n${p.indent})")
        case _ => (woSlash, _ => s"$char")
      }
      (pp, f(pp))
    }
  }

  private def prettyPrint(raw: String): String =
    raw.foldLeft((PrettyPrinter(0, false, false), new StringBuilder(""))) { case ((pp, sb), char) =>
      val (newPP, res) = pp.transform(char)
      (newPP, sb.append(res))
    }._2.toString.replaceAll("""\(\s+\)""", "()")

  private def showTree(tree: m.Tree, pretty: Boolean): String =
    if (pretty) prettyPrint(tree.structure) else tree.structure

  // best way to inspect a tree, just call this
  def debug(name: String, tree: m.Tree, pretty: Boolean = true): Unit =
    println(s"===\n$name ${tree.pos}:\n${tree.syntax}\n${showTree(tree, pretty)}")

  // recovers the final part of an annotation
  def annotationName(ann: m.Mod.Annot): String =
    ann.init.tpe match {
      case m.Type.Name(name) => name
      case m.Type.Select(_, m.Type.Name(name)) => name
      case _ =>
        error(ann.pos, s"no name for $ann")
        ""
    }

  def structurallyEqual(t1: m.Tree, t2: m.Tree): Boolean = t1.isEqual[Structurally](t2)

  private def phase = new PluginComponent with TypingTransformers {
    override val phaseName: String = AnnotationPlugin.this.name
    override val global: AnnotationPlugin.this.global.type =
      AnnotationPlugin.this.global
    override final def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: g.CompilationUnit): Unit = newTransformer(unit).transformUnit(unit)
    }

    private def newTransformer(unit: g.CompilationUnit) =
      new TypingTransformer(unit) {
        override def transform(tree: g.Tree): g.Tree =
          autobots(super.transform(tree))
    }

    override val runsRightAfter: Option[String] = Some("parser")
    override val runsAfter: List[String] = runsRightAfter.toList
    override val runsBefore: List[String] = List[String]("typer")

    val Triggers: List[g.TypeName] = triggers.map(g.newTypeName)
    private def hasTrigger(t: g.Tree): Boolean = t.exists {
      case c: g.ClassDef if hasTrigger(c.mods)  => true
      case m: g.ModuleDef if hasTrigger(m.mods) => true
      case _                                    => false
    }

    private def hasTrigger(mods: g.Modifiers): Boolean = Triggers.exists(mods.hasAnnotationNamed)

    private def extractTrigger[A <: m.Tree: Extract[?, m.Mod]: Replace[?, m.Mod]](tree: A): (List[m.Mod.Annot], A) = {
      val trigger = getTriggers(tree.extract[m.Mod])
      val update = tree.withMods(tree.extract[m.Mod].filterNot(trigger.contains(_)))
      (trigger, update)
    }

    private def getTriggers(mods: List[m.Mod]): List[m.Mod.Annot] =
      mods.collect {
        case a: m.Mod.Annot if triggers.contains(annotationName(a)) => a
      }

    private def gTreeToMTree(gTree: g.Tree): m.Tree = g.showCode(gTree).parse[m.Stat].get

    private def mTreeToGTree(mTree: m.Tree): g.Tree = {
      val objName = g.currentFreshNameCreator.newName("_annotation_plugin_")
      g.newUnitParser(s"object $objName { ${mTree.syntax} }", g.currentSource.path).smartParse().collect {
        case g.ModuleDef(_, g.TermName(`objName`), g.Template(_, _,
          g.DefDef(_, g.nme.CONSTRUCTOR, _, _, _, _) :: t :: Nil)) => t
      }.head
    }

    private case class Structural(t: g.Tree) {
      override def hashCode: Int = 0
      override def equals(other: Any): Boolean = other.isInstanceOf[g.Tree] && t.equalsStructure(other.asInstanceOf[g.Tree])
    }
    private var allPositions: Map[Structural, g.Position] = null
    private def mkPositions(trees: List[g.Tree]): Map[Structural, g.Position] =
      trees.foldLeft(Map[Structural, g.Position]())((acc, t) => acc ++ Map(Structural(t) -> t.pos) ++ mkPositions(t.children))

    private def updatePos(trees: Vector[g.Tree], fallback: Option[g.Position]): Reader[LocalScope, Vector[g.Tree]] =
      Reader { scope =>
        if (allPositions == null) allPositions = mkPositions(scope.self.children)
        val f = (_: Option[g.Position]).filter(_ != g.NoPosition)
        trees.foreach { tree =>
          val pos = f(allPositions.get(Structural(tree))).orElse(f(fallback))
          pos.foreach(p => tree.setPos(new r.TransparentPosition(p.source, p.start, p.end, p.end)))
          updatePos(tree.children.toVector, pos).run(scope)
        }
        trees
      }

    private def updateTreeAndCompanion[A <: m.Stat: Extract[?, m.Mod]: Replace[?, m.Mod]: Named](
      orig: g.Tree,
      updF: (List[m.Mod.Annot], A, Option[m.Defn.Object]) => Reader[LocalScope, (Option[(A, Option[m.Defn.Object])], Vector[m.Stat])]
    ): Reader[LocalScope, Vector[g.Tree]] =
      for {
        tree <- Reader((_: LocalScope) => gTreeToMTree(orig).asInstanceOf[A])
        xCompanion <- Reader((_: LocalScope).objects.get(tree.name.value))
        companion = xCompanion.map(_._2)
        t = extractTrigger(tree)
        updated <- updF(t._1, t._2, companion)
        upd = (ts: Vector[m.Tree]) => updatePos(ts.map(mTreeToGTree), Some(orig.pos))
        updA <- upd(updated._1.map(_._1).toVector)
        updCompanion <- upd(updated._1.flatMap(_._2).toVector)
        updExtra <- upd(updated._2)
      } yield updA ++ updCompanion ++ updExtra

    // responds to visiting all the parts of the tree and passes to decepticons
    // to do the rewrites
    def autobots(tree: g.Tree): g.Tree =
      tree match {
        case p: g.PackageDef if hasTrigger(p) =>
          g.treeCopy.PackageDef(p, p.pid, decepticons(p, p.stats))
        case m: g.ModuleDef if hasTrigger(m.impl) =>
          g.treeCopy.ModuleDef(m, m.mods, m.name,
            g.treeCopy.Template(m.impl, m.impl.parents, m.impl.self, decepticons(m, m.impl.body)))
        case c: g.ClassDef if hasTrigger(c.impl) =>
          g.treeCopy.ClassDef(c, c.mods, c.name, c.tparams,
            g.treeCopy.Template(c.impl, c.impl.parents, c.impl.self, decepticons(c, c.impl.body)))
        case t => t
      }

    private def withoutCompanion(companionName: g.Name, stats: Vector[g.Tree]): Vector[g.Tree] =
      stats.filter(_ match {
        case o: g.ModuleDef if o.name.decodedName.toString == companionName.decodedName.toString => false
        case _ => true
      })

    private def getLocals[A <: g.NameTree, B <: m.Stat](trees: List[g.Tree])(pf: PartialFunction[g.Tree, A]): Map[String, (A, B)] =
      trees.flatMap(pf.lift(_).map(a => a.name.decodedName.toString -> ((a, gTreeToMTree(a).asInstanceOf[B])))).toMap

    private def isTrait(c: g.ClassDef): Boolean = c.mods.hasFlag(g.Flag.TRAIT)

    def decepticons(scopeSelf: g.Tree, trees: List[g.Tree]): List[g.Tree] = {
      lazy val scope = LocalScope(
        scopeSelf,
        getLocals[g.ClassDef, m.Defn.Class](trees) { case c: g.ClassDef if !isTrait(c) => c },
        getLocals[g.ModuleDef, m.Defn.Object](trees) { case o: g.ModuleDef => o },
        getLocals[g.ClassDef, m.Defn.Trait](trees) { case t: g.ClassDef if isTrait(t) => t },
        getLocals[g.TypeDef, m.Defn.Type](trees) { case t: g.TypeDef => t })

      @tailrec
      def go(queue: Vector[g.Tree], out: Vector[g.Tree]): Vector[g.Tree] =
        queue match {
          case Vector() => out

          case (c: g.ClassDef) +: tail if hasTrigger(c.mods) =>
            go(withoutCompanion(c.name.companionName, tail), out ++
              (if (isTrait(c)) updateTreeAndCompanion[m.Defn.Trait](c, update)
               else updateTreeAndCompanion[m.Defn.Class](c, update)).run(scope))

          case (o: g.ModuleDef) +: tail if hasTrigger(o.mods) =>
            val (ann, cleaned) = extractTrigger(gTreeToMTree(o).asInstanceOf[m.Defn.Object])
            val (upd, extra) = update(ann, cleaned).run(scope)
            go(tail, out ++ updatePos((upd.toVector ++ extra).map(mTreeToGTree), None).run(scope))

          case (t: g.TypeDef) +: tail if hasTrigger(t.mods) =>
            go(withoutCompanion(t.name.companionName, tail), out ++ updateTreeAndCompanion[m.Defn.Type](t, update).run(scope))

          case t +: tail => go(tail, out :+ t)
        }

      go(trees.toVector, Vector()).toList
    }
  }

  override lazy val components: List[PluginComponent] = List(phase)
}
