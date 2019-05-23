package andxor

import scala.collection.breakOut
import scala.{meta => m}
import scala.meta.contrib._
import scala.meta.contrib.equality.{Equal, Structurally}
import scala.meta.quasiquotes._
import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.meta.internal.tokenizers.PlatformTokenizerCache
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.transform.TypingTransformers

abstract class AnnotationPlugin(override val global: Global) extends Plugin { self =>
  val g: global.type = global

  override lazy val description: String =
    s"Generates code for annotations $triggers"

  /** Annotations that trigger the plugin */
  def triggers: List[String]

  def updateClass(triggered: List[m.Mod.Annot], clazz: m.Defn.Class): m.Defn.Class
  def updateCompanion(
    triggered: List[m.Mod.Annot],
    clazz: m.Defn.Class,
    companion: m.Defn.Object
  ): m.Defn.Object
  def updateModule(triggered: List[m.Mod.Annot], module: m.Defn.Object): m.Defn.Object

  def regenModule(comp: m.Defn.Object, extras: List[m.Stat]): m.Defn.Object =
    comp.withStats(comp.extract[m.Stat] ::: extras)

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

  /** Use to create code that shortcuts in ENSIME and ScalaIDE */
  def isIde: Boolean      = global.isInstanceOf[tools.nsc.interactive.Global]
  def isScaladoc: Boolean = global.isInstanceOf[tools.nsc.doc.ScaladocGlobal]

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
      case _ => g.abort(s"no name for $ann")
    }

  def structurallyEqual(t1: m.Tree, t2: m.Tree): Boolean =
    implicitly[Equal[Structurally[m.Tree]]].isEqual(Structurally(t1), Structurally(t2))

  private def phase = new PluginComponent with TypingTransformers {
    override val phaseName: String = AnnotationPlugin.this.name
    override val global: AnnotationPlugin.this.global.type =
      AnnotationPlugin.this.global
    override final def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: g.CompilationUnit): Unit = {
        if (unit.isJava || unit.source.file.path.endsWith(".template.scala")) ()
        else {
          PlatformTokenizerCache.megaCache.clear
          val semanticdb = new SemanticdbOps { val global: self.g.type = self.g }
          import semanticdb.XtensionCompilationUnitSource
          val src = unit.toSource
          val transformed = transformer(src)
          if (!structurallyEqual(src, transformed))
            unit.body = g.newUnitParser(g.newCompilationUnit(transformed.syntax, unit.source.path)).smartParse()
        }
      }
    }

    override val runsRightAfter: Option[String] = Some("parser")
    override val runsAfter: List[String] = runsRightAfter.toList
    override val runsBefore: List[String] = List[String]("typer")

    private object transformer extends m.Transformer {
      override def apply(tree: m.Tree): m.Tree = autobots(super.apply(tree))
    }

    private def hasTrigger(t: m.Tree): Boolean = t.collectFirst {
      case c: m.Defn.Class if hasTrigger(c.mods)  => ()
      case o: m.Defn.Object if hasTrigger(o.mods) => ()
      case o: m.Pkg.Object if hasTrigger(o.mods) => ()
    }.nonEmpty

    private def hasTrigger(mods: List[m.Mod]): Boolean = getTriggers(mods).nonEmpty

    private def extractTrigger(c: m.Defn.Class): (m.Defn.Class, List[m.Mod.Annot]) = {
      val trigger = getTriggers(c.mods)
      val update = c.withMods(c.extract[m.Mod].filterNot(trigger.contains(_)))
      (update, trigger)
    }
    private def extractTrigger(o: m.Defn.Object): (m.Defn.Object, List[m.Mod.Annot]) = {
      val trigger = getTriggers(o.mods)
      val update = o.withMods(o.extract[m.Mod].filterNot(trigger.contains(_)))
      (update, trigger)
    }

    private def getTriggers(mods: List[m.Mod]): List[m.Mod.Annot] =
      mods.collect {
        case a: m.Mod.Annot if triggers.contains(annotationName(a)) => a
      }

    /** generates a zero-functionality companion for a class */
    private def genCompanion(clazz: m.Defn.Class): m.Defn.Object = {
      val mods = clazz.mods.collectFirst { case p: m.Mod.Private => p }
        .orElse(clazz.mods.collectFirst { case p: m.Mod.Protected => p })
        .toList

      val isCase = clazz.mods.collectFirst { case _: m.Mod.Case => () }.nonEmpty

      def toString_ =
        q"override def toString: _root_.java.lang.String = ${m.Lit.String(clazz.name.value)}"

      q"""..$mods object ${m.Term.Name(clazz.name.value)} extends _root_.scala.AnyRef {
        ..${if (isCase) List(toString_) else Nil}
      }"""
    }

    // responds to visiting all the parts of the tree and passes to decepticons
    // to do the rewrites
    def autobots(tree: m.Tree): m.Tree = {
      tree match {
        // yuck, this finds classes and modules defined inside other classes.
        // added for completeness.
        case t: m.Defn.Class if hasTrigger(t) => t.withStats(decepticons(t.extract[m.Stat]))
        case t: m.Defn.Object if hasTrigger(t) => t.withStats(decepticons(t.extract[m.Stat]))
        case t: m.Defn.Trait if hasTrigger(t) => t.withStats(decepticons(t.extract[m.Stat]))
        case t: m.Pkg if hasTrigger(t) => t.withStats(decepticons(t.extract[m.Stat]))
        case t => t
      }
    }

    // does not recurse, let the autobots handle that
    def decepticons(stats: List[m.Stat]): List[m.Stat] = {
      val classes: Map[String, m.Defn.Class] = stats.collect {
        case c: m.Defn.Class => c.name.value -> c
      }(breakOut)

      val modules: Map[String, m.Defn.Object] = stats.collect {
        case m: m.Defn.Object => m.name.value -> m
      }(breakOut)

      object ClassNoCompanion {
        def unapply(t: m.Tree): Option[m.Defn.Class] = t match {
          case c: m.Defn.Class if !modules.contains(c.name.value) =>
            Some(c)
          case _ => None
        }
      }

      object ClassHasCompanion {
        def unapply(t: m.Tree): Option[m.Defn.Class] = t match {
          case c: m.Defn.Class if modules.contains(c.name.value) =>
            Some(c)
          case _ => None
        }
      }

      object CompanionAndClass {
        def unapply(t: m.Tree): Option[(m.Defn.Object, m.Defn.Class)] = t match {
          case m: m.Defn.Object =>
            classes.get(m.name.value).map { c =>
              (m, c)
            }
          case _ => None
        }
      }

      stats.flatMap {
        case ClassNoCompanion(c) if hasTrigger(c.mods) =>
          val companion        = genCompanion(c)
          val (cleaned, ann)   = extractTrigger(c)
          val updatedCompanion = updateCompanion(ann, cleaned, companion)
          List[m.Stat](updateClass(ann, cleaned), updatedCompanion)

        case ClassHasCompanion(c) if hasTrigger(c.mods) =>
          val (cleaned, ann) = extractTrigger(c)
          List[m.Stat](updateClass(ann, cleaned))

        case CompanionAndClass(companion, c) if hasTrigger(c.mods) =>
          val (cleaned, ann) = extractTrigger(c)
          List[m.Stat](updateCompanion(ann, cleaned, companion))

        case o: m.Defn.Object if hasTrigger(o.mods) =>
          val (cleaned, ann) = extractTrigger(o)
          List[m.Stat](updateModule(ann, cleaned))

        case tr =>
          List[m.Stat](tr)
      }
    }
  }

  override lazy val components: List[PluginComponent] = List(phase)
}
