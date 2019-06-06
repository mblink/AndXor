package andxor

import scala.annotation.tailrec
import scala.{meta => m}
import scala.meta.contrib._
import scala.meta.contrib.equality.Structurally
import scala.meta.quasiquotes._
import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.meta.internal.tokenizers.PlatformTokenizerCache
import scala.reflect.internal.{util => r}
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.transform.TypingTransformers

private[andxor] final class u extends deprecated("unused", "")

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
    self: m.Tree,
    classes: Map[String, m.Defn.Class],
    objects: Map[String, m.Defn.Object],
    traits: Map[String, m.Defn.Trait],
    types: Map[String, m.Defn.Type]
  )

  def error(pos: m.inputs.Position, msg: String): Unit = pos.input match {
    case m.Input.File(path, _) => g.globalError(r.Position.range(g.getSourceFile(path.toString), pos.start, pos.start, pos.end), msg)
    case _ => g.error(msg)
  }

  def update(
    @u anns: List[m.Mod.Annot],
    klass: m.Defn.Class,
    companion: Option[m.Defn.Object]
  ): Reader[LocalScope, (Option[(m.Defn.Class, Option[m.Defn.Object])], Vector[m.Stat])] =
    Reader(_ => (Some((klass, companion)), Vector()))

  def update(
    @u anns: List[m.Mod.Annot],
    obj: m.Defn.Object
  ): Reader[LocalScope, (Option[m.Defn.Object], Vector[m.Stat])] =
    Reader(_ => (Some(obj), Vector()))

  def update(
    @u anns: List[m.Mod.Annot],
    tr: m.Defn.Trait,
    companion: Option[m.Defn.Object]
  ): Reader[LocalScope, (Option[(m.Defn.Trait, Option[m.Defn.Object])], Vector[m.Stat])] =
    Reader(_ => (Some((tr, companion)), Vector()))

  def update(
    @u anns: List[m.Mod.Annot],
    tpe: m.Defn.Type,
    companion: Option[m.Defn.Object]
  ): Reader[LocalScope, (Option[(m.Defn.Type, Option[m.Defn.Object])], Vector[m.Stat])] =
    Reader(_ => (Some((tpe, companion)), Vector()))

  trait Named[A] { def name(a: A): m.Name }
  object Named {
    implicit val namedClass: Named[m.Defn.Class] = _.name
    implicit val namedObject: Named[m.Defn.Object] = _.name
    implicit val namedPkg: Named[m.Pkg] = _.name
    implicit val namedTrait: Named[m.Defn.Trait] = _.name
    implicit val namedType: Named[m.Defn.Type] = _.name
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
      case t: m.Defn.Type if hasTrigger(t.mods) => ()
      case t: m.Defn.Trait if hasTrigger(t.mods) => ()
      case o: m.Pkg.Object if hasTrigger(o.mods) => ()
    }.nonEmpty

    private def hasTrigger(mods: List[m.Mod]): Boolean = getTriggers(mods).nonEmpty

    private def extractTrigger[A <: m.Tree: Extract[?, m.Mod]: Replace[?, m.Mod]](tree: A): (List[m.Mod.Annot], A) = {
      val trigger = getTriggers(tree.extract[m.Mod])
      val update = tree.withMods(tree.extract[m.Mod].filterNot(trigger.contains(_)))
      (trigger, update)
    }

    private def getTriggers(mods: List[m.Mod]): List[m.Mod.Annot] =
      mods.collect {
        case a: m.Mod.Annot if triggers.contains(annotationName(a)) => a
      }

    private def updateTreeAndCompanion[A <: m.Stat: Extract[?, m.Mod]: Replace[?, m.Mod]: Named](
      tree: A,
      upd: (List[m.Mod.Annot], A, Option[m.Defn.Object]) => Reader[LocalScope, (Option[(A, Option[m.Defn.Object])], Vector[m.Stat])]
    ): Reader[LocalScope, Vector[m.Stat]] =
      for {
        companion <- Reader((_: LocalScope).objects.get(tree.name.value))
        (ann, cleaned) = extractTrigger(tree)
        updated <- upd(ann, cleaned, companion)
      } yield updated._1.map(_._1).toVector ++
              updated._1.flatMap(_._2).toVector ++
              updated._2

    // responds to visiting all the parts of the tree and passes to decepticons
    // to do the rewrites
    def autobots(tree: m.Tree): m.Tree =
      tree match {
        case t: m.Defn.Class if hasTrigger(t) => decepticons(t)
        case t: m.Defn.Object if hasTrigger(t) => decepticons(t)
        case t: m.Defn.Trait if hasTrigger(t) => decepticons(t)
        case t: m.Pkg if hasTrigger(t) => decepticons(t)
        case t => t
      }

    private def companionName[A <: m.Stat](tree: A): Option[m.Name] =
      tree match {
        case c: m.Defn.Class => Some(c.name)
        case t: m.Defn.Trait => Some(t.name)
        case t: m.Defn.Type => Some(t.name)
        case _ => None
      }

    private def withoutCompanion[A <: m.Stat](tree: A, stats: Vector[m.Stat]): Vector[m.Stat] =
      companionName(tree).fold(stats)(name => stats.filter(_ match {
        case o: m.Defn.Object if o.name.value == name.value => false
        case _ => true
      }))

    private def getLocals[T <: m.Tree: Extract[?, m.Stat], A <: m.Stat: Named](tree: T)(pf: PartialFunction[m.Tree, A]): Map[String, A] =
      tree.extract[m.Stat].flatMap(pf.lift(_).map(a => a.name.value -> a)).toMap

    // does not recurse, let the autobots handle that
    def decepticons[A <: m.Tree: Extract[?, m.Stat]: Replace[?, m.Stat]: Named](tree: A): A = {
      val scope = LocalScope(
        tree,
        getLocals(tree) { case c: m.Defn.Class => c },
        getLocals(tree) { case o: m.Defn.Object => o },
        getLocals(tree) { case t: m.Defn.Trait => t },
        getLocals(tree) { case t: m.Defn.Type => t })

      @tailrec
      def go(queue: Vector[m.Stat], out: Vector[m.Stat]): Vector[m.Stat] =
        queue match {
          case Vector() => out

          case (c: m.Defn.Class) +: tail if hasTrigger(c.mods) =>
            go(withoutCompanion(c, tail), out ++ updateTreeAndCompanion[m.Defn.Class](c, update).run(scope))

          case (o: m.Defn.Object) +: tail if hasTrigger(o.mods) =>
            val (ann, cleaned) = extractTrigger(o)
            val (upd, extra) = update(ann, cleaned).run(scope)
            go(tail, out ++ upd.toVector ++ extra)

          case (t: m.Defn.Trait) +: tail if hasTrigger(t.mods) =>
            go(withoutCompanion(t, tail), out ++ updateTreeAndCompanion[m.Defn.Trait](t, update).run(scope))

          case (t: m.Defn.Type) +: tail if hasTrigger(t.mods) =>
            go(withoutCompanion(t, tail), out ++ updateTreeAndCompanion[m.Defn.Type](t, update).run(scope))

          case t +: tail => go(tail, out :+ t)
        }

      tree.withStats(go(tree.extract[m.Stat].toVector, Vector()).toList)
    }
  }

  override lazy val components: List[PluginComponent] = List(phase)
}
