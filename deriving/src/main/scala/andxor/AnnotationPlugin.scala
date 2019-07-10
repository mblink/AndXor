package andxor

import scala.annotation.tailrec
import scala.reflect.internal.util.TransparentPosition
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.transform.TypingTransformers

private[andxor] final case class Reader[A, B](run: A => B) {
  def map[C](f: B => C): Reader[A, C] = Reader(run.andThen(f))
  def flatMap[C](f: B => Reader[A, C]): Reader[A, C] = Reader(a => f(run(a)).run(a))
}

abstract class AnnotationPlugin(override val global: Global) extends Plugin { self =>
  val g: global.type = global

  import global._

  override lazy val description: String =
    s"Generates code for annotations $triggers"

  /** Annotations that trigger the plugin */
  def triggers: List[String]

  def isImplicitVal(v: ValDef): Boolean = v.mods.isImplicit
  def isImplicitVal(vs: List[ValDef]): Boolean = vs.forall(isImplicitVal(_))

  def ctorParams(klass: ClassDef): (List[List[ValDef]], Option[List[ValDef]]) =
    klass.impl.body.collect { case d@DefDef(_, termNames.CONSTRUCTOR, _, _, _, impl) => d } match {
      case DefDef(_, _, _, vh :: vt, _, _) :: Nil if !isImplicitVal(vh) =>
        vt.span(!isImplicitVal(_)) match {
          case (e, i :: Nil) => (vh :: e, Some(i))
          case (e, Nil) => (vh :: e, None)
          case _ => abort(s"Found more than one implicit parameter group for ${klass.name}")
        }
      case _ => abort(s"Failed to find exactly one constructor for ${klass.name}")
    }

  def freshName(prefix: String): String = currentFreshNameCreator.newName(prefix)

  trait Get[A, T] { def get(a: A): T }
  object Get {
    def apply[A, T](f: A => T): Get[A, T] = new Get[A, T] { def get(a: A): T = f(a) }

    implicit lazy val getClassMods: Get[ClassDef, Modifiers] = Get(_.mods)
    implicit lazy val getObjMods: Get[ModuleDef, Modifiers] = Get(_.mods)
    implicit lazy val getTypeMods: Get[TypeDef, Modifiers] = Get(_.mods)

    implicit lazy val getClassStats: Get[ClassDef, List[Tree]] = Get(_.impl.body.flatMap(_ match {
      case v: ValDef => if (v.mods.isCaseAccessor || v.mods.isParamAccessor) Nil else List(v)
      case d: DefDef => if (d.name == termNames.CONSTRUCTOR) Nil else List(d)
    }))
    implicit lazy val getObjStats: Get[ModuleDef, List[Tree]] = Get(_.impl.body)
  }
  implicit class GetOps[A](a: A) {
    def get[T](implicit g: Get[A, T]): T = g.get(a)
  }

  trait Set0[A, T] { def set(a: A, t: T): A }
  object Set0 {
    def apply[A, T](f: (A, T) => A): Set0[A, T] = new Set0[A, T] { def set(a: A, t: T): A = f(a, t) }

    implicit lazy val setClassMods: Set0[g.ClassDef, g.Modifiers] = Set0((x, m) => treeCopy.ClassDef(x, m, x.name, x.tparams, x.impl))
    implicit lazy val setObjMods: Set0[g.ModuleDef, g.Modifiers] = Set0((x, m) => treeCopy.ModuleDef(x, m, x.name, x.impl))
    implicit lazy val setTypeMods: Set0[g.TypeDef, g.Modifiers] = Set0((x, m) => treeCopy.TypeDef(x, m, x.name, x.tparams, x.rhs))
  }
  implicit class Set0Ops[A](a: A) {
    def set[T](t: T)(implicit s: Set0[A, T]): A = s.set(a, t)
  }

  case class LocalScope(
    self: g.Tree,
    classes: Map[String, g.ClassDef],
    objects: Map[String, g.ModuleDef],
    traits: Map[String, g.ClassDef],
    types: Map[String, g.TypeDef]
  )

  def error(pos: g.Position, msg: String): Unit = g.globalError(pos, msg)

  def updateClass(
    @deprecated("unused", "") anns: List[g.Tree],
    klass: g.ClassDef,
    companion: Option[g.ModuleDef]
  ): Reader[LocalScope, (Option[(g.ClassDef, Option[g.ModuleDef])], Vector[g.Tree])] =
    Reader(_ => (Some((klass, companion)), Vector()))

  def updateObject(
    @deprecated("unused", "") anns: List[g.Tree],
    obj: g.ModuleDef
  ): Reader[LocalScope, (Option[g.ModuleDef], Vector[g.Tree])] =
    Reader(_ => (Some(obj), Vector()))

  def updateTrait(
    @deprecated("unused", "") anns: List[g.Tree],
    tr: g.ClassDef,
    companion: Option[g.ModuleDef]
  ): Reader[LocalScope, (Option[(g.ClassDef, Option[g.ModuleDef])], Vector[g.Tree])] =
    Reader(_ => (Some((tr, companion)), Vector()))

  def updateType(
    @deprecated("unused", "") anns: List[g.Tree],
    tpe: g.TypeDef,
    companion: Option[g.ModuleDef]
  ): Reader[LocalScope, (Option[(g.TypeDef, Option[g.ModuleDef])], Vector[g.Tree])] =
    Reader(_ => (Some((tpe, companion)), Vector()))

  def genCompanion[A <: g.NameTree: Get[?, g.Modifiers]](tree: A): g.ModuleDef = {
    val (name, mods) = (tree.name, tree.get[g.Modifiers])
    val objMods = Option(Modifiers(g.Flag.PRIVATE)).filter(_ => mods.isPrivate)
      .orElse(Option(Modifiers(g.Flag.PROTECTED)).filter(_ => mods.isProtected))
      .getOrElse(Modifiers())

    def toString_ =
      q"override def toString: _root_.java.lang.String = ${g.Literal(g.Constant(tree.name.companionName.decode))}"

    q"""$objMods object ${g.TermName(name.decode)} extends _root_.scala.AnyRef {
      ..${if (mods.isCase) List(toString_) else Nil}
    }"""
  }

  def regenObject(obj: g.ModuleDef, extras: List[g.Tree]): g.ModuleDef =
    extras match {
      case Nil => obj
      case x => treeCopy.ModuleDef(obj, obj.mods, obj.name,
        treeCopy.Template(obj.impl, obj.impl.parents, obj.impl.self, obj.impl.body ::: x))
    }

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

  private def showTree(tree: g.Tree, pretty: Boolean): String =
    if (pretty) prettyPrint(g.showRaw(tree)) else g.showRaw(tree)

  def debug(name: String, tree: g.Tree, pretty: Boolean = true): Unit =
    println(s"===\n$name ${tree.pos}:\n${g.showCode(tree)}\n${showTree(tree, pretty)}")

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

    override val runsAfter: List[String] = List("parser")
    override val runsBefore: List[String] = List("namer")

    val Triggers: List[g.TypeName] = triggers.map(g.newTypeName)
    private def hasTrigger(t: g.Tree): Boolean = t.exists {
      case c: g.ClassDef if hasTrigger(c.mods)  => true
      case m: g.ModuleDef if hasTrigger(m.mods) => true
      case _                                    => false
    }

    private def hasTrigger(mods: g.Modifiers): Boolean = Triggers.exists(mods.hasAnnotationNamed)

    private def extractTrigger[A <: g.Tree: Get[?, g.Modifiers]: Set0[?, g.Modifiers]](tree: A): (List[g.Tree], A) = {
      val mods = tree.get[g.Modifiers]
      val (triggers, rest) = getTriggers(mods.annotations)
      val update = tree.set(mods.mapAnnotations(_ => rest))
      (triggers, update)
    }

    private def getTriggers(anns: List[g.Tree]): (List[g.Tree], List[g.Tree]) =
      anns.partition(a => Triggers.exists(isNamed(a, _)))

    private def isNamed(t: g.Tree, name: g.TypeName) = t match {
      case Apply(Select(New(Ident(`name`)), _), _)     => true
      case Apply(Select(New(Select(_, `name`)), _), _) => true
      case _                                           => false
    }

    private def withAllPos(tree: Tree, pos: Position): Tree = {
      tree.foreach { t =>
        if (!t.pos.isDefined || t.pos == NoPosition)
          t.setPos(new TransparentPosition(pos.source, pos.start, pos.end, pos.end))
        ()
      }
      tree
    }

    private def updateTreeAndCompanion[A <: g.NameTree: Get[?, g.Modifiers]: Set0[?, g.Modifiers]](
      tree: A,
      updF: (List[g.Tree], A, Option[g.ModuleDef]) => Reader[LocalScope, (Option[(A, Option[g.ModuleDef])], Vector[g.Tree])]
    ): Reader[LocalScope, Vector[g.Tree]] =
      for {
        companion <- Reader((_: LocalScope).objects.get(tree.name.decode))
        t = extractTrigger(tree)
        updated <- updF(t._1, t._2, companion)
        upd = (ts: Vector[Tree], p: Position) => ts.map(withAllPos(_, p))
        updA = upd(updated._1.map(_._1).toVector, tree.pos)
        updCompanion = upd(updated._1.flatMap(_._2).toVector, companion.getOrElse(tree).pos)
        updExtra = upd(updated._2, tree.pos)
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

    private def getLocals[A <: g.NameTree](trees: List[g.Tree])(pf: PartialFunction[g.Tree, A]): Map[String, A] =
      trees.flatMap(pf.lift(_).map(a => a.name.decodedName.toString -> a)).toMap

    private def isTrait(c: g.ClassDef): Boolean = c.mods.isTrait

    def decepticons(scopeSelf: g.Tree, trees: List[g.Tree]): List[g.Tree] = {
      lazy val scope = LocalScope(
        scopeSelf,
        getLocals[g.ClassDef](trees) { case c: g.ClassDef if !isTrait(c) => c },
        getLocals[g.ModuleDef](trees) { case o: g.ModuleDef => o },
        getLocals[g.ClassDef](trees) { case t: g.ClassDef if isTrait(t) => t },
        getLocals[g.TypeDef](trees) { case t: g.TypeDef => t })

      @tailrec
      def go(queue: Vector[g.Tree], out: Vector[g.Tree]): Vector[g.Tree] =
        queue match {
          case Vector() => out

          case (c: g.ClassDef) +: tail if hasTrigger(c.mods) =>
            go(withoutCompanion(c.name.companionName, tail), out ++
              (if (isTrait(c)) updateTreeAndCompanion(c, updateTrait)
               else updateTreeAndCompanion(c, updateClass)).run(scope))

          case (o: g.ModuleDef) +: tail if hasTrigger(o.mods) =>
            val (ann, cleaned) = extractTrigger(o)
            val (upd, extra) = updateObject(ann, cleaned).run(scope)
            go(tail, out ++ upd.map(withAllPos(_, o.pos)).toVector ++ extra.map(withAllPos(_, o.pos)))

          case (t: g.TypeDef) +: tail if hasTrigger(t.mods) =>
            go(withoutCompanion(t.name.companionName, tail), out ++ updateTreeAndCompanion(t, updateType).run(scope))

          case t +: tail => go(tail, out :+ t)
        }

      go(trees.toVector, Vector()).toList
    }
  }

  override lazy val components: List[PluginComponent] = List(phase)
}
