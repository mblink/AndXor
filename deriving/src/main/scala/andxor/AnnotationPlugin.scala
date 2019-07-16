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
  import global._

  protected var debugMode = false
  protected def setDebug(d: Boolean): Unit = { debugMode = d }

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

  private def showTree(tree: Tree, pretty: Boolean): String =
    if (pretty) prettyPrint(showRaw(tree)) else showRaw(tree)

  private def debugStr(name: String, tree: Tree, pretty: Boolean = true): String =
    s"===\n$name ${tree.pos}:\n${show(tree)}\n${showTree(tree, pretty)}"

  protected def debug(args: (String, Any)*): Unit = if (debugMode) println(s"""
********************************************************************************
${args.map { case (s, v) => if (v.isInstanceOf[Tree]) debugStr(s, v.asInstanceOf[Tree]) else s"$s: $v" }.mkString("\n\n")}
********************************************************************************
""")

  protected def debug(name: String, tree: Tree, pretty: Boolean = true): Unit =
    println(debugStr(name, tree, pretty))

  override lazy val description: String =
    s"Generates code for annotations $triggers"

  /** Annotations that trigger the plugin */
  def triggers: List[String]

  def isImplicitVal(v: ValDef): Boolean = v.mods.isImplicit
  def isImplicitVal(vs: List[ValDef]): Boolean = Some(vs).filter(_.nonEmpty).fold(false)(_.forall(isImplicitVal(_)))

  def ctorParams(klass: ClassDef): (List[List[ValDef]], Option[List[ValDef]]) =
    klass.impl.body.collect { case d@DefDef(_, termNames.CONSTRUCTOR, _, _, _, impl) => d } match {
      case DefDef(_, _, _, vs, _, _) :: Nil if vs.headOption.fold(true)(!isImplicitVal(_)) =>
        vs.span(!isImplicitVal(_)) match {
          case (e, i :: Nil) => (e, Some(i))
          case (e, Nil) => (e, None)
          case _ => abort(s"Found more than one implicit parameter group for ${klass.name}")
        }
      case ds =>
        error(klass.pos, s"Failed to find exactly one constructor for class `${klass.name}`, ${ds.map(debugStr("d", _))}")
        (Nil, None)
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

    implicit lazy val setClassMods: Set0[ClassDef, Modifiers] = Set0((x, m) => treeCopy.ClassDef(x, m, x.name, x.tparams, x.impl))
    implicit lazy val setObjMods: Set0[ModuleDef, Modifiers] = Set0((x, m) => treeCopy.ModuleDef(x, m, x.name, x.impl))
    implicit lazy val setTypeMods: Set0[TypeDef, Modifiers] = Set0((x, m) => treeCopy.TypeDef(x, m, x.name, x.tparams, x.rhs))
  }
  implicit class Set0Ops[A](a: A) {
    def set[T](t: T)(implicit s: Set0[A, T]): A = s.set(a, t)
  }

  case class LocalScope(
    self: Tree,
    classes: Map[String, ClassDef],
    objects: Map[String, ModuleDef],
    traits: Map[String, ClassDef],
    types: Map[String, TypeDef]
  )

  def error(pos: Position, msg: String): Unit = globalError(pos, msg)

  def updateClass(
    @deprecated("unused", "") anns: List[Tree],
    klass: ClassDef,
    companion: Option[ModuleDef]
  ): Reader[LocalScope, (Option[(ClassDef, Option[ModuleDef])], Vector[Tree])] =
    Reader(_ => (Some((klass, companion)), Vector()))

  def updateObject(
    @deprecated("unused", "") anns: List[Tree],
    obj: ModuleDef
  ): Reader[LocalScope, (Option[ModuleDef], Vector[Tree])] =
    Reader(_ => (Some(obj), Vector()))

  def updateTrait(
    @deprecated("unused", "") anns: List[Tree],
    tr: ClassDef,
    companion: Option[ModuleDef]
  ): Reader[LocalScope, (Option[(ClassDef, Option[ModuleDef])], Vector[Tree])] =
    Reader(_ => (Some((tr, companion)), Vector()))

  def updateType(
    @deprecated("unused", "") anns: List[Tree],
    tpe: TypeDef,
    companion: Option[ModuleDef]
  ): Reader[LocalScope, (Option[(TypeDef, Option[ModuleDef])], Vector[Tree])] =
    Reader(_ => (Some((tpe, companion)), Vector()))

  def genCompanion[A <: NameTree: Get[?, Modifiers]](tree: A): ModuleDef = {
    val (name, mods) = (tree.name, tree.get[Modifiers])
    val objMods = Option(Modifiers(Flag.PRIVATE)).filter(_ => mods.isPrivate)
      .orElse(Option(Modifiers(Flag.PROTECTED)).filter(_ => mods.isProtected))
      .getOrElse(Modifiers())

    def toString_ =
      q"override def toString: _root_.java.lang.String = ${Literal(Constant(tree.name.companionName.decode))}"

    q"""$objMods object ${TermName(name.decode)} extends _root_.scala.AnyRef {
      ..${if (mods.isCase) List(toString_) else Nil}
    }"""
  }

  def regenObject(obj: ModuleDef, extras: List[Tree]): ModuleDef =
    extras match {
      case Nil => obj
      case x => treeCopy.ModuleDef(obj, obj.mods, obj.name,
        treeCopy.Template(obj.impl, obj.impl.parents, obj.impl.self, obj.impl.body ::: x))
    }

  private def phase = new PluginComponent with TypingTransformers {
    override val phaseName: String = AnnotationPlugin.this.name
    override val global: AnnotationPlugin.this.global.type =
      AnnotationPlugin.this.global
    override final def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = newTransformer(unit).transformUnit(unit)
    }

    private def newTransformer(unit: CompilationUnit) =
      new TypingTransformer(unit) {
        override def transform(tree: Tree): Tree =
          autobots(super.transform(tree))
    }

    override val runsAfter: List[String] = List("parser")
    override val runsBefore: List[String] = List("namer")

    val Triggers: List[TypeName] = triggers.map(newTypeName)
    private def hasTrigger(t: Tree): Boolean = t.exists {
      case c: ClassDef if hasTrigger(c.mods)  => true
      case m: ModuleDef if hasTrigger(m.mods) => true
      case _                                  => false
    }

    private def hasTrigger(mods: Modifiers): Boolean = Triggers.exists(mods.hasAnnotationNamed)

    private def extractTrigger[A <: Tree: Get[?, Modifiers]: Set0[?, Modifiers]](tree: A): (List[Tree], A) = {
      val mods = tree.get[Modifiers]
      val (triggers, rest) = getTriggers(mods.annotations)
      val update = tree.set(mods.mapAnnotations(_ => rest))
      (triggers, update)
    }

    private def getTriggers(anns: List[Tree]): (List[Tree], List[Tree]) =
      anns.partition(a => Triggers.exists(isNamed(a, _)))

    private def isNamed(t: Tree, name: TypeName) = t match {
      case Apply(Select(New(Ident(`name`)), _), _)     => true
      case Apply(Select(New(Select(_, `name`)), _), _) => true
      case _                                           => false
    }

    private def withAllPos[A <: Tree](tree: A, pos: Position): A = {
      tree.foreach { t =>
        if (!t.pos.isDefined || t.pos == NoPosition)
          t.setPos(new TransparentPosition(pos.source, pos.start, pos.end, pos.end))
        ()
      }
      tree
    }

    private def updateTreeAndCompanion[A <: NameTree: Get[?, Modifiers]: Set0[?, Modifiers]](
      tree: A,
      updF: (List[Tree], A, Option[ModuleDef]) => Reader[LocalScope, (Option[(A, Option[ModuleDef])], Vector[Tree])]
    ): Reader[LocalScope, (Option[A], Option[ModuleDef], Vector[Tree])] =
      for {
        companion <- Reader((_: LocalScope).objects.get(tree.name.decode))
        t = extractTrigger(tree)
        updated <- updF(t._1, t._2, companion)
        upd = (ts: Vector[Tree], p: Position) => ts.map(withAllPos(_, p))
        updA = updated._1.map(_._1).map(withAllPos(_, tree.pos))
        updCompanion = updated._1.flatMap(_._2).map(withAllPos(_, companion.getOrElse(tree).pos))
        updExtra = updated._2.map(withAllPos(_, tree.pos))
      } yield (updA, updCompanion, updExtra)

    // responds to visiting all the parts of the tree and passes to decepticons
    // to do the rewrites
    def autobots(tree: Tree): Tree =
      tree match {
        case p: PackageDef if hasTrigger(p) =>
          treeCopy.PackageDef(p, p.pid, decepticons(p, p.stats))
        case m: ModuleDef if hasTrigger(m.impl) =>
          treeCopy.ModuleDef(m, m.mods, m.name,
            treeCopy.Template(m.impl, m.impl.parents, m.impl.self, decepticons(m, m.impl.body)))
        case c: ClassDef if hasTrigger(c.impl) =>
          treeCopy.ClassDef(c, c.mods, c.name, c.tparams,
            treeCopy.Template(c.impl, c.impl.parents, c.impl.self, decepticons(c, c.impl.body)))
        case t => t
      }

    private def withoutCompanion(companionName: Name, stats: Vector[Tree]): Vector[Tree] =
      stats.filter(_ match {
        case o: ModuleDef if o.name.decodedName.toString == companionName.decodedName.toString => false
        case _ => true
      })

    private def getLocals[A <: NameTree](trees: List[Tree])(pf: PartialFunction[Tree, A]): Map[String, A] =
      trees.flatMap(pf.lift(_).map(a => a.name.decodedName.toString -> a)).toMap

    private def isTrait(c: ClassDef): Boolean = c.mods.isTrait

    private def processResult[A <: NameTree: Get[?, Modifiers]](tpe: String, a: Option[A], companion: Option[ModuleDef], extras: Vector[Tree]): Vector[Tree] = {
      if (debugMode) {
        val prefix = s"${a.fold("")(x => s"${x.get[Modifiers].flagString} $tpe ${x.name.decode}")}"
        a.map(x => debug(prefix -> x))
        companion.map(x => debug(s"$prefix companion" -> x))
        Some(extras).filter(_.nonEmpty).foreach(xs => debug(xs.map(x => s"$prefix extra" -> x):_*))
      }

      a.toVector ++ companion.toVector ++ extras
    }

    def decepticons(scopeSelf: Tree, trees: List[Tree]): List[Tree] = {
      lazy val scope = LocalScope(
        scopeSelf,
        getLocals[ClassDef](trees) { case c: ClassDef if !isTrait(c) => c },
        getLocals[ModuleDef](trees) { case o: ModuleDef => o },
        getLocals[ClassDef](trees) { case t: ClassDef if isTrait(t) => t },
        getLocals[TypeDef](trees) { case t: TypeDef => t })

      @tailrec
      def go(queue: Vector[Tree], out: Vector[Tree]): Vector[Tree] =
        queue match {
          case Vector() => out

          case (c: ClassDef) +: tail if hasTrigger(c.mods) =>
            val (updC, companion, extras) =
              (if (isTrait(c)) updateTreeAndCompanion(c, updateTrait)
              else updateTreeAndCompanion(c, updateClass)).run(scope)
            go(withoutCompanion(c.name.companionName, tail),
              out ++ processResult(if (isTrait(c)) "trait" else "class", updC, companion, extras))

          case (o: ModuleDef) +: tail if hasTrigger(o.mods) =>
            val (ann, cleaned) = extractTrigger(o)
            val (upd, extra) = updateObject(ann, cleaned).run(scope)
            val (updO, extras) = (upd.map(withAllPos(_, o.pos)), extra.map(withAllPos(_, o.pos)))
            go(tail, out ++ processResult("object", updO, None, extras))

          case (t: TypeDef) +: tail if hasTrigger(t.mods) =>
            val (updT, companion, extras) = updateTreeAndCompanion(t, updateType).run(scope)
            go(withoutCompanion(t.name.companionName, tail),
              out ++ processResult("type", updT, companion, extras))

          case t +: tail => go(tail, out :+ t)
        }

      go(trees.toVector, Vector()).toList
    }
  }

  override lazy val components: List[PluginComponent] = List(phase)
}
