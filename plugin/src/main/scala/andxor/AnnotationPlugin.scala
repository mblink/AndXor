package andxor

import scala.deprecated
import scala.collection.immutable.Map
import scala.collection.breakOut
import scala.reflect.internal.util._
import scala.tools.nsc._
import scala.tools.nsc.plugins._
import scala.tools.nsc.transform._

/**
 * A compiler plugin for code generation when an annotation is on a class, trait
 * or object. For classes and traits, ensures that a companion is created and
 * passes both to the implementation.
 *
 * TL;DR a drop-in replacement for syntactic annotation macros.
 *
 * The main advantage over annotation macros is that it works in all tooling
 * (except IntelliJ) and this approach will work in the long term
 * (macro-paradise is abandoned).
 *
 * The caveats are that typechecking and naming will not occur, so the code
 * generation / rewrites must be purely syntactic. If you require any type
 * inference, point your generated code at a blackbox macro that you implement
 * downstream.
 *
 * Unlike macro annotations, the annotation is not removed. This avoids false
 * negatives regarding unused imports. It should be possible to add a -cleanup
 * phase to remove the trigger annotations if needed.
 */
abstract class AnnotationPlugin(override val global: Global) extends Plugin {

  import global._
  override lazy val description: String =
    s"Generates code for annotations $triggers"

  case class NEL[A](head: A, tail: List[A]) { def toList: List[A] = List(head) ::: tail }
  object NEL {
    def apply[A](a: A, as: A*): NEL[A] = NEL(a, as.toList)
    def fromList[A](l: List[A]): Option[NEL[A]] = l match {
      case h :: t => Some(NEL(h, t))
      case _ => None
    }
  }

  // - The class itself
  // - Optional list of type params
  // - One or more explicit parameter groups
  // - Up to one implicit parameter group
  case class ClassT(
    klass: ClassDef,
    arity: Int,
    tpeParams: Option[NEL[TypeDef]],
    paramGroups: NEL[List[ValDef]],
    implParams: Option[NEL[ValDef]]
  ) {
    lazy val classTpe = AppliedTypeTree(Ident(klass.name), tpeParams.map(_.toList).getOrElse(Nil).map(t => Ident(t.name)))
  }

  def isImplicitVal(v: ValDef): Boolean = v.mods.hasFlag(Flag.IMPLICIT)
  def isImplicitVal(vs: List[ValDef]): Boolean = vs.forall(isImplicitVal(_))

  def getClassT(klass: ClassDef): ClassT = {
    val (ps, ips) = klass.impl.collect { case d@DefDef(_, name, _, _, _, _) if name == termNames.CONSTRUCTOR => d } match {
      case DefDef(_, _, _, vh :: vt, _, _) :: Nil if !isImplicitVal(vh) =>
        vt.span(!isImplicitVal(_)) match {
          case (e, i :: Nil) => (NEL(vh, e:_*), NEL.fromList(i))
          case (e, Nil) => (NEL(vh, e:_*), None)
          case _ => abort(s"Found more than one implicit parameter group for ${klass.name}")
        }
      case _ => abort(s"Failed to find exactly one constructor for ${klass.name}")
    }
    ClassT(klass, ps.toList.flatten.length, NEL.fromList(klass.tparams), ps, ips)
  }

  /** Annotations that trigger the plugin */
  def triggers: List[String]

  def updateClass(triggered: List[Tree], klass: ClassT): ClassT
  def updateCompanion(triggered: List[Tree], klass: ClassT, companion: ModuleDef): ModuleDef
  def updateModule(triggered: List[Tree], module: ModuleDef): ModuleDef

  /** Use to create code that shortcuts in ENSIME and ScalaIDE */
  def isIde: Boolean      = global.isInstanceOf[tools.nsc.interactive.Global]
  def isScaladoc: Boolean = global.isInstanceOf[tools.nsc.doc.ScaladocGlobal]

  // best way to inspect a tree, just call this
  def debug(name: String, tree: Tree): Unit =
    println(s"====\n$name ${tree.id} ${tree.pos}:\n${showCode(tree)}\n${showRaw(tree).split(')').mkString(")\n")}")

  // recovers the final part of an annotation
  def annotationName(ann: Tree): TermName =
    ann.children.collectFirst {
      case Select(New(Ident(name)), _)     => name.toTermName
      case Select(New(Select(_, name)), _) => name.toTermName
    }.getOrElse(abort(s"no name for $ann"))

  // case classes without companions should inherit Function
  def addSuperFunction(@deprecated("unused", "") klass: ClassT): Boolean =
    true

  implicit class RichTree[T <: Tree](private val t: T) {

    /** when generating a tree, use this to generate positions all the way down. */
    def withAllPos(pos: Position): T = {
      t.foreach { p =>
        val _ = p.setPos(
          new TransparentPosition(pos.source, pos.start, pos.end, pos.end)
        )
      }
      t
    }
  }

  private def phase = new PluginComponent with TypingTransformers {
    override val phaseName: String = AnnotationPlugin.this.name
    override val global: AnnotationPlugin.this.global.type =
      AnnotationPlugin.this.global
    override final def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit =
        newTransformer(unit).transformUnit(unit)
    }
    override val runsAfter: List[String]  = "parser" :: Nil
    override val runsBefore: List[String] = "namer" :: Nil

    private def newTransformer(unit: CompilationUnit) =
      new TypingTransformer(unit) {
        override def transform(tree: Tree): Tree =
          autobots(super.transform(tree))
      }

    val Triggers: List[TypeName] = triggers.map(newTypeName)
    private def hasTrigger(t: Tree): Boolean = t.exists {
      case c: ClassDef if hasTrigger(c.mods)  => true
      case m: ModuleDef if hasTrigger(m.mods) => true
      case _                                  => false
    }

    private def hasTrigger(mods: Modifiers): Boolean =
      Triggers.exists(mods.hasAnnotationNamed)

    private def extractTrigger(c: ClassT): (ClassT, List[Tree]) = {
      val trigger = getTriggers(c.klass.mods.annotations)
      //val mods = c.mods.mapAnnotations { anns => anns.filterNot(isNamed(_, Trigger)) }
      // if we remove the annotation, like a macro annotation, we end up with a
      // compiler warning saying that the annotation is unused. Perhaps we could
      // remove it after such warnings are emitted.
      //val update = treeCopy.ClassDef(c, mods, c.name, c.tparams, c.impl)
      val update = c
      (update, trigger)
    }
    private def extractTrigger(m: ModuleDef): (ModuleDef, List[Tree]) = {
      val trigger = getTriggers(m.mods.annotations)
      //val mods = m.mods.mapAnnotations { anns => anns.filterNot(isNamed(_, Trigger)) }
      //val update = treeCopy.ModuleDef(m, mods, m.name, m.impl)
      val update = m
      (update, trigger)
    }

    private def getTriggers(anns: List[Tree]): List[Tree] =
      anns.filter(a => Triggers.exists(isNamed(a, _)))

    private def isNamed(t: Tree, name: TypeName) = t match {
      case Apply(Select(New(Ident(`name`)), _), _)     => true
      case Apply(Select(New(Select(_, `name`)), _), _) => true
      case _                                           => false
    }

    /** generates a zero-functionality companion for a class */
    private def genCompanion(klass: ClassT): ModuleDef = {
      val mods =
        if (klass.klass.mods.hasFlag(Flag.PRIVATE))
          Modifiers(Flag.PRIVATE, klass.klass.mods.privateWithin)
        else if (klass.klass.mods.hasFlag(Flag.PROTECTED))
          Modifiers(Flag.PROTECTED, klass.klass.mods.privateWithin)
        else NoMods

      val isCase = klass.klass.mods.hasFlag(Flag.CASE)

      def toString_ =
        DefDef(
          Modifiers(Flag.OVERRIDE | Flag.SYNTHETIC),
          nme.toString_,
          Nil,
          Nil,
          Select(Select(Ident(nme.java), nme.lang), nme.String.toTypeName),
          Literal(Constant(klass.klass.name.companionName.decode))
        )

      val cons = DefDef(
        Modifiers(Flag.SYNTHETIC),
        nme.CONSTRUCTOR,
        Nil,
        List(Nil),
        TypeTree(),
        Block(List(pendingSuperCall), Literal(Constant(())))
      )

      ModuleDef(
        mods | Flag.SYNTHETIC,
        klass.klass.name.companionName,
        Template(
          List(Select(Ident(nme.scala_), nme.AnyRef.toTypeName)),
          noSelfType,
          cons :: (if (isCase) List(toString_) else Nil)
        )
      )
    }

    // responds to visiting all the parts of the tree and passes to decepticons
    // to do the rewrites
    def autobots(tree: Tree): Tree = tree match {
      case t: PackageDef if hasTrigger(t) =>
        val updated = decepticons(t.stats)
        treeCopy.PackageDef(t, t.pid, updated)

      case t: ModuleDef if hasTrigger(t.impl) =>
        val update = treeCopy.Template(
          t.impl,
          t.impl.parents,
          t.impl.self,
          decepticons(t.impl.body)
        )
        treeCopy.ModuleDef(
          t,
          t.mods,
          t.name,
          update
        )

      case t: ClassDef if hasTrigger(t.impl) =>
        // yuck, this finds classes and modules defined inside other classes.
        // added for completeness.
        val update = treeCopy.Template(
          t.impl,
          t.impl.parents,
          t.impl.self,
          decepticons(t.impl.body)
        )
        treeCopy.ClassDef(
          t,
          t.mods,
          t.name,
          t.tparams,
          update
        )

      case t => t
    }

    // does not recurse, let the autobots handle that
    def decepticons(trees: List[Tree]): List[Tree] = {
      val classes: Map[TypeName, ClassDef] = trees.collect {
        case c: ClassDef => c.name -> c
      }(breakOut)

      val modules: Map[TermName, ModuleDef] = trees.collect {
        case m: ModuleDef => m.name -> m
      }(breakOut)

      object ClassNoCompanion {
        def unapply(t: Tree): Option[ClassDef] = t match {
          case c: ClassDef if !modules.contains(c.name.companionName) =>
            Some(c)
          case _ => None
        }
      }

      object ClassHasCompanion {
        def unapply(t: Tree): Option[ClassDef] = t match {
          case c: ClassDef if modules.contains(c.name.companionName) =>
            Some(c)
          case _ => None
        }
      }

      object CompanionAndClass {
        def unapply(t: Tree): Option[(ModuleDef, ClassDef)] = t match {
          case m: ModuleDef =>
            classes.get(m.name.companionName).map { c =>
              (m, c)
            }
          case _ => None
        }
      }

      trees.flatMap {
        case ClassNoCompanion(c) if hasTrigger(c.mods) =>
          val klass            = getClassT(c)
          val companion        = genCompanion(klass).withAllPos(c.pos)
          val (cleaned, ann)   = extractTrigger(klass)
          val updatedCompanion = updateCompanion(ann, cleaned, companion)
          List(updateClass(ann, cleaned).klass, updatedCompanion)

        case ClassHasCompanion(c) if hasTrigger(c.mods) =>
          val klass          = getClassT(c)
          val (cleaned, ann) = extractTrigger(klass)
          List(updateClass(ann, cleaned).klass)

        case CompanionAndClass(companion, c) if hasTrigger(c.mods) =>
          val klass          = getClassT(c)
          val (cleaned, ann) = extractTrigger(klass)
          List(updateCompanion(ann, cleaned, companion))

        case m: ModuleDef if hasTrigger(m.mods) =>
          val (cleaned, ann) = extractTrigger(m)
          List(updateModule(ann, cleaned))

        case tr =>
          List(tr)
      }
    }

  }

  override lazy val components: List[PluginComponent] = List(phase)
}
