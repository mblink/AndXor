package andxor

import scala.annotation.Annotation

// final class andxor extends Annotation
final class deriveCovariant(val typeclasses: AnyRef*) extends Annotation
final class deriveContravariant(val typeclasses: AnyRef*) extends Annotation
