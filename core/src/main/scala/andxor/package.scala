package andxor

import scala.annotation.Annotation

final class deriveCovariant(val typeclasses: AnyRef*) extends Annotation
final class deriveLabelledCovariant(val typeclasses: AnyRef*) extends Annotation
final class deriveContravariant(val typeclasses: AnyRef*) extends Annotation
final class deriveLabelledContravariant(val typeclasses: AnyRef*) extends Annotation
