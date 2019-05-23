package andxor

import scala.annotation.Annotation

final class deriving(
  val covariant: Vector[AnyRef] = Vector(),
  val labelledCovariant: Vector[AnyRef] = Vector(),
  val contravariant: Vector[AnyRef] = Vector(),
  val labelledContravariant: Vector[AnyRef] = Vector(),
) extends Annotation
