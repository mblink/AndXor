package andxor
package routes

import tapir.Codec.PlainCodec

sealed trait Path
case class FixedPath(s: String) extends Path
case class PathCapture[A](codec: PlainCodec[A], name: String) extends Path
case object PathsCapture extends Path
