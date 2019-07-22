package andxor
package routes

import tapir.Codec.PlainCodec

sealed trait QueryString
case class QueryParam[A](name: String, codec: PlainCodec[A]) extends QueryString
