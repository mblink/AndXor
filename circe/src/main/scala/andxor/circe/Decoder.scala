package andxor.circe

import _root_.io.circe.{Decoder, DecodingFailure}

trait DecoderAp {
  def ap[A, B](fa: => Decoder[A])(f: => Decoder[A => B]): Decoder[B] =
    Decoder.instance(c => (f(c), fa(c)) match {
      case (Right(g), Right(a)) => Right(g(a))
      case (Left(f1), Left(f2)) => Left(DecodingFailure(f1.message, f1.history ++ f2.history))
      case (Left(f), _) => Left(f)
      case (_, Left(f)) => Left(f)
    })
}
