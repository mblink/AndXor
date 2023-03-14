package andxor

import cats.{Applicative, Eq, Eval, Traverse}

object labelled {
  @newtype case class Labelled[A, L](value: A) {
    def label(implicit l: ValueOf[L]): L = l.value
  }

  sealed trait LabelledLP {
    implicit def eqLabelledSingleton[A <: Singleton, L <: String](implicit l: ValueOf[L]): Eq[Labelled[A, L]] =
      Eq.instance((a1, a2) => a1.value == a2.value && a1.label == a2.label)
  }

  object Labelled extends LabelledLP {
    implicit def traverseLabelled[L]: Traverse[Labelled[*, L]] = new Traverse[Labelled[*, L]] {
      def foldLeft[A, B](fa: Labelled[A, L], b: B)(f: (B, A) => B): B = f(b, fa.value)
      def foldRight[A, B](fa: Labelled[A, L], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa.value, lb)
      def traverse[G[_], A, B](fa: Labelled[A, L])(f: A => G[B])(implicit G: Applicative[G]): G[Labelled[B, L]] =
        G.map(f(fa.value))(Labelled(_))
    }

    implicit def eqLabelled[A: Eq, L <: String](implicit l: ValueOf[L]): Eq[Labelled[A, L]] =
      Eq.by(a => (l.value: String, a.value))

    implicit def valueOfLabelled[A, L](implicit a: ValueOf[A]): ValueOf[Labelled[A, L]] =
      new ValueOf[Labelled[A, L]](Labelled[A, L](a.value))
  }
}
