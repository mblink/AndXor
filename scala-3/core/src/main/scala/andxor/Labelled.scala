package andxor

opaque type Labelled[A, L] = A

object Labelled {
  // TODO - this is only reported as unused because it's not used in `def label`
  // https://github.com/lampepfl/dotty/issues/17101
  extension [A, L](x: Labelled[A, L]) {
    def value: A = x
    def label(implicit l: ValueOf[L]): L = l.value
  }

  inline def apply[A, L](a: A): Labelled[A, L] = a

  inline given tcK0ForLabelled[TC[_], A, T](using tc: TC[A]): TC[Labelled[A, T]] =
    tc.asInstanceOf[TC[Labelled[A, T]]]

  inline given tcK1ForLabelled[TC[_[_]], T](using tc: TC[[a] =>> a]): TC[Labelled[*, T]] =
    tc.asInstanceOf[TC[Labelled[*, T]]]

  inline given valueOfLabelled[A, L](using a: ValueOf[A]): ValueOf[Labelled[A, L]] =
    new ValueOf[Labelled[A, L]](Labelled[A, L](a.value))
}
