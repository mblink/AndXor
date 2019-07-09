package andxor.test

trait Show[A] { def apply(a: A): String }
object Show {
  def apply[A](a: A)(implicit s: Show[A]): String = s(a)
}

object types {
  @newtype type Test1 = String
  object Test1 {
    def callValue(t: Test1): String = t.value
  }

  @newtype case class Test2(run: Int) {
    def extension: String = ""
  }
  object Test2 {
    def callRun(t: Test2): Int = t.run
    def callExtension(t: Test2): String = t.extension
  }

  class Test3() {
    @newtype case class Test4(run: Boolean) {
      def extension: String = ""
    }

    object Test4 {
      def callRun(t: Test4): Boolean = t.run
      def callExtension(t: Test4): String = t.extension
    }
  }

  @newtype case class Test4(run: String)
  object Test4 {
    implicit val show: Show[Test4] = new Show[Test4] { def apply(t: Test4): String = s"newtype Test4(${t.run})" }

    def callRun(t: Test4): String = t.run
    def printTest4(t: Test4): Unit = println(Show(t))
  }

  trait Map2P[A1, A2] {
    val mapN = this

    def map1[B](p: (A1, A2))(f: A1 => B): (B, A2) = {
      val a0 = p._1
      val a1 = p._2
      (f(a0), a1)
    }

    def mapAt[B](f: A1 => B)(p: (A1, A2)): (B, A2) =
      mapN.map1(p)(f)

    def map2[B](p: (A1, A2))(f: A2 => B): (A1, B) = {
      val a0 = p._1
      val a1 = p._2
      (a0, f(a1))
    }

    def mapAt[B](f: A2 => B)(p: (A1, A2))(implicit @deprecated("unused", "") d: DummyImplicit): (A1, B) =
      mapN.map2(p)(f)
  }

  @newtype case class Prod2[F[_], A1[_[_]], A2[_[_]]](run: (A1[F], A2[F])) { self =>
    def t1: A1[F] = run._1
    def t2: A2[F] = run._2

    private def mapN = new Map2P[A1[F], A2[F]] {}

    def map1[B[_[_]]](f: A1[F] => B[F]): Prod2[F, B, A2] =
      Prod2[F, B, A2](mapN.map1(run)(f))

    def mapAt[B[_[_]]](f: A1[F] => B[F]): Prod2[F, B, A2] =
      Prod2[F, B, A2](mapN.mapAt(f)(run))

    def map2[B[_[_]]](f: A2[F] => B[F]): Prod2[F, A1, B] =
      Prod2[F, A1, B](mapN.map2(run)(f))

    def mapAt[B[_[_]]](f: A2[F] => B[F])(implicit d: DummyImplicit): Prod2[F, A1, B] =
      Prod2[F, A1, B](mapN.mapAt(f)(run))
  }
}
