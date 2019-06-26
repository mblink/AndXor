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
}
