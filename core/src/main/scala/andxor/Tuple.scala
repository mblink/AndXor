package andxor

package object tuple {

  implicit class Tuple2Ops[A1, A2](t: (A1, A2)) {
    def t1: A1 = t._1
    def t2: A2 = t._2

  }

  implicit class Tuple3Ops[A1, A2, A3](t: (A1, A2, A3)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3

  }

  implicit class Tuple4Ops[A1, A2, A3, A4](t: (A1, A2, A3, A4)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4

  }

}
