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

  implicit class Tuple5Ops[A1, A2, A3, A4, A5](t: (A1, A2, A3, A4, A5)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5

  }

  implicit class Tuple6Ops[A1, A2, A3, A4, A5, A6](t: (A1, A2, A3, A4, A5, A6)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6

  }

}
