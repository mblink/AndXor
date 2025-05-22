package andxor

package object tuple {

  implicit class Tuple2Ops[A1, A2](t: (A1, A2)) {
    def t1: A1 = t._1
    def t2: A2 = t._2

    def map1[B](f: A1 => B): (B, A2) = {

      val a0 = t.t1
      val a1 = t.t2
      (f(a0), a1)

    }

    def map2[B](f: A2 => B): (A1, B) = {

      val a0 = t.t1
      val a1 = t.t2
      (a0, f(a1))

    }

  }

  implicit class Tuple3Ops[A1, A2, A3](t: (A1, A2, A3)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3

    def map1[B](f: A1 => B): (B, A2, A3) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      (f(a0), a1, a2)

    }

    def map2[B](f: A2 => B): (A1, B, A3) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      (a0, f(a1), a2)

    }

    def map3[B](f: A3 => B): (A1, A2, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      (a0, a1, f(a2))

    }

  }

  implicit class Tuple4Ops[A1, A2, A3, A4](t: (A1, A2, A3, A4)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4

    def map1[B](f: A1 => B): (B, A2, A3, A4) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      (f(a0), a1, a2, a3)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      (a0, f(a1), a2, a3)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      (a0, a1, f(a2), a3)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      (a0, a1, a2, f(a3))

    }

  }

  implicit class Tuple5Ops[A1, A2, A3, A4, A5](t: (A1, A2, A3, A4, A5)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      (f(a0), a1, a2, a3, a4)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      (a0, f(a1), a2, a3, a4)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      (a0, a1, f(a2), a3, a4)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      (a0, a1, a2, f(a3), a4)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      (a0, a1, a2, a3, f(a4))

    }

  }

  implicit class Tuple6Ops[A1, A2, A3, A4, A5, A6](t: (A1, A2, A3, A4, A5, A6)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      (f(a0), a1, a2, a3, a4, a5)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      (a0, f(a1), a2, a3, a4, a5)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      (a0, a1, f(a2), a3, a4, a5)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      (a0, a1, a2, f(a3), a4, a5)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      (a0, a1, a2, a3, f(a4), a5)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      (a0, a1, a2, a3, a4, f(a5))

    }

  }

  implicit class Tuple7Ops[A1, A2, A3, A4, A5, A6, A7](t: (A1, A2, A3, A4, A5, A6, A7)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      (f(a0), a1, a2, a3, a4, a5, a6)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      (a0, f(a1), a2, a3, a4, a5, a6)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      (a0, a1, f(a2), a3, a4, a5, a6)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      (a0, a1, a2, f(a3), a4, a5, a6)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      (a0, a1, a2, a3, f(a4), a5, a6)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      (a0, a1, a2, a3, a4, f(a5), a6)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      (a0, a1, a2, a3, a4, a5, f(a6))

    }

  }

  implicit class Tuple8Ops[A1, A2, A3, A4, A5, A6, A7, A8](t: (A1, A2, A3, A4, A5, A6, A7, A8)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7
    def t8: A8 = t._8

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      (f(a0), a1, a2, a3, a4, a5, a6, a7)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      (a0, f(a1), a2, a3, a4, a5, a6, a7)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      (a0, a1, f(a2), a3, a4, a5, a6, a7)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      (a0, a1, a2, f(a3), a4, a5, a6, a7)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      (a0, a1, a2, a3, f(a4), a5, a6, a7)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      (a0, a1, a2, a3, a4, f(a5), a6, a7)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      (a0, a1, a2, a3, a4, a5, f(a6), a7)

    }

    def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      (a0, a1, a2, a3, a4, a5, a6, f(a7))

    }

  }

  implicit class Tuple9Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9](t: (A1, A2, A3, A4, A5, A6, A7, A8, A9)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7
    def t8: A8 = t._8
    def t9: A9 = t._9

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      (f(a0), a1, a2, a3, a4, a5, a6, a7, a8)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      (a0, f(a1), a2, a3, a4, a5, a6, a7, a8)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      (a0, a1, f(a2), a3, a4, a5, a6, a7, a8)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      (a0, a1, a2, f(a3), a4, a5, a6, a7, a8)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      (a0, a1, a2, a3, f(a4), a5, a6, a7, a8)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      (a0, a1, a2, a3, a4, f(a5), a6, a7, a8)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      (a0, a1, a2, a3, a4, a5, f(a6), a7, a8)

    }

    def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      (a0, a1, a2, a3, a4, a5, a6, f(a7), a8)

    }

    def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      (a0, a1, a2, a3, a4, a5, a6, a7, f(a8))

    }

  }

  implicit class Tuple10Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7
    def t8: A8 = t._8
    def t9: A9 = t._9
    def t10: A10 = t._10

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9)

    }

    def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9)

    }

    def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9)

    }

    def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9))

    }

  }

  implicit class Tuple11Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7
    def t8: A8 = t._8
    def t9: A9 = t._9
    def t10: A10 = t._10
    def t11: A11 = t._11

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10)

    }

    def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10)

    }

    def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10)

    }

    def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10)

    }

    def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10))

    }

  }

  implicit class Tuple12Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7
    def t8: A8 = t._8
    def t9: A9 = t._9
    def t10: A10 = t._10
    def t11: A11 = t._11
    def t12: A12 = t._12

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11)

    }

    def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11)

    }

    def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11)

    }

    def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11)

    }

    def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11)

    }

    def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11))

    }

  }

  implicit class Tuple13Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7
    def t8: A8 = t._8
    def t9: A9 = t._9
    def t10: A10 = t._10
    def t11: A11 = t._11
    def t12: A12 = t._12
    def t13: A13 = t._13

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12)

    }

    def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12)

    }

    def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12)

    }

    def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12)

    }

    def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12)

    }

    def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12)

    }

    def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12))

    }

  }

  implicit class Tuple14Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7
    def t8: A8 = t._8
    def t9: A9 = t._9
    def t10: A10 = t._10
    def t11: A11 = t._11
    def t12: A12 = t._12
    def t13: A13 = t._13
    def t14: A14 = t._14

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13)

    }

    def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13)

    }

    def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13)

    }

    def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13)

    }

    def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13)

    }

    def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13)

    }

    def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13)

    }

    def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13))

    }

  }

  implicit class Tuple15Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7
    def t8: A8 = t._8
    def t9: A9 = t._9
    def t10: A10 = t._10
    def t11: A11 = t._11
    def t12: A12 = t._12
    def t13: A13 = t._13
    def t14: A14 = t._14
    def t15: A15 = t._15

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14)

    }

    def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14)

    }

    def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14)

    }

    def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14)

    }

    def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14)

    }

    def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14)

    }

    def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14)

    }

    def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14)

    }

    def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14))

    }

  }

  implicit class Tuple16Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7
    def t8: A8 = t._8
    def t9: A9 = t._9
    def t10: A10 = t._10
    def t11: A11 = t._11
    def t12: A12 = t._12
    def t13: A13 = t._13
    def t14: A14 = t._14
    def t15: A15 = t._15
    def t16: A16 = t._16

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14, a15)

    }

    def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14, a15)

    }

    def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14, a15)

    }

    def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14, a15)

    }

    def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14, a15)

    }

    def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14, a15)

    }

    def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14, a15)

    }

    def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14, a15)

    }

    def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14), a15)

    }

    def map16[B](f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, f(a15))

    }

  }

  implicit class Tuple17Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7
    def t8: A8 = t._8
    def t9: A9 = t._9
    def t10: A10 = t._10
    def t11: A11 = t._11
    def t12: A12 = t._12
    def t13: A13 = t._13
    def t14: A14 = t._14
    def t15: A15 = t._15
    def t16: A16 = t._16
    def t17: A17 = t._17

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

    }

    def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14, a15, a16)

    }

    def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14, a15, a16)

    }

    def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14, a15, a16)

    }

    def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14, a15, a16)

    }

    def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14, a15, a16)

    }

    def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14, a15, a16)

    }

    def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14, a15, a16)

    }

    def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14), a15, a16)

    }

    def map16[B](f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, f(a15), a16)

    }

    def map17[B](f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, f(a16))

    }

  }

  implicit class Tuple18Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7
    def t8: A8 = t._8
    def t9: A9 = t._9
    def t10: A10 = t._10
    def t11: A11 = t._11
    def t12: A12 = t._12
    def t13: A13 = t._13
    def t14: A14 = t._14
    def t15: A15 = t._15
    def t16: A16 = t._16
    def t17: A17 = t._17
    def t18: A18 = t._18

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

    }

    def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

    }

    def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14, a15, a16, a17)

    }

    def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14, a15, a16, a17)

    }

    def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14, a15, a16, a17)

    }

    def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14, a15, a16, a17)

    }

    def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14, a15, a16, a17)

    }

    def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14, a15, a16, a17)

    }

    def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14), a15, a16, a17)

    }

    def map16[B](f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, f(a15), a16, a17)

    }

    def map17[B](f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, f(a16), a17)

    }

    def map18[B](f: A18 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, f(a17))

    }

  }

  implicit class Tuple19Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7
    def t8: A8 = t._8
    def t9: A9 = t._9
    def t10: A10 = t._10
    def t11: A11 = t._11
    def t12: A12 = t._12
    def t13: A13 = t._13
    def t14: A14 = t._14
    def t15: A15 = t._15
    def t16: A16 = t._16
    def t17: A17 = t._17
    def t18: A18 = t._18
    def t19: A19 = t._19

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

    }

    def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

    }

    def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

    }

    def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14, a15, a16, a17, a18)

    }

    def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14, a15, a16, a17, a18)

    }

    def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14, a15, a16, a17, a18)

    }

    def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14, a15, a16, a17, a18)

    }

    def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14, a15, a16, a17, a18)

    }

    def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14), a15, a16, a17, a18)

    }

    def map16[B](f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, f(a15), a16, a17, a18)

    }

    def map17[B](f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, f(a16), a17, a18)

    }

    def map18[B](f: A18 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, f(a17), a18)

    }

    def map19[B](f: A19 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, f(a18))

    }

  }

  implicit class Tuple20Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7
    def t8: A8 = t._8
    def t9: A9 = t._9
    def t10: A10 = t._10
    def t11: A11 = t._11
    def t12: A12 = t._12
    def t13: A13 = t._13
    def t14: A14 = t._14
    def t15: A15 = t._15
    def t16: A16 = t._16
    def t17: A17 = t._17
    def t18: A18 = t._18
    def t19: A19 = t._19
    def t20: A20 = t._20

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

    }

    def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

    }

    def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

    }

    def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

    }

    def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14, a15, a16, a17, a18, a19)

    }

    def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14, a15, a16, a17, a18, a19)

    }

    def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14, a15, a16, a17, a18, a19)

    }

    def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14, a15, a16, a17, a18, a19)

    }

    def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14), a15, a16, a17, a18, a19)

    }

    def map16[B](f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, f(a15), a16, a17, a18, a19)

    }

    def map17[B](f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, f(a16), a17, a18, a19)

    }

    def map18[B](f: A18 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, f(a17), a18, a19)

    }

    def map19[B](f: A19 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B, A20) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, f(a18), a19)

    }

    def map20[B](f: A20 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, f(a19))

    }

  }

  implicit class Tuple21Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7
    def t8: A8 = t._8
    def t9: A9 = t._9
    def t10: A10 = t._10
    def t11: A11 = t._11
    def t12: A12 = t._12
    def t13: A13 = t._13
    def t14: A14 = t._14
    def t15: A15 = t._15
    def t16: A16 = t._16
    def t17: A17 = t._17
    def t18: A18 = t._18
    def t19: A19 = t._19
    def t20: A20 = t._20
    def t21: A21 = t._21

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

    }

    def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

    }

    def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

    }

    def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

    }

    def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

    }

    def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14, a15, a16, a17, a18, a19, a20)

    }

    def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14, a15, a16, a17, a18, a19, a20)

    }

    def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14, a15, a16, a17, a18, a19, a20)

    }

    def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14), a15, a16, a17, a18, a19, a20)

    }

    def map16[B](f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, f(a15), a16, a17, a18, a19, a20)

    }

    def map17[B](f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, f(a16), a17, a18, a19, a20)

    }

    def map18[B](f: A18 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, f(a17), a18, a19, a20)

    }

    def map19[B](f: A19 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B, A20, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, f(a18), a19, a20)

    }

    def map20[B](f: A20 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B, A21) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, f(a19), a20)

    }

    def map21[B](f: A21 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, f(a20))

    }

  }

  implicit class Tuple22Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](t: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7
    def t8: A8 = t._8
    def t9: A9 = t._9
    def t10: A10 = t._10
    def t11: A11 = t._11
    def t12: A12 = t._12
    def t13: A13 = t._13
    def t14: A14 = t._14
    def t15: A15 = t._15
    def t16: A16 = t._16
    def t17: A17 = t._17
    def t18: A18 = t._18
    def t19: A19 = t._19
    def t20: A20 = t._20
    def t21: A21 = t._21
    def t22: A22 = t._22

    def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

    }

    def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

    }

    def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

    }

    def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

    }

    def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

    }

    def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

    }

    def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

    }

    def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

    }

    def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

    }

    def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

    }

    def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

    }

    def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

    }

    def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14, a15, a16, a17, a18, a19, a20, a21)

    }

    def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14, a15, a16, a17, a18, a19, a20, a21)

    }

    def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14), a15, a16, a17, a18, a19, a20, a21)

    }

    def map16[B](f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, f(a15), a16, a17, a18, a19, a20, a21)

    }

    def map17[B](f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, f(a16), a17, a18, a19, a20, a21)

    }

    def map18[B](f: A18 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, f(a17), a18, a19, a20, a21)

    }

    def map19[B](f: A19 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B, A20, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, f(a18), a19, a20, a21)

    }

    def map20[B](f: A20 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B, A21, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, f(a19), a20, a21)

    }

    def map21[B](f: A21 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B, A22) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, f(a20), a21)

    }

    def map22[B](f: A22 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, B) = {

      val a0 = t.t1
      val a1 = t.t2
      val a2 = t.t3
      val a3 = t.t4
      val a4 = t.t5
      val a5 = t.t6
      val a6 = t.t7
      val a7 = t.t8
      val a8 = t.t9
      val a9 = t.t10
      val a10 = t.t11
      val a11 = t.t12
      val a12 = t.t13
      val a13 = t.t14
      val a14 = t.t15
      val a15 = t.t16
      val a16 = t.t17
      val a17 = t.t18
      val a18 = t.t19
      val a19 = t.t20
      val a20 = t.t21
      val a21 = t.t22
      (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, f(a21))

    }

  }

}
