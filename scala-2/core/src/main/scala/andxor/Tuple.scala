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

  implicit class Tuple7Ops[A1, A2, A3, A4, A5, A6, A7](t: (A1, A2, A3, A4, A5, A6, A7)) {
    def t1: A1 = t._1
    def t2: A2 = t._2
    def t3: A3 = t._3
    def t4: A4 = t._4
    def t5: A5 = t._5
    def t6: A6 = t._6
    def t7: A7 = t._7

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

  }

}
