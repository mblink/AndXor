package andxor

import scalaz.{Monoid, Need}

package object tuple {

  implicit def tuple9Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9](
      implicit m0: Monoid[A1],
      m1: Monoid[A2],
      m2: Monoid[A3],
      m3: Monoid[A4],
      m4: Monoid[A5],
      m5: Monoid[A6],
      m6: Monoid[A7],
      m7: Monoid[A8],
      m8: Monoid[A9]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (
          m0.append(t1._1, t2.value._1),
          m1.append(t1._2, t2.value._2),
          m2.append(t1._3, t2.value._3),
          m3.append(t1._4, t2.value._4),
          m4.append(t1._5, t2.value._5),
          m5.append(t1._6, t2.value._6),
          m6.append(t1._7, t2.value._7),
          m7.append(t1._8, t2.value._8),
          m8.append(t1._9, t2.value._9)
        )
      }
    }

  implicit def tuple10Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
      implicit m0: Monoid[A1],
      m1: Monoid[A2],
      m2: Monoid[A3],
      m3: Monoid[A4],
      m4: Monoid[A5],
      m5: Monoid[A6],
      m6: Monoid[A7],
      m7: Monoid[A8],
      m8: Monoid[A9],
      m9: Monoid[A10]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (
          m0.append(t1._1, t2.value._1),
          m1.append(t1._2, t2.value._2),
          m2.append(t1._3, t2.value._3),
          m3.append(t1._4, t2.value._4),
          m4.append(t1._5, t2.value._5),
          m5.append(t1._6, t2.value._6),
          m6.append(t1._7, t2.value._7),
          m7.append(t1._8, t2.value._8),
          m8.append(t1._9, t2.value._9),
          m9.append(t1._10, t2.value._10)
        )
      }
    }

  implicit def tuple11Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
      implicit m0: Monoid[A1],
      m1: Monoid[A2],
      m2: Monoid[A3],
      m3: Monoid[A4],
      m4: Monoid[A5],
      m5: Monoid[A6],
      m6: Monoid[A7],
      m7: Monoid[A8],
      m8: Monoid[A9],
      m9: Monoid[A10],
      m10: Monoid[A11]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (
          m0.append(t1._1, t2.value._1),
          m1.append(t1._2, t2.value._2),
          m2.append(t1._3, t2.value._3),
          m3.append(t1._4, t2.value._4),
          m4.append(t1._5, t2.value._5),
          m5.append(t1._6, t2.value._6),
          m6.append(t1._7, t2.value._7),
          m7.append(t1._8, t2.value._8),
          m8.append(t1._9, t2.value._9),
          m9.append(t1._10, t2.value._10),
          m10.append(t1._11, t2.value._11)
        )
      }
    }

  implicit def tuple12Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      implicit m0: Monoid[A1],
      m1: Monoid[A2],
      m2: Monoid[A3],
      m3: Monoid[A4],
      m4: Monoid[A5],
      m5: Monoid[A6],
      m6: Monoid[A7],
      m7: Monoid[A8],
      m8: Monoid[A9],
      m9: Monoid[A10],
      m10: Monoid[A11],
      m11: Monoid[A12]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (
          m0.append(t1._1, t2.value._1),
          m1.append(t1._2, t2.value._2),
          m2.append(t1._3, t2.value._3),
          m3.append(t1._4, t2.value._4),
          m4.append(t1._5, t2.value._5),
          m5.append(t1._6, t2.value._6),
          m6.append(t1._7, t2.value._7),
          m7.append(t1._8, t2.value._8),
          m8.append(t1._9, t2.value._9),
          m9.append(t1._10, t2.value._10),
          m10.append(t1._11, t2.value._11),
          m11.append(t1._12, t2.value._12)
        )
      }
    }

  implicit def tuple13Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
      implicit m0: Monoid[A1],
      m1: Monoid[A2],
      m2: Monoid[A3],
      m3: Monoid[A4],
      m4: Monoid[A5],
      m5: Monoid[A6],
      m6: Monoid[A7],
      m7: Monoid[A8],
      m8: Monoid[A9],
      m9: Monoid[A10],
      m10: Monoid[A11],
      m11: Monoid[A12],
      m12: Monoid[A13]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (
          m0.append(t1._1, t2.value._1),
          m1.append(t1._2, t2.value._2),
          m2.append(t1._3, t2.value._3),
          m3.append(t1._4, t2.value._4),
          m4.append(t1._5, t2.value._5),
          m5.append(t1._6, t2.value._6),
          m6.append(t1._7, t2.value._7),
          m7.append(t1._8, t2.value._8),
          m8.append(t1._9, t2.value._9),
          m9.append(t1._10, t2.value._10),
          m10.append(t1._11, t2.value._11),
          m11.append(t1._12, t2.value._12),
          m12.append(t1._13, t2.value._13)
        )
      }
    }

  implicit def tuple14Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
      implicit m0: Monoid[A1],
      m1: Monoid[A2],
      m2: Monoid[A3],
      m3: Monoid[A4],
      m4: Monoid[A5],
      m5: Monoid[A6],
      m6: Monoid[A7],
      m7: Monoid[A8],
      m8: Monoid[A9],
      m9: Monoid[A10],
      m10: Monoid[A11],
      m11: Monoid[A12],
      m12: Monoid[A13],
      m13: Monoid[A14]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (
          m0.append(t1._1, t2.value._1),
          m1.append(t1._2, t2.value._2),
          m2.append(t1._3, t2.value._3),
          m3.append(t1._4, t2.value._4),
          m4.append(t1._5, t2.value._5),
          m5.append(t1._6, t2.value._6),
          m6.append(t1._7, t2.value._7),
          m7.append(t1._8, t2.value._8),
          m8.append(t1._9, t2.value._9),
          m9.append(t1._10, t2.value._10),
          m10.append(t1._11, t2.value._11),
          m11.append(t1._12, t2.value._12),
          m12.append(t1._13, t2.value._13),
          m13.append(t1._14, t2.value._14)
        )
      }
    }

  implicit def tuple15Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
      implicit m0: Monoid[A1],
      m1: Monoid[A2],
      m2: Monoid[A3],
      m3: Monoid[A4],
      m4: Monoid[A5],
      m5: Monoid[A6],
      m6: Monoid[A7],
      m7: Monoid[A8],
      m8: Monoid[A9],
      m9: Monoid[A10],
      m10: Monoid[A11],
      m11: Monoid[A12],
      m12: Monoid[A13],
      m13: Monoid[A14],
      m14: Monoid[A15]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero, m14.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (
          m0.append(t1._1, t2.value._1),
          m1.append(t1._2, t2.value._2),
          m2.append(t1._3, t2.value._3),
          m3.append(t1._4, t2.value._4),
          m4.append(t1._5, t2.value._5),
          m5.append(t1._6, t2.value._6),
          m6.append(t1._7, t2.value._7),
          m7.append(t1._8, t2.value._8),
          m8.append(t1._9, t2.value._9),
          m9.append(t1._10, t2.value._10),
          m10.append(t1._11, t2.value._11),
          m11.append(t1._12, t2.value._12),
          m12.append(t1._13, t2.value._13),
          m13.append(t1._14, t2.value._14),
          m14.append(t1._15, t2.value._15)
        )
      }
    }

  implicit def tuple16Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
      implicit m0: Monoid[A1],
      m1: Monoid[A2],
      m2: Monoid[A3],
      m3: Monoid[A4],
      m4: Monoid[A5],
      m5: Monoid[A6],
      m6: Monoid[A7],
      m7: Monoid[A8],
      m8: Monoid[A9],
      m9: Monoid[A10],
      m10: Monoid[A11],
      m11: Monoid[A12],
      m12: Monoid[A13],
      m13: Monoid[A14],
      m14: Monoid[A15],
      m15: Monoid[A16]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero, m14.zero, m15.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (
          m0.append(t1._1, t2.value._1),
          m1.append(t1._2, t2.value._2),
          m2.append(t1._3, t2.value._3),
          m3.append(t1._4, t2.value._4),
          m4.append(t1._5, t2.value._5),
          m5.append(t1._6, t2.value._6),
          m6.append(t1._7, t2.value._7),
          m7.append(t1._8, t2.value._8),
          m8.append(t1._9, t2.value._9),
          m9.append(t1._10, t2.value._10),
          m10.append(t1._11, t2.value._11),
          m11.append(t1._12, t2.value._12),
          m12.append(t1._13, t2.value._13),
          m13.append(t1._14, t2.value._14),
          m14.append(t1._15, t2.value._15),
          m15.append(t1._16, t2.value._16)
        )
      }
    }

  implicit def tuple17Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
      implicit m0: Monoid[A1],
      m1: Monoid[A2],
      m2: Monoid[A3],
      m3: Monoid[A4],
      m4: Monoid[A5],
      m5: Monoid[A6],
      m6: Monoid[A7],
      m7: Monoid[A8],
      m8: Monoid[A9],
      m9: Monoid[A10],
      m10: Monoid[A11],
      m11: Monoid[A12],
      m12: Monoid[A13],
      m13: Monoid[A14],
      m14: Monoid[A15],
      m15: Monoid[A16],
      m16: Monoid[A17]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero, m14.zero, m15.zero, m16.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (
          m0.append(t1._1, t2.value._1),
          m1.append(t1._2, t2.value._2),
          m2.append(t1._3, t2.value._3),
          m3.append(t1._4, t2.value._4),
          m4.append(t1._5, t2.value._5),
          m5.append(t1._6, t2.value._6),
          m6.append(t1._7, t2.value._7),
          m7.append(t1._8, t2.value._8),
          m8.append(t1._9, t2.value._9),
          m9.append(t1._10, t2.value._10),
          m10.append(t1._11, t2.value._11),
          m11.append(t1._12, t2.value._12),
          m12.append(t1._13, t2.value._13),
          m13.append(t1._14, t2.value._14),
          m14.append(t1._15, t2.value._15),
          m15.append(t1._16, t2.value._16),
          m16.append(t1._17, t2.value._17)
        )
      }
    }

  implicit def tuple18Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
      implicit m0: Monoid[A1],
      m1: Monoid[A2],
      m2: Monoid[A3],
      m3: Monoid[A4],
      m4: Monoid[A5],
      m5: Monoid[A6],
      m6: Monoid[A7],
      m7: Monoid[A8],
      m8: Monoid[A9],
      m9: Monoid[A10],
      m10: Monoid[A11],
      m11: Monoid[A12],
      m12: Monoid[A13],
      m13: Monoid[A14],
      m14: Monoid[A15],
      m15: Monoid[A16],
      m16: Monoid[A17],
      m17: Monoid[A18]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero, m14.zero, m15.zero, m16.zero, m17.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (
          m0.append(t1._1, t2.value._1),
          m1.append(t1._2, t2.value._2),
          m2.append(t1._3, t2.value._3),
          m3.append(t1._4, t2.value._4),
          m4.append(t1._5, t2.value._5),
          m5.append(t1._6, t2.value._6),
          m6.append(t1._7, t2.value._7),
          m7.append(t1._8, t2.value._8),
          m8.append(t1._9, t2.value._9),
          m9.append(t1._10, t2.value._10),
          m10.append(t1._11, t2.value._11),
          m11.append(t1._12, t2.value._12),
          m12.append(t1._13, t2.value._13),
          m13.append(t1._14, t2.value._14),
          m14.append(t1._15, t2.value._15),
          m15.append(t1._16, t2.value._16),
          m16.append(t1._17, t2.value._17),
          m17.append(t1._18, t2.value._18)
        )
      }
    }

  implicit def tuple19Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
      implicit m0: Monoid[A1],
      m1: Monoid[A2],
      m2: Monoid[A3],
      m3: Monoid[A4],
      m4: Monoid[A5],
      m5: Monoid[A6],
      m6: Monoid[A7],
      m7: Monoid[A8],
      m8: Monoid[A9],
      m9: Monoid[A10],
      m10: Monoid[A11],
      m11: Monoid[A12],
      m12: Monoid[A13],
      m13: Monoid[A14],
      m14: Monoid[A15],
      m15: Monoid[A16],
      m16: Monoid[A17],
      m17: Monoid[A18],
      m18: Monoid[A19]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero, m14.zero, m15.zero, m16.zero, m17.zero, m18.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (
          m0.append(t1._1, t2.value._1),
          m1.append(t1._2, t2.value._2),
          m2.append(t1._3, t2.value._3),
          m3.append(t1._4, t2.value._4),
          m4.append(t1._5, t2.value._5),
          m5.append(t1._6, t2.value._6),
          m6.append(t1._7, t2.value._7),
          m7.append(t1._8, t2.value._8),
          m8.append(t1._9, t2.value._9),
          m9.append(t1._10, t2.value._10),
          m10.append(t1._11, t2.value._11),
          m11.append(t1._12, t2.value._12),
          m12.append(t1._13, t2.value._13),
          m13.append(t1._14, t2.value._14),
          m14.append(t1._15, t2.value._15),
          m15.append(t1._16, t2.value._16),
          m16.append(t1._17, t2.value._17),
          m17.append(t1._18, t2.value._18),
          m18.append(t1._19, t2.value._19)
        )
      }
    }

  implicit def tuple20Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
      implicit m0: Monoid[A1],
      m1: Monoid[A2],
      m2: Monoid[A3],
      m3: Monoid[A4],
      m4: Monoid[A5],
      m5: Monoid[A6],
      m6: Monoid[A7],
      m7: Monoid[A8],
      m8: Monoid[A9],
      m9: Monoid[A10],
      m10: Monoid[A11],
      m11: Monoid[A12],
      m12: Monoid[A13],
      m13: Monoid[A14],
      m14: Monoid[A15],
      m15: Monoid[A16],
      m16: Monoid[A17],
      m17: Monoid[A18],
      m18: Monoid[A19],
      m19: Monoid[A20]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)
      def zero: T =
        (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero, m14.zero, m15.zero, m16.zero, m17.zero, m18.zero, m19.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (
          m0.append(t1._1, t2.value._1),
          m1.append(t1._2, t2.value._2),
          m2.append(t1._3, t2.value._3),
          m3.append(t1._4, t2.value._4),
          m4.append(t1._5, t2.value._5),
          m5.append(t1._6, t2.value._6),
          m6.append(t1._7, t2.value._7),
          m7.append(t1._8, t2.value._8),
          m8.append(t1._9, t2.value._9),
          m9.append(t1._10, t2.value._10),
          m10.append(t1._11, t2.value._11),
          m11.append(t1._12, t2.value._12),
          m12.append(t1._13, t2.value._13),
          m13.append(t1._14, t2.value._14),
          m14.append(t1._15, t2.value._15),
          m15.append(t1._16, t2.value._16),
          m16.append(t1._17, t2.value._17),
          m17.append(t1._18, t2.value._18),
          m18.append(t1._19, t2.value._19),
          m19.append(t1._20, t2.value._20)
        )
      }
    }

  implicit def tuple21Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
      implicit m0: Monoid[A1],
      m1: Monoid[A2],
      m2: Monoid[A3],
      m3: Monoid[A4],
      m4: Monoid[A5],
      m5: Monoid[A6],
      m6: Monoid[A7],
      m7: Monoid[A8],
      m8: Monoid[A9],
      m9: Monoid[A10],
      m10: Monoid[A11],
      m11: Monoid[A12],
      m12: Monoid[A13],
      m13: Monoid[A14],
      m14: Monoid[A15],
      m15: Monoid[A16],
      m16: Monoid[A17],
      m17: Monoid[A18],
      m18: Monoid[A19],
      m19: Monoid[A20],
      m20: Monoid[A21]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)
      def zero: T =
        (
          m0.zero,
          m1.zero,
          m2.zero,
          m3.zero,
          m4.zero,
          m5.zero,
          m6.zero,
          m7.zero,
          m8.zero,
          m9.zero,
          m10.zero,
          m11.zero,
          m12.zero,
          m13.zero,
          m14.zero,
          m15.zero,
          m16.zero,
          m17.zero,
          m18.zero,
          m19.zero,
          m20.zero
        )
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (
          m0.append(t1._1, t2.value._1),
          m1.append(t1._2, t2.value._2),
          m2.append(t1._3, t2.value._3),
          m3.append(t1._4, t2.value._4),
          m4.append(t1._5, t2.value._5),
          m5.append(t1._6, t2.value._6),
          m6.append(t1._7, t2.value._7),
          m7.append(t1._8, t2.value._8),
          m8.append(t1._9, t2.value._9),
          m9.append(t1._10, t2.value._10),
          m10.append(t1._11, t2.value._11),
          m11.append(t1._12, t2.value._12),
          m12.append(t1._13, t2.value._13),
          m13.append(t1._14, t2.value._14),
          m14.append(t1._15, t2.value._15),
          m15.append(t1._16, t2.value._16),
          m16.append(t1._17, t2.value._17),
          m17.append(t1._18, t2.value._18),
          m18.append(t1._19, t2.value._19),
          m19.append(t1._20, t2.value._20),
          m20.append(t1._21, t2.value._21)
        )
      }
    }

  implicit def tuple22Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
      implicit m0: Monoid[A1],
      m1: Monoid[A2],
      m2: Monoid[A3],
      m3: Monoid[A4],
      m4: Monoid[A5],
      m5: Monoid[A6],
      m6: Monoid[A7],
      m7: Monoid[A8],
      m8: Monoid[A9],
      m9: Monoid[A10],
      m10: Monoid[A11],
      m11: Monoid[A12],
      m12: Monoid[A13],
      m13: Monoid[A14],
      m14: Monoid[A15],
      m15: Monoid[A16],
      m16: Monoid[A17],
      m17: Monoid[A18],
      m18: Monoid[A19],
      m19: Monoid[A20],
      m20: Monoid[A21],
      m21: Monoid[A22]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)
      def zero: T =
        (
          m0.zero,
          m1.zero,
          m2.zero,
          m3.zero,
          m4.zero,
          m5.zero,
          m6.zero,
          m7.zero,
          m8.zero,
          m9.zero,
          m10.zero,
          m11.zero,
          m12.zero,
          m13.zero,
          m14.zero,
          m15.zero,
          m16.zero,
          m17.zero,
          m18.zero,
          m19.zero,
          m20.zero,
          m21.zero
        )
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (
          m0.append(t1._1, t2.value._1),
          m1.append(t1._2, t2.value._2),
          m2.append(t1._3, t2.value._3),
          m3.append(t1._4, t2.value._4),
          m4.append(t1._5, t2.value._5),
          m5.append(t1._6, t2.value._6),
          m6.append(t1._7, t2.value._7),
          m7.append(t1._8, t2.value._8),
          m8.append(t1._9, t2.value._9),
          m9.append(t1._10, t2.value._10),
          m10.append(t1._11, t2.value._11),
          m11.append(t1._12, t2.value._12),
          m12.append(t1._13, t2.value._13),
          m13.append(t1._14, t2.value._14),
          m14.append(t1._15, t2.value._15),
          m15.append(t1._16, t2.value._16),
          m16.append(t1._17, t2.value._17),
          m17.append(t1._18, t2.value._18),
          m18.append(t1._19, t2.value._19),
          m19.append(t1._20, t2.value._20),
          m20.append(t1._21, t2.value._21),
          m21.append(t1._22, t2.value._22)
        )
      }
    }

}
