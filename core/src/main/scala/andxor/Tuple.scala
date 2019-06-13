package andxor

import scalaz.{Monoid, Need}

package object tuple {

  implicit def tuple9Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit m0: Monoid[A1], m1: Monoid[A2], m2: Monoid[A3], m3: Monoid[A4], m4: Monoid[A5], m5: Monoid[A6], m6: Monoid[A7], m7: Monoid[A8], m8: Monoid[A9]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (m0.append(t1.t1, t2.value.t1), m1.append(t1.t2, t2.value.t2), m2.append(t1.t3, t2.value.t3), m3.append(t1.t4, t2.value.t4), m4.append(t1.t5, t2.value.t5), m5.append(t1.t6, t2.value.t6), m6.append(t1.t7, t2.value.t7), m7.append(t1.t8, t2.value.t8), m8.append(t1.t9, t2.value.t9))
      }
    }

  implicit def tuple10Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit m0: Monoid[A1], m1: Monoid[A2], m2: Monoid[A3], m3: Monoid[A4], m4: Monoid[A5], m5: Monoid[A6], m6: Monoid[A7], m7: Monoid[A8], m8: Monoid[A9], m9: Monoid[A10]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (m0.append(t1.t1, t2.value.t1), m1.append(t1.t2, t2.value.t2), m2.append(t1.t3, t2.value.t3), m3.append(t1.t4, t2.value.t4), m4.append(t1.t5, t2.value.t5), m5.append(t1.t6, t2.value.t6), m6.append(t1.t7, t2.value.t7), m7.append(t1.t8, t2.value.t8), m8.append(t1.t9, t2.value.t9), m9.append(t1.t10, t2.value.t10))
      }
    }

  implicit def tuple11Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit m0: Monoid[A1], m1: Monoid[A2], m2: Monoid[A3], m3: Monoid[A4], m4: Monoid[A5], m5: Monoid[A6], m6: Monoid[A7], m7: Monoid[A8], m8: Monoid[A9], m9: Monoid[A10], m10: Monoid[A11]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (m0.append(t1.t1, t2.value.t1), m1.append(t1.t2, t2.value.t2), m2.append(t1.t3, t2.value.t3), m3.append(t1.t4, t2.value.t4), m4.append(t1.t5, t2.value.t5), m5.append(t1.t6, t2.value.t6), m6.append(t1.t7, t2.value.t7), m7.append(t1.t8, t2.value.t8), m8.append(t1.t9, t2.value.t9), m9.append(t1.t10, t2.value.t10), m10.append(t1.t11, t2.value.t11))
      }
    }

  implicit def tuple12Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit m0: Monoid[A1], m1: Monoid[A2], m2: Monoid[A3], m3: Monoid[A4], m4: Monoid[A5], m5: Monoid[A6], m6: Monoid[A7], m7: Monoid[A8], m8: Monoid[A9], m9: Monoid[A10], m10: Monoid[A11], m11: Monoid[A12]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (m0.append(t1.t1, t2.value.t1), m1.append(t1.t2, t2.value.t2), m2.append(t1.t3, t2.value.t3), m3.append(t1.t4, t2.value.t4), m4.append(t1.t5, t2.value.t5), m5.append(t1.t6, t2.value.t6), m6.append(t1.t7, t2.value.t7), m7.append(t1.t8, t2.value.t8), m8.append(t1.t9, t2.value.t9), m9.append(t1.t10, t2.value.t10), m10.append(t1.t11, t2.value.t11), m11.append(t1.t12, t2.value.t12))
      }
    }

  implicit def tuple13Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit m0: Monoid[A1], m1: Monoid[A2], m2: Monoid[A3], m3: Monoid[A4], m4: Monoid[A5], m5: Monoid[A6], m6: Monoid[A7], m7: Monoid[A8], m8: Monoid[A9], m9: Monoid[A10], m10: Monoid[A11], m11: Monoid[A12], m12: Monoid[A13]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (m0.append(t1.t1, t2.value.t1), m1.append(t1.t2, t2.value.t2), m2.append(t1.t3, t2.value.t3), m3.append(t1.t4, t2.value.t4), m4.append(t1.t5, t2.value.t5), m5.append(t1.t6, t2.value.t6), m6.append(t1.t7, t2.value.t7), m7.append(t1.t8, t2.value.t8), m8.append(t1.t9, t2.value.t9), m9.append(t1.t10, t2.value.t10), m10.append(t1.t11, t2.value.t11), m11.append(t1.t12, t2.value.t12), m12.append(t1.t13, t2.value.t13))
      }
    }

  implicit def tuple14Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit m0: Monoid[A1], m1: Monoid[A2], m2: Monoid[A3], m3: Monoid[A4], m4: Monoid[A5], m5: Monoid[A6], m6: Monoid[A7], m7: Monoid[A8], m8: Monoid[A9], m9: Monoid[A10], m10: Monoid[A11], m11: Monoid[A12], m12: Monoid[A13], m13: Monoid[A14]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (m0.append(t1.t1, t2.value.t1), m1.append(t1.t2, t2.value.t2), m2.append(t1.t3, t2.value.t3), m3.append(t1.t4, t2.value.t4), m4.append(t1.t5, t2.value.t5), m5.append(t1.t6, t2.value.t6), m6.append(t1.t7, t2.value.t7), m7.append(t1.t8, t2.value.t8), m8.append(t1.t9, t2.value.t9), m9.append(t1.t10, t2.value.t10), m10.append(t1.t11, t2.value.t11), m11.append(t1.t12, t2.value.t12), m12.append(t1.t13, t2.value.t13), m13.append(t1.t14, t2.value.t14))
      }
    }

  implicit def tuple15Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit m0: Monoid[A1], m1: Monoid[A2], m2: Monoid[A3], m3: Monoid[A4], m4: Monoid[A5], m5: Monoid[A6], m6: Monoid[A7], m7: Monoid[A8], m8: Monoid[A9], m9: Monoid[A10], m10: Monoid[A11], m11: Monoid[A12], m12: Monoid[A13], m13: Monoid[A14], m14: Monoid[A15]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero, m14.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (m0.append(t1.t1, t2.value.t1), m1.append(t1.t2, t2.value.t2), m2.append(t1.t3, t2.value.t3), m3.append(t1.t4, t2.value.t4), m4.append(t1.t5, t2.value.t5), m5.append(t1.t6, t2.value.t6), m6.append(t1.t7, t2.value.t7), m7.append(t1.t8, t2.value.t8), m8.append(t1.t9, t2.value.t9), m9.append(t1.t10, t2.value.t10), m10.append(t1.t11, t2.value.t11), m11.append(t1.t12, t2.value.t12), m12.append(t1.t13, t2.value.t13), m13.append(t1.t14, t2.value.t14), m14.append(t1.t15, t2.value.t15))
      }
    }

  implicit def tuple16Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit m0: Monoid[A1], m1: Monoid[A2], m2: Monoid[A3], m3: Monoid[A4], m4: Monoid[A5], m5: Monoid[A6], m6: Monoid[A7], m7: Monoid[A8], m8: Monoid[A9], m9: Monoid[A10], m10: Monoid[A11], m11: Monoid[A12], m12: Monoid[A13], m13: Monoid[A14], m14: Monoid[A15], m15: Monoid[A16]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero, m14.zero, m15.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (m0.append(t1.t1, t2.value.t1), m1.append(t1.t2, t2.value.t2), m2.append(t1.t3, t2.value.t3), m3.append(t1.t4, t2.value.t4), m4.append(t1.t5, t2.value.t5), m5.append(t1.t6, t2.value.t6), m6.append(t1.t7, t2.value.t7), m7.append(t1.t8, t2.value.t8), m8.append(t1.t9, t2.value.t9), m9.append(t1.t10, t2.value.t10), m10.append(t1.t11, t2.value.t11), m11.append(t1.t12, t2.value.t12), m12.append(t1.t13, t2.value.t13), m13.append(t1.t14, t2.value.t14), m14.append(t1.t15, t2.value.t15), m15.append(t1.t16, t2.value.t16))
      }
    }

  implicit def tuple17Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit m0: Monoid[A1], m1: Monoid[A2], m2: Monoid[A3], m3: Monoid[A4], m4: Monoid[A5], m5: Monoid[A6], m6: Monoid[A7], m7: Monoid[A8], m8: Monoid[A9], m9: Monoid[A10], m10: Monoid[A11], m11: Monoid[A12], m12: Monoid[A13], m13: Monoid[A14], m14: Monoid[A15], m15: Monoid[A16], m16: Monoid[A17]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero, m14.zero, m15.zero, m16.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (m0.append(t1.t1, t2.value.t1), m1.append(t1.t2, t2.value.t2), m2.append(t1.t3, t2.value.t3), m3.append(t1.t4, t2.value.t4), m4.append(t1.t5, t2.value.t5), m5.append(t1.t6, t2.value.t6), m6.append(t1.t7, t2.value.t7), m7.append(t1.t8, t2.value.t8), m8.append(t1.t9, t2.value.t9), m9.append(t1.t10, t2.value.t10), m10.append(t1.t11, t2.value.t11), m11.append(t1.t12, t2.value.t12), m12.append(t1.t13, t2.value.t13), m13.append(t1.t14, t2.value.t14), m14.append(t1.t15, t2.value.t15), m15.append(t1.t16, t2.value.t16), m16.append(t1.t17, t2.value.t17))
      }
    }

  implicit def tuple18Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit m0: Monoid[A1], m1: Monoid[A2], m2: Monoid[A3], m3: Monoid[A4], m4: Monoid[A5], m5: Monoid[A6], m6: Monoid[A7], m7: Monoid[A8], m8: Monoid[A9], m9: Monoid[A10], m10: Monoid[A11], m11: Monoid[A12], m12: Monoid[A13], m13: Monoid[A14], m14: Monoid[A15], m15: Monoid[A16], m16: Monoid[A17], m17: Monoid[A18]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero, m14.zero, m15.zero, m16.zero, m17.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (m0.append(t1.t1, t2.value.t1), m1.append(t1.t2, t2.value.t2), m2.append(t1.t3, t2.value.t3), m3.append(t1.t4, t2.value.t4), m4.append(t1.t5, t2.value.t5), m5.append(t1.t6, t2.value.t6), m6.append(t1.t7, t2.value.t7), m7.append(t1.t8, t2.value.t8), m8.append(t1.t9, t2.value.t9), m9.append(t1.t10, t2.value.t10), m10.append(t1.t11, t2.value.t11), m11.append(t1.t12, t2.value.t12), m12.append(t1.t13, t2.value.t13), m13.append(t1.t14, t2.value.t14), m14.append(t1.t15, t2.value.t15), m15.append(t1.t16, t2.value.t16), m16.append(t1.t17, t2.value.t17), m17.append(t1.t18, t2.value.t18))
      }
    }

  implicit def tuple19Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit m0: Monoid[A1], m1: Monoid[A2], m2: Monoid[A3], m3: Monoid[A4], m4: Monoid[A5], m5: Monoid[A6], m6: Monoid[A7], m7: Monoid[A8], m8: Monoid[A9], m9: Monoid[A10], m10: Monoid[A11], m11: Monoid[A12], m12: Monoid[A13], m13: Monoid[A14], m14: Monoid[A15], m15: Monoid[A16], m16: Monoid[A17], m17: Monoid[A18], m18: Monoid[A19]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero, m14.zero, m15.zero, m16.zero, m17.zero, m18.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (m0.append(t1.t1, t2.value.t1), m1.append(t1.t2, t2.value.t2), m2.append(t1.t3, t2.value.t3), m3.append(t1.t4, t2.value.t4), m4.append(t1.t5, t2.value.t5), m5.append(t1.t6, t2.value.t6), m6.append(t1.t7, t2.value.t7), m7.append(t1.t8, t2.value.t8), m8.append(t1.t9, t2.value.t9), m9.append(t1.t10, t2.value.t10), m10.append(t1.t11, t2.value.t11), m11.append(t1.t12, t2.value.t12), m12.append(t1.t13, t2.value.t13), m13.append(t1.t14, t2.value.t14), m14.append(t1.t15, t2.value.t15), m15.append(t1.t16, t2.value.t16), m16.append(t1.t17, t2.value.t17), m17.append(t1.t18, t2.value.t18), m18.append(t1.t19, t2.value.t19))
      }
    }

  implicit def tuple20Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit m0: Monoid[A1], m1: Monoid[A2], m2: Monoid[A3], m3: Monoid[A4], m4: Monoid[A5], m5: Monoid[A6], m6: Monoid[A7], m7: Monoid[A8], m8: Monoid[A9], m9: Monoid[A10], m10: Monoid[A11], m11: Monoid[A12], m12: Monoid[A13], m13: Monoid[A14], m14: Monoid[A15], m15: Monoid[A16], m16: Monoid[A17], m17: Monoid[A18], m18: Monoid[A19], m19: Monoid[A20]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero, m14.zero, m15.zero, m16.zero, m17.zero, m18.zero, m19.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (m0.append(t1.t1, t2.value.t1), m1.append(t1.t2, t2.value.t2), m2.append(t1.t3, t2.value.t3), m3.append(t1.t4, t2.value.t4), m4.append(t1.t5, t2.value.t5), m5.append(t1.t6, t2.value.t6), m6.append(t1.t7, t2.value.t7), m7.append(t1.t8, t2.value.t8), m8.append(t1.t9, t2.value.t9), m9.append(t1.t10, t2.value.t10), m10.append(t1.t11, t2.value.t11), m11.append(t1.t12, t2.value.t12), m12.append(t1.t13, t2.value.t13), m13.append(t1.t14, t2.value.t14), m14.append(t1.t15, t2.value.t15), m15.append(t1.t16, t2.value.t16), m16.append(t1.t17, t2.value.t17), m17.append(t1.t18, t2.value.t18), m18.append(t1.t19, t2.value.t19), m19.append(t1.t20, t2.value.t20))
      }
    }

  implicit def tuple21Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit m0: Monoid[A1], m1: Monoid[A2], m2: Monoid[A3], m3: Monoid[A4], m4: Monoid[A5], m5: Monoid[A6], m6: Monoid[A7], m7: Monoid[A8], m8: Monoid[A9], m9: Monoid[A10], m10: Monoid[A11], m11: Monoid[A12], m12: Monoid[A13], m13: Monoid[A14], m14: Monoid[A15], m15: Monoid[A16], m16: Monoid[A17], m17: Monoid[A18], m18: Monoid[A19], m19: Monoid[A20], m20: Monoid[A21]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero, m14.zero, m15.zero, m16.zero, m17.zero, m18.zero, m19.zero, m20.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (m0.append(t1.t1, t2.value.t1), m1.append(t1.t2, t2.value.t2), m2.append(t1.t3, t2.value.t3), m3.append(t1.t4, t2.value.t4), m4.append(t1.t5, t2.value.t5), m5.append(t1.t6, t2.value.t6), m6.append(t1.t7, t2.value.t7), m7.append(t1.t8, t2.value.t8), m8.append(t1.t9, t2.value.t9), m9.append(t1.t10, t2.value.t10), m10.append(t1.t11, t2.value.t11), m11.append(t1.t12, t2.value.t12), m12.append(t1.t13, t2.value.t13), m13.append(t1.t14, t2.value.t14), m14.append(t1.t15, t2.value.t15), m15.append(t1.t16, t2.value.t16), m16.append(t1.t17, t2.value.t17), m17.append(t1.t18, t2.value.t18), m18.append(t1.t19, t2.value.t19), m19.append(t1.t20, t2.value.t20), m20.append(t1.t21, t2.value.t21))
      }
    }

  implicit def tuple22Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit m0: Monoid[A1], m1: Monoid[A2], m2: Monoid[A3], m3: Monoid[A4], m4: Monoid[A5], m5: Monoid[A6], m6: Monoid[A7], m7: Monoid[A8], m8: Monoid[A9], m9: Monoid[A10], m10: Monoid[A11], m11: Monoid[A12], m12: Monoid[A13], m13: Monoid[A14], m14: Monoid[A15], m15: Monoid[A16], m16: Monoid[A17], m17: Monoid[A18], m18: Monoid[A19], m19: Monoid[A20], m20: Monoid[A21], m21: Monoid[A22]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] {
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)
      def zero: T = (m0.zero, m1.zero, m2.zero, m3.zero, m4.zero, m5.zero, m6.zero, m7.zero, m8.zero, m9.zero, m10.zero, m11.zero, m12.zero, m13.zero, m14.zero, m15.zero, m16.zero, m17.zero, m18.zero, m19.zero, m20.zero, m21.zero)
      def append(t1: T, _t2: => T): T = {
        val t2 = Need(_t2)
        (m0.append(t1.t1, t2.value.t1), m1.append(t1.t2, t2.value.t2), m2.append(t1.t3, t2.value.t3), m3.append(t1.t4, t2.value.t4), m4.append(t1.t5, t2.value.t5), m5.append(t1.t6, t2.value.t6), m6.append(t1.t7, t2.value.t7), m7.append(t1.t8, t2.value.t8), m8.append(t1.t9, t2.value.t9), m9.append(t1.t10, t2.value.t10), m10.append(t1.t11, t2.value.t11), m11.append(t1.t12, t2.value.t12), m12.append(t1.t13, t2.value.t13), m13.append(t1.t14, t2.value.t14), m14.append(t1.t15, t2.value.t15), m15.append(t1.t16, t2.value.t16), m16.append(t1.t17, t2.value.t17), m17.append(t1.t18, t2.value.t18), m18.append(t1.t19, t2.value.t19), m19.append(t1.t20, t2.value.t20), m20.append(t1.t21, t2.value.t21), m21.append(t1.t22, t2.value.t22))
      }
    }

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
