package andxor

import scalaz.Monoid

package object tuple {

  implicit def tuple9Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit ml: Monoid[(A1, A2, A3, A4, A5, A6, A7)], mr: Monoid[(A8, A9)]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] {
      type L = (A1, A2, A3, A4, A5, A6, A7)
      type R = (A8, A9)
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9)
      def left(t: T): L = (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
      def right(t: T): R = (t._8, t._9)
      def flatten(l: L, r: R): T =
        (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r._1, r._2)
      def zero: T = flatten(ml.zero, mr.zero)
      def append(t1: T, t2: => T): T = flatten(ml.append(left(t1), left(t2)), mr.append(right(t1), right(t2)))
    }

  implicit def tuple10Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit ml: Monoid[(A1, A2, A3, A4, A5, A6, A7)], mr: Monoid[(A8, A9, A10)]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] {
      type L = (A1, A2, A3, A4, A5, A6, A7)
      type R = (A8, A9, A10)
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)
      def left(t: T): L = (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
      def right(t: T): R = (t._8, t._9, t._10)
      def flatten(l: L, r: R): T =
        (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r._1, r._2, r._3)
      def zero: T = flatten(ml.zero, mr.zero)
      def append(t1: T, t2: => T): T = flatten(ml.append(left(t1), left(t2)), mr.append(right(t1), right(t2)))
    }

  implicit def tuple11Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
      implicit ml: Monoid[(A1, A2, A3, A4, A5, A6, A7)],
      mr: Monoid[(A8, A9, A10, A11)]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] = new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] {
    type L = (A1, A2, A3, A4, A5, A6, A7)
    type R = (A8, A9, A10, A11)
    type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)
    def left(t: T): L = (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
    def right(t: T): R = (t._8, t._9, t._10, t._11)
    def flatten(l: L, r: R): T =
      (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r._1, r._2, r._3, r._4)
    def zero: T = flatten(ml.zero, mr.zero)
    def append(t1: T, t2: => T): T = flatten(ml.append(left(t1), left(t2)), mr.append(right(t1), right(t2)))
  }

  implicit def tuple12Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      implicit ml: Monoid[(A1, A2, A3, A4, A5, A6, A7)],
      mr: Monoid[(A8, A9, A10, A11, A12)]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] = new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] {
    type L = (A1, A2, A3, A4, A5, A6, A7)
    type R = (A8, A9, A10, A11, A12)
    type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)
    def left(t: T): L = (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
    def right(t: T): R = (t._8, t._9, t._10, t._11, t._12)
    def flatten(l: L, r: R): T =
      (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r._1, r._2, r._3, r._4, r._5)
    def zero: T = flatten(ml.zero, mr.zero)
    def append(t1: T, t2: => T): T = flatten(ml.append(left(t1), left(t2)), mr.append(right(t1), right(t2)))
  }

  implicit def tuple13Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
      implicit ml: Monoid[(A1, A2, A3, A4, A5, A6, A7)],
      mr: Monoid[(A8, A9, A10, A11, A12, A13)]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] = new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] {
    type L = (A1, A2, A3, A4, A5, A6, A7)
    type R = (A8, A9, A10, A11, A12, A13)
    type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)
    def left(t: T): L = (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
    def right(t: T): R = (t._8, t._9, t._10, t._11, t._12, t._13)
    def flatten(l: L, r: R): T =
      (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r._1, r._2, r._3, r._4, r._5, r._6)
    def zero: T = flatten(ml.zero, mr.zero)
    def append(t1: T, t2: => T): T = flatten(ml.append(left(t1), left(t2)), mr.append(right(t1), right(t2)))
  }

  implicit def tuple14Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
      implicit ml: Monoid[(A1, A2, A3, A4, A5, A6, A7)],
      mr: Monoid[(A8, A9, A10, A11, A12, A13, A14)]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] = new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] {
    type L = (A1, A2, A3, A4, A5, A6, A7)
    type R = (A8, A9, A10, A11, A12, A13, A14)
    type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)
    def left(t: T): L = (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
    def right(t: T): R = (t._8, t._9, t._10, t._11, t._12, t._13, t._14)
    def flatten(l: L, r: R): T =
      (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r._1, r._2, r._3, r._4, r._5, r._6, r._7)
    def zero: T = flatten(ml.zero, mr.zero)
    def append(t1: T, t2: => T): T = flatten(ml.append(left(t1), left(t2)), mr.append(right(t1), right(t2)))
  }

  implicit def tuple15Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
      implicit ml: Monoid[(A1, A2, A3, A4, A5, A6, A7)],
      mr: Monoid[(A8, A9, A10, A11, A12, A13, A14, A15)]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] = new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] {
    type L = (A1, A2, A3, A4, A5, A6, A7)
    type R = (A8, A9, A10, A11, A12, A13, A14, A15)
    type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)
    def left(t: T): L = (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
    def right(t: T): R = (t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)
    def flatten(l: L, r: R): T =
      (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r._1, r._2, r._3, r._4, r._5, r._6, r._7, r._8)
    def zero: T = flatten(ml.zero, mr.zero)
    def append(t1: T, t2: => T): T = flatten(ml.append(left(t1), left(t2)), mr.append(right(t1), right(t2)))
  }

  implicit def tuple16Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
      implicit ml: Monoid[(A1, A2, A3, A4, A5, A6, A7)],
      mr: Monoid[(A8, A9, A10, A11, A12, A13, A14, A15, A16)]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] = new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] {
    type L = (A1, A2, A3, A4, A5, A6, A7)
    type R = (A8, A9, A10, A11, A12, A13, A14, A15, A16)
    type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)
    def left(t: T): L = (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
    def right(t: T): R = (t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)
    def flatten(l: L, r: R): T =
      (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r._1, r._2, r._3, r._4, r._5, r._6, r._7, r._8, r._9)
    def zero: T = flatten(ml.zero, mr.zero)
    def append(t1: T, t2: => T): T = flatten(ml.append(left(t1), left(t2)), mr.append(right(t1), right(t2)))
  }

  implicit def tuple17Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
      implicit ml: Monoid[(A1, A2, A3, A4, A5, A6, A7)],
      mr: Monoid[(A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] = new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] {
    type L = (A1, A2, A3, A4, A5, A6, A7)
    type R = (A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)
    type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)
    def left(t: T): L = (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
    def right(t: T): R = (t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
    def flatten(l: L, r: R): T =
      (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r._1, r._2, r._3, r._4, r._5, r._6, r._7, r._8, r._9, r._10)
    def zero: T = flatten(ml.zero, mr.zero)
    def append(t1: T, t2: => T): T = flatten(ml.append(left(t1), left(t2)), mr.append(right(t1), right(t2)))
  }

  implicit def tuple18Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
      implicit ml: Monoid[(A1, A2, A3, A4, A5, A6, A7)],
      mr: Monoid[(A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] = new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] {
    type L = (A1, A2, A3, A4, A5, A6, A7)
    type R = (A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)
    type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)
    def left(t: T): L = (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
    def right(t: T): R = (t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
    def flatten(l: L, r: R): T =
      (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r._1, r._2, r._3, r._4, r._5, r._6, r._7, r._8, r._9, r._10, r._11)
    def zero: T = flatten(ml.zero, mr.zero)
    def append(t1: T, t2: => T): T = flatten(ml.append(left(t1), left(t2)), mr.append(right(t1), right(t2)))
  }

  implicit def tuple19Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
      implicit ml: Monoid[(A1, A2, A3, A4, A5, A6, A7)],
      mr: Monoid[(A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] {
      type L = (A1, A2, A3, A4, A5, A6, A7)
      type R = (A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)
      def left(t: T): L = (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
      def right(t: T): R = (t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
      def flatten(l: L, r: R): T =
        (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r._1, r._2, r._3, r._4, r._5, r._6, r._7, r._8, r._9, r._10, r._11, r._12)
      def zero: T = flatten(ml.zero, mr.zero)
      def append(t1: T, t2: => T): T = flatten(ml.append(left(t1), left(t2)), mr.append(right(t1), right(t2)))
    }

  implicit def tuple20Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
      implicit ml: Monoid[(A1, A2, A3, A4, A5, A6, A7)],
      mr: Monoid[(A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] {
      type L = (A1, A2, A3, A4, A5, A6, A7)
      type R = (A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)
      def left(t: T): L = (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
      def right(t: T): R = (t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
      def flatten(l: L, r: R): T =
        (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r._1, r._2, r._3, r._4, r._5, r._6, r._7, r._8, r._9, r._10, r._11, r._12, r._13)
      def zero: T = flatten(ml.zero, mr.zero)
      def append(t1: T, t2: => T): T = flatten(ml.append(left(t1), left(t2)), mr.append(right(t1), right(t2)))
    }

  implicit def tuple21Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
      implicit ml: Monoid[(A1, A2, A3, A4, A5, A6, A7)],
      mr: Monoid[(A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] {
      type L = (A1, A2, A3, A4, A5, A6, A7)
      type R = (A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)
      def left(t: T): L = (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
      def right(t: T): R = (t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
      def flatten(l: L, r: R): T =
        (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r._1, r._2, r._3, r._4, r._5, r._6, r._7, r._8, r._9, r._10, r._11, r._12, r._13, r._14)
      def zero: T = flatten(ml.zero, mr.zero)
      def append(t1: T, t2: => T): T = flatten(ml.append(left(t1), left(t2)), mr.append(right(t1), right(t2)))
    }

  implicit def tuple22Monoid[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
      implicit ml: Monoid[(A1, A2, A3, A4, A5, A6, A7)],
      mr: Monoid[(A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)]
  ): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] =
    new Monoid[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] {
      type L = (A1, A2, A3, A4, A5, A6, A7)
      type R = (A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)
      type T = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)
      def left(t: T): L = (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
      def right(t: T): R = (t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
      def flatten(l: L, r: R): T =
        (l._1, l._2, l._3, l._4, l._5, l._6, l._7, r._1, r._2, r._3, r._4, r._5, r._6, r._7, r._8, r._9, r._10, r._11, r._12, r._13, r._14, r._15)
      def zero: T = flatten(ml.zero, mr.zero)
      def append(t1: T, t2: => T): T = flatten(ml.append(left(t1), left(t2)), mr.append(right(t1), right(t2)))
    }

}
