package andxor

import andxor.tuple._
import andxor.types.dummy._
import scalaz.\/

trait Map2P[A1, A2] {
  val mapN = this

  def map1[B](p: (A1, A2))(f: A1 => B): (B, A2) = {

    val a0 = p.t1
    val a1 = p.t2
    (f(a0), a1)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2)): (B, A2) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2))(f: A2 => B): (A1, B) = {

    val a0 = p.t1
    val a1 = p.t2
    (a0, f(a1))

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2))(implicit @scalaz.unused d: Dummy2): (A1, B) =
    mapN.map2(p)(f)

}

trait Map2C[A1, A2] {
  val mapN = this

  def map1[B](c: (A1 \/ A2))(f: A1 => B): (B \/ A2) =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: (A1 \/ A2)): (B \/ A2) =
    mapN.map1(c)(f)

  def map2[B](c: (A1 \/ A2))(f: A2 => B): (A1 \/ B) =

    c.map(f)

  def mapAt[B](f: A2 => B)(c: (A1 \/ A2))(implicit @scalaz.unused d: Dummy2): (A1 \/ B) =
    mapN.map2(c)(f)

}

trait Map3P[A1, A2, A3] {
  val mapN = this

  def map1[B](p: (A1, A2, A3))(f: A1 => B): (B, A2, A3) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    (f(a0), a1, a2)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3)): (B, A2, A3) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3))(f: A2 => B): (A1, B, A3) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    (a0, f(a1), a2)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3))(implicit @scalaz.unused d: Dummy2): (A1, B, A3) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3))(f: A3 => B): (A1, A2, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    (a0, a1, f(a2))

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3))(implicit @scalaz.unused d: Dummy3): (A1, A2, B) =
    mapN.map3(p)(f)

}

trait Map3C[A1, A2, A3] {
  val mapN = this

  def map1[B](c: (A1 \/ (A2 \/ A3)))(f: A1 => B): (B \/ (A2 \/ A3)) =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: (A1 \/ (A2 \/ A3))): (B \/ (A2 \/ A3)) =
    mapN.map1(c)(f)

  def map2[B](c: (A1 \/ (A2 \/ A3)))(f: A2 => B): (A1 \/ (B \/ A3)) =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: (A1 \/ (A2 \/ A3)))(implicit @scalaz.unused d: Dummy2): (A1 \/ (B \/ A3)) =
    mapN.map2(c)(f)

  def map3[B](c: (A1 \/ (A2 \/ A3)))(f: A3 => B): (A1 \/ (A2 \/ B)) =

    c.map(_.map(f))

  def mapAt[B](f: A3 => B)(c: (A1 \/ (A2 \/ A3)))(implicit @scalaz.unused d: Dummy3): (A1 \/ (A2 \/ B)) =
    mapN.map3(c)(f)

}

trait Map4P[A1, A2, A3, A4] {
  val mapN = this

  def map1[B](p: (A1, A2, A3, A4))(f: A1 => B): (B, A2, A3, A4) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    (f(a0), a1, a2, a3)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4)): (B, A2, A3, A4) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4))(f: A2 => B): (A1, B, A3, A4) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    (a0, f(a1), a2, a3)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4))(implicit @scalaz.unused d: Dummy2): (A1, B, A3, A4) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4))(f: A3 => B): (A1, A2, B, A4) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    (a0, a1, f(a2), a3)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4))(implicit @scalaz.unused d: Dummy3): (A1, A2, B, A4) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4))(f: A4 => B): (A1, A2, A3, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    (a0, a1, a2, f(a3))

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4))(implicit @scalaz.unused d: Dummy4): (A1, A2, A3, B) =
    mapN.map4(p)(f)

}

trait Map4C[A1, A2, A3, A4] {
  val mapN = this

  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ A4))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ A4))) =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: (A1 \/ (A2 \/ (A3 \/ A4)))): (B \/ (A2 \/ (A3 \/ A4))) =
    mapN.map1(c)(f)

  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ A4))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ A4))) =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: (A1 \/ (A2 \/ (A3 \/ A4))))(implicit @scalaz.unused d: Dummy2): (A1 \/ (B \/ (A3 \/ A4))) =
    mapN.map2(c)(f)

  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ A4))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ A4))) =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: (A1 \/ (A2 \/ (A3 \/ A4))))(implicit @scalaz.unused d: Dummy3): (A1 \/ (A2 \/ (B \/ A4))) =
    mapN.map3(c)(f)

  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ A4))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ B))) =

    c.map(_.map(_.map(f)))

  def mapAt[B](f: A4 => B)(c: (A1 \/ (A2 \/ (A3 \/ A4))))(implicit @scalaz.unused d: Dummy4): (A1 \/ (A2 \/ (A3 \/ B))) =
    mapN.map4(c)(f)

}

object MapN {
  object syntax {

    implicit class Map2POps[A1, A2](p: (A1, A2)) {
      val mapN = new Map2P[A1, A2] {}

      def map1[B](f: A1 => B): (B, A2) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @scalaz.unused d: Dummy2): (A1, B) =
        mapN.map2(p)(f)

    }

    implicit class Map2COps[A1, A2](c: (A1 \/ A2)) {
      val mapN = new Map2C[A1, A2] {}

      def map1[B](f: A1 => B): (B \/ A2) =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): (B \/ A2) =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): (A1 \/ B) =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @scalaz.unused d: Dummy2): (A1 \/ B) =
        mapN.map2(c)(f)

    }

    implicit class Map3POps[A1, A2, A3](p: (A1, A2, A3)) {
      val mapN = new Map3P[A1, A2, A3] {}

      def map1[B](f: A1 => B): (B, A2, A3) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @scalaz.unused d: Dummy2): (A1, B, A3) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @scalaz.unused d: Dummy3): (A1, A2, B) =
        mapN.map3(p)(f)

    }

    implicit class Map3COps[A1, A2, A3](c: (A1 \/ (A2 \/ A3))) {
      val mapN = new Map3C[A1, A2, A3] {}

      def map1[B](f: A1 => B): (B \/ (A2 \/ A3)) =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): (B \/ (A2 \/ A3)) =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): (A1 \/ (B \/ A3)) =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @scalaz.unused d: Dummy2): (A1 \/ (B \/ A3)) =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): (A1 \/ (A2 \/ B)) =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @scalaz.unused d: Dummy3): (A1 \/ (A2 \/ B)) =
        mapN.map3(c)(f)

    }

    implicit class Map4POps[A1, A2, A3, A4](p: (A1, A2, A3, A4)) {
      val mapN = new Map4P[A1, A2, A3, A4] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @scalaz.unused d: Dummy2): (A1, B, A3, A4) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @scalaz.unused d: Dummy3): (A1, A2, B, A4) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @scalaz.unused d: Dummy4): (A1, A2, A3, B) =
        mapN.map4(p)(f)

    }

    implicit class Map4COps[A1, A2, A3, A4](c: (A1 \/ (A2 \/ (A3 \/ A4)))) {
      val mapN = new Map4C[A1, A2, A3, A4] {}

      def map1[B](f: A1 => B): (B \/ (A2 \/ (A3 \/ A4))) =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): (B \/ (A2 \/ (A3 \/ A4))) =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): (A1 \/ (B \/ (A3 \/ A4))) =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @scalaz.unused d: Dummy2): (A1 \/ (B \/ (A3 \/ A4))) =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): (A1 \/ (A2 \/ (B \/ A4))) =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @scalaz.unused d: Dummy3): (A1 \/ (A2 \/ (B \/ A4))) =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): (A1 \/ (A2 \/ (A3 \/ B))) =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @scalaz.unused d: Dummy4): (A1 \/ (A2 \/ (A3 \/ B))) =
        mapN.map4(c)(f)

    }

  }
}
