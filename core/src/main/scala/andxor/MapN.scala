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

  }
}
