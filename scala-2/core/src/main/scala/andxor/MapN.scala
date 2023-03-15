package andxor

import andxor.tuple._
import andxor.types._
import cats.syntax.either._

trait Map2P[A1, A2] {
  private val mapN = this

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

  def mapAt[B](f: A2 => B)(p: (A1, A2))(implicit @unused d: Dummy2): (A1, B) =
    mapN.map2(p)(f)

}

trait Map2C[A1, A2] {
  private val mapN = this

  def map1[B](c: Either[A1, A2])(f: A1 => B): Either[B, A2] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, A2]): Either[B, A2] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, A2])(f: A2 => B): Either[A1, B] =

    c.map(f)

  def mapAt[B](f: A2 => B)(c: Either[A1, A2])(implicit @unused d: Dummy2): Either[A1, B] =
    mapN.map2(c)(f)

}

trait Map3P[A1, A2, A3] {
  private val mapN = this

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

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3))(implicit @unused d: Dummy2): (A1, B, A3) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3))(f: A3 => B): (A1, A2, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    (a0, a1, f(a2))

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3))(implicit @unused d: Dummy3): (A1, A2, B) =
    mapN.map3(p)(f)

}

trait Map3C[A1, A2, A3] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, A3]])(f: A1 => B): Either[B, Either[A2, A3]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, A3]]): Either[B, Either[A2, A3]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, A3]])(f: A2 => B): Either[A1, Either[B, A3]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, A3]])(implicit @unused d: Dummy2): Either[A1, Either[B, A3]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, A3]])(f: A3 => B): Either[A1, Either[A2, B]] =

    c.map(_.map(f))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, A3]])(implicit @unused d: Dummy3): Either[A1, Either[A2, B]] =
    mapN.map3(c)(f)

}

trait Map4P[A1, A2, A3, A4] {
  private val mapN = this

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

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4))(implicit @unused d: Dummy2): (A1, B, A3, A4) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4))(f: A3 => B): (A1, A2, B, A4) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    (a0, a1, f(a2), a3)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4))(implicit @unused d: Dummy3): (A1, A2, B, A4) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4))(f: A4 => B): (A1, A2, A3, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    (a0, a1, a2, f(a3))

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4))(implicit @unused d: Dummy4): (A1, A2, A3, B) =
    mapN.map4(p)(f)

}

trait Map4C[A1, A2, A3, A4] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, A4]]])(f: A1 => B): Either[B, Either[A2, Either[A3, A4]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, A4]]]): Either[B, Either[A2, Either[A3, A4]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, A4]]])(f: A2 => B): Either[A1, Either[B, Either[A3, A4]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, A4]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, A4]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, A4]]])(f: A3 => B): Either[A1, Either[A2, Either[B, A4]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, A4]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, A4]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, A4]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, B]]] =

    c.map(_.map(_.map(f)))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, A4]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, B]]] =
    mapN.map4(c)(f)

}

trait Map5P[A1, A2, A3, A4, A5] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5))(f: A1 => B): (B, A2, A3, A4, A5) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    (f(a0), a1, a2, a3, a4)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5)): (B, A2, A3, A4, A5) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5))(f: A2 => B): (A1, B, A3, A4, A5) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    (a0, f(a1), a2, a3, a4)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5))(f: A3 => B): (A1, A2, B, A4, A5) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    (a0, a1, f(a2), a3, a4)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5))(f: A4 => B): (A1, A2, A3, B, A5) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    (a0, a1, a2, f(a3), a4)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5))(f: A5 => B): (A1, A2, A3, A4, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    (a0, a1, a2, a3, f(a4))

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B) =
    mapN.map5(p)(f)

}

trait Map5C[A1, A2, A3, A4, A5] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, A5]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, A5]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, A5]]]]): Either[B, Either[A2, Either[A3, Either[A4, A5]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, A5]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, A5]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, A5]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, A5]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, A5]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, A5]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, A5]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, A5]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, A5]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, A5]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, A5]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, A5]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, A5]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, B]]]] =

    c.map(_.map(_.map(_.map(f))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, A5]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, B]]]] =
    mapN.map5(c)(f)

}

trait Map6P[A1, A2, A3, A4, A5, A6] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6))(f: A1 => B): (B, A2, A3, A4, A5, A6) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    (f(a0), a1, a2, a3, a4, a5)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6)): (B, A2, A3, A4, A5, A6) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6))(f: A2 => B): (A1, B, A3, A4, A5, A6) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    (a0, f(a1), a2, a3, a4, a5)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6))(f: A3 => B): (A1, A2, B, A4, A5, A6) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    (a0, a1, f(a2), a3, a4, a5)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6))(f: A4 => B): (A1, A2, A3, B, A5, A6) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    (a0, a1, a2, f(a3), a4, a5)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6))(f: A5 => B): (A1, A2, A3, A4, B, A6) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    (a0, a1, a2, a3, f(a4), a5)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6))(f: A6 => B): (A1, A2, A3, A4, A5, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    (a0, a1, a2, a3, a4, f(a5))

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B) =
    mapN.map6(p)(f)

}

trait Map6C[A1, A2, A3, A4, A5, A6] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, A6]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, A6]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, A6]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, A6]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, A6]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, A6]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, A6]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, A6]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, B]]]]] =

    c.map(_.map(_.map(_.map(_.map(f)))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, B]]]]] =
    mapN.map6(c)(f)

}

trait Map7P[A1, A2, A3, A4, A5, A6, A7] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    (f(a0), a1, a2, a3, a4, a5, a6)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7)): (B, A2, A3, A4, A5, A6, A7) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    (a0, f(a1), a2, a3, a4, a5, a6)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    (a0, a1, f(a2), a3, a4, a5, a6)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    (a0, a1, a2, f(a3), a4, a5, a6)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    (a0, a1, a2, a3, f(a4), a5, a6)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    (a0, a1, a2, a3, a4, f(a5), a6)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    (a0, a1, a2, a3, a4, a5, f(a6))

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B) =
    mapN.map7(p)(f)

}

trait Map7C[A1, A2, A3, A4, A5, A6, A7] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, A7]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, A7]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, A7]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, A7]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, A7]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, A7]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, A7]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, A7]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, B]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(f))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, B]]]]]] =
    mapN.map7(c)(f)

}

trait Map8P[A1, A2, A3, A4, A5, A6, A7, A8] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7, A8))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (f(a0), a1, a2, a3, a4, a5, a6, a7)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8)): (B, A2, A3, A4, A5, A6, A7, A8) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7, A8))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (a0, f(a1), a2, a3, a4, a5, a6, a7)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7, A8))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (a0, a1, f(a2), a3, a4, a5, a6, a7)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7, A8))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (a0, a1, a2, f(a3), a4, a5, a6, a7)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7, A8))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (a0, a1, a2, a3, f(a4), a5, a6, a7)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7, A8))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (a0, a1, a2, a3, a4, f(a5), a6, a7)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7, A8))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (a0, a1, a2, a3, a4, a5, f(a6), a7)

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8) =
    mapN.map7(p)(f)

  def map8[B](p: (A1, A2, A3, A4, A5, A6, A7, A8))(f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (a0, a1, a2, a3, a4, a5, a6, f(a7))

  }

  def mapAt[B](f: A8 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8))(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B) =
    mapN.map8(p)(f)

}

trait Map8C[A1, A2, A3, A4, A5, A6, A7, A8] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, A8]]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, A8]]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, A8]]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, A8]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, A8]]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, A8]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, A8]]]]]]] =
    mapN.map7(c)(f)

  def map8[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]])(f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, B]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))

  def mapAt[B](f: A8 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]])(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, B]]]]]]] =
    mapN.map8(c)(f)

}

trait Map9P[A1, A2, A3, A4, A5, A6, A7, A8, A9] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (f(a0), a1, a2, a3, a4, a5, a6, a7, a8)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9)): (B, A2, A3, A4, A5, A6, A7, A8, A9) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, f(a1), a2, a3, a4, a5, a6, a7, a8)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, a1, f(a2), a3, a4, a5, a6, a7, a8)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, a1, a2, f(a3), a4, a5, a6, a7, a8)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, a1, a2, a3, f(a4), a5, a6, a7, a8)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, a1, a2, a3, a4, f(a5), a6, a7, a8)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, a1, a2, a3, a4, a5, f(a6), a7, a8)

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9) =
    mapN.map7(p)(f)

  def map8[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, a1, a2, a3, a4, a5, a6, f(a7), a8)

  }

  def mapAt[B](f: A8 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9) =
    mapN.map8(p)(f)

  def map9[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, a1, a2, a3, a4, a5, a6, a7, f(a8))

  }

  def mapAt[B](f: A9 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9))(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B) =
    mapN.map9(p)(f)

}

trait Map9C[A1, A2, A3, A4, A5, A6, A7, A8, A9] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, A9]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, A9]]]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, A9]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, A9]]]]]]]] =
    mapN.map7(c)(f)

  def map8[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, A9]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

  def mapAt[B](f: A8 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, A9]]]]]]]] =
    mapN.map8(c)(f)

  def map9[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, B]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))

  def mapAt[B](f: A9 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]])(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, B]]]]]]]] =
    mapN.map9(c)(f)

}

trait Map10P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9)

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10) =
    mapN.map7(p)(f)

  def map8[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9)

  }

  def mapAt[B](f: A8 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10) =
    mapN.map8(p)(f)

  def map9[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9)

  }

  def mapAt[B](f: A9 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10) =
    mapN.map9(p)(f)

  def map10[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9))

  }

  def mapAt[B](f: A10 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B) =
    mapN.map10(p)(f)

}

trait Map10C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, A10]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, A10]]]]]]]]] =
    mapN.map7(c)(f)

  def map8[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, A10]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

  def mapAt[B](f: A8 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, A10]]]]]]]]] =
    mapN.map8(c)(f)

  def map9[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, A10]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

  def mapAt[B](f: A9 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, A10]]]]]]]]] =
    mapN.map9(c)(f)

  def map10[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, B]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))

  def mapAt[B](f: A10 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]])(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, B]]]]]]]]] =
    mapN.map10(c)(f)

}

trait Map11P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10)

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11) =
    mapN.map7(p)(f)

  def map8[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10)

  }

  def mapAt[B](f: A8 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11) =
    mapN.map8(p)(f)

  def map9[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10)

  }

  def mapAt[B](f: A9 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11) =
    mapN.map9(p)(f)

  def map10[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10)

  }

  def mapAt[B](f: A10 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11) =
    mapN.map10(p)(f)

  def map11[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10))

  }

  def mapAt[B](f: A11 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B) =
    mapN.map11(p)(f)

}

trait Map11C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
    mapN.map7(c)(f)

  def map8[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, A11]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

  def mapAt[B](f: A8 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, A11]]]]]]]]]] =
    mapN.map8(c)(f)

  def map9[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, A11]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

  def mapAt[B](f: A9 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, A11]]]]]]]]]] =
    mapN.map9(c)(f)

  def map10[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, A11]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

  def mapAt[B](f: A10 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, A11]]]]]]]]]] =
    mapN.map10(c)(f)

  def map11[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, B]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))

  def mapAt[B](f: A11 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]])(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, B]]]]]]]]]] =
    mapN.map11(c)(f)

}

trait Map12P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11)

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12) =
    mapN.map7(p)(f)

  def map8[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11)

  }

  def mapAt[B](f: A8 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12) =
    mapN.map8(p)(f)

  def map9[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11)

  }

  def mapAt[B](f: A9 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12) =
    mapN.map9(p)(f)

  def map10[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11)

  }

  def mapAt[B](f: A10 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12) =
    mapN.map10(p)(f)

  def map11[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11)

  }

  def mapAt[B](f: A11 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12) =
    mapN.map11(p)(f)

  def map12[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11))

  }

  def mapAt[B](f: A12 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B) =
    mapN.map12(p)(f)

}

trait Map12C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
    mapN.map7(c)(f)

  def map8[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

  def mapAt[B](f: A8 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
    mapN.map8(c)(f)

  def map9[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, A12]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

  def mapAt[B](f: A9 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, A12]]]]]]]]]]] =
    mapN.map9(c)(f)

  def map10[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, A12]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

  def mapAt[B](f: A10 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, A12]]]]]]]]]]] =
    mapN.map10(c)(f)

  def map11[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, A12]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

  def mapAt[B](f: A11 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, A12]]]]]]]]]]] =
    mapN.map11(c)(f)

  def map12[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, B]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))

  def mapAt[B](f: A12 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]])(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, B]]]]]]]]]]] =
    mapN.map12(c)(f)

}

trait Map13P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12)

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13) =
    mapN.map7(p)(f)

  def map8[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12)

  }

  def mapAt[B](f: A8 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13) =
    mapN.map8(p)(f)

  def map9[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12)

  }

  def mapAt[B](f: A9 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13) =
    mapN.map9(p)(f)

  def map10[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12)

  }

  def mapAt[B](f: A10 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13) =
    mapN.map10(p)(f)

  def map11[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12)

  }

  def mapAt[B](f: A11 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13) =
    mapN.map11(p)(f)

  def map12[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12)

  }

  def mapAt[B](f: A12 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13) =
    mapN.map12(p)(f)

  def map13[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12))

  }

  def mapAt[B](f: A13 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B) =
    mapN.map13(p)(f)

}

trait Map13C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
    mapN.map7(c)(f)

  def map8[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

  def mapAt[B](f: A8 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
    mapN.map8(c)(f)

  def map9[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

  def mapAt[B](f: A9 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
    mapN.map9(c)(f)

  def map10[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, A13]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

  def mapAt[B](f: A10 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, A13]]]]]]]]]]]] =
    mapN.map10(c)(f)

  def map11[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, A13]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

  def mapAt[B](f: A11 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, A13]]]]]]]]]]]] =
    mapN.map11(c)(f)

  def map12[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, A13]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

  def mapAt[B](f: A12 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, A13]]]]]]]]]]]] =
    mapN.map12(c)(f)

  def map13[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, B]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))

  def mapAt[B](f: A13 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]])(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, B]]]]]]]]]]]] =
    mapN.map13(c)(f)

}

trait Map14P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13)

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14) =
    mapN.map7(p)(f)

  def map8[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13)

  }

  def mapAt[B](f: A8 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14) =
    mapN.map8(p)(f)

  def map9[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13)

  }

  def mapAt[B](f: A9 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14) =
    mapN.map9(p)(f)

  def map10[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13)

  }

  def mapAt[B](f: A10 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14) =
    mapN.map10(p)(f)

  def map11[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13)

  }

  def mapAt[B](f: A11 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14) =
    mapN.map11(p)(f)

  def map12[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13)

  }

  def mapAt[B](f: A12 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14) =
    mapN.map12(p)(f)

  def map13[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13)

  }

  def mapAt[B](f: A13 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14) =
    mapN.map13(p)(f)

  def map14[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13))

  }

  def mapAt[B](f: A14 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B) =
    mapN.map14(p)(f)

}

trait Map14C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
    mapN.map7(c)(f)

  def map8[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

  def mapAt[B](f: A8 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
    mapN.map8(c)(f)

  def map9[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

  def mapAt[B](f: A9 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
    mapN.map9(c)(f)

  def map10[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

  def mapAt[B](f: A10 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
    mapN.map10(c)(f)

  def map11[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, A14]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

  def mapAt[B](f: A11 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
    mapN.map11(c)(f)

  def map12[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, A14]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

  def mapAt[B](f: A12 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, A14]]]]]]]]]]]]] =
    mapN.map12(c)(f)

  def map13[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, A14]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

  def mapAt[B](f: A13 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, A14]]]]]]]]]]]]] =
    mapN.map13(c)(f)

  def map14[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, B]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))

  def mapAt[B](f: A14 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]])(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, B]]]]]]]]]]]]] =
    mapN.map14(c)(f)

}

trait Map15P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14)

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15) =
    mapN.map7(p)(f)

  def map8[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14)

  }

  def mapAt[B](f: A8 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15) =
    mapN.map8(p)(f)

  def map9[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14)

  }

  def mapAt[B](f: A9 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15) =
    mapN.map9(p)(f)

  def map10[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14)

  }

  def mapAt[B](f: A10 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15) =
    mapN.map10(p)(f)

  def map11[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14)

  }

  def mapAt[B](f: A11 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15) =
    mapN.map11(p)(f)

  def map12[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14)

  }

  def mapAt[B](f: A12 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15) =
    mapN.map12(p)(f)

  def map13[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14)

  }

  def mapAt[B](f: A13 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15) =
    mapN.map13(p)(f)

  def map14[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14)

  }

  def mapAt[B](f: A14 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15) =
    mapN.map14(p)(f)

  def map15[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14))

  }

  def mapAt[B](f: A15 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B) =
    mapN.map15(p)(f)

}

trait Map15C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
    mapN.map7(c)(f)

  def map8[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

  def mapAt[B](f: A8 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
    mapN.map8(c)(f)

  def map9[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

  def mapAt[B](f: A9 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
    mapN.map9(c)(f)

  def map10[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

  def mapAt[B](f: A10 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
    mapN.map10(c)(f)

  def map11[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

  def mapAt[B](f: A11 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
    mapN.map11(c)(f)

  def map12[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

  def mapAt[B](f: A12 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
    mapN.map12(c)(f)

  def map13[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, A15]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

  def mapAt[B](f: A13 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, A15]]]]]]]]]]]]]] =
    mapN.map13(c)(f)

  def map14[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, A15]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

  def mapAt[B](f: A14 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, A15]]]]]]]]]]]]]] =
    mapN.map14(c)(f)

  def map15[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, B]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))

  def mapAt[B](f: A15 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]])(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, B]]]]]]]]]]]]]] =
    mapN.map15(c)(f)

}

trait Map16P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14, a15)

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
    mapN.map7(p)(f)

  def map8[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14, a15)

  }

  def mapAt[B](f: A8 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16) =
    mapN.map8(p)(f)

  def map9[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14, a15)

  }

  def mapAt[B](f: A9 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16) =
    mapN.map9(p)(f)

  def map10[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14, a15)

  }

  def mapAt[B](f: A10 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16) =
    mapN.map10(p)(f)

  def map11[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14, a15)

  }

  def mapAt[B](f: A11 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16) =
    mapN.map11(p)(f)

  def map12[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14, a15)

  }

  def mapAt[B](f: A12 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16) =
    mapN.map12(p)(f)

  def map13[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14, a15)

  }

  def mapAt[B](f: A13 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16) =
    mapN.map13(p)(f)

  def map14[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14, a15)

  }

  def mapAt[B](f: A14 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16) =
    mapN.map14(p)(f)

  def map15[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14), a15)

  }

  def mapAt[B](f: A15 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16) =
    mapN.map15(p)(f)

  def map16[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, f(a15))

  }

  def mapAt[B](f: A16 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit @unused d: Dummy16): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B) =
    mapN.map16(p)(f)

}

trait Map16C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
    mapN.map7(c)(f)

  def map8[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

  def mapAt[B](f: A8 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
    mapN.map8(c)(f)

  def map9[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

  def mapAt[B](f: A9 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
    mapN.map9(c)(f)

  def map10[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

  def mapAt[B](f: A10 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
    mapN.map10(c)(f)

  def map11[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

  def mapAt[B](f: A11 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
    mapN.map11(c)(f)

  def map12[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

  def mapAt[B](f: A12 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
    mapN.map12(c)(f)

  def map13[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

  def mapAt[B](f: A13 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
    mapN.map13(c)(f)

  def map14[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, A16]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

  def mapAt[B](f: A14 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, A16]]]]]]]]]]]]]]] =
    mapN.map14(c)(f)

  def map15[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, A16]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))

  def mapAt[B](f: A15 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, A16]]]]]]]]]]]]]]] =
    mapN.map15(c)(f)

  def map16[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, B]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))

  def mapAt[B](f: A16 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]])(implicit @unused d: Dummy16): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, B]]]]]]]]]]]]]]] =
    mapN.map16(c)(f)

}

trait Map17P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
    mapN.map7(p)(f)

  def map8[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14, a15, a16)

  }

  def mapAt[B](f: A8 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
    mapN.map8(p)(f)

  def map9[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14, a15, a16)

  }

  def mapAt[B](f: A9 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17) =
    mapN.map9(p)(f)

  def map10[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14, a15, a16)

  }

  def mapAt[B](f: A10 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17) =
    mapN.map10(p)(f)

  def map11[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14, a15, a16)

  }

  def mapAt[B](f: A11 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17) =
    mapN.map11(p)(f)

  def map12[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14, a15, a16)

  }

  def mapAt[B](f: A12 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17) =
    mapN.map12(p)(f)

  def map13[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14, a15, a16)

  }

  def mapAt[B](f: A13 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17) =
    mapN.map13(p)(f)

  def map14[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14, a15, a16)

  }

  def mapAt[B](f: A14 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17) =
    mapN.map14(p)(f)

  def map15[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14), a15, a16)

  }

  def mapAt[B](f: A15 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17) =
    mapN.map15(p)(f)

  def map16[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, f(a15), a16)

  }

  def mapAt[B](f: A16 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy16): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17) =
    mapN.map16(p)(f)

  def map17[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, f(a16))

  }

  def mapAt[B](f: A17 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit @unused d: Dummy17): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B) =
    mapN.map17(p)(f)

}

trait Map17C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
    mapN.map7(c)(f)

  def map8[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

  def mapAt[B](f: A8 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
    mapN.map8(c)(f)

  def map9[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

  def mapAt[B](f: A9 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
    mapN.map9(c)(f)

  def map10[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

  def mapAt[B](f: A10 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
    mapN.map10(c)(f)

  def map11[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

  def mapAt[B](f: A11 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
    mapN.map11(c)(f)

  def map12[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

  def mapAt[B](f: A12 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
    mapN.map12(c)(f)

  def map13[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

  def mapAt[B](f: A13 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
    mapN.map13(c)(f)

  def map14[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

  def mapAt[B](f: A14 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
    mapN.map14(c)(f)

  def map15[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, A17]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))

  def mapAt[B](f: A15 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, A17]]]]]]]]]]]]]]]] =
    mapN.map15(c)(f)

  def map16[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, A17]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))

  def mapAt[B](f: A16 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy16): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, A17]]]]]]]]]]]]]]]] =
    mapN.map16(c)(f)

  def map17[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, B]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))))

  def mapAt[B](f: A17 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]])(implicit @unused d: Dummy17): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, B]]]]]]]]]]]]]]]] =
    mapN.map17(c)(f)

}

trait Map18P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
    mapN.map7(p)(f)

  def map8[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

  }

  def mapAt[B](f: A8 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
    mapN.map8(p)(f)

  def map9[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14, a15, a16, a17)

  }

  def mapAt[B](f: A9 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
    mapN.map9(p)(f)

  def map10[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14, a15, a16, a17)

  }

  def mapAt[B](f: A10 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18) =
    mapN.map10(p)(f)

  def map11[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14, a15, a16, a17)

  }

  def mapAt[B](f: A11 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18) =
    mapN.map11(p)(f)

  def map12[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14, a15, a16, a17)

  }

  def mapAt[B](f: A12 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18) =
    mapN.map12(p)(f)

  def map13[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14, a15, a16, a17)

  }

  def mapAt[B](f: A13 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18) =
    mapN.map13(p)(f)

  def map14[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14, a15, a16, a17)

  }

  def mapAt[B](f: A14 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18) =
    mapN.map14(p)(f)

  def map15[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14), a15, a16, a17)

  }

  def mapAt[B](f: A15 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18) =
    mapN.map15(p)(f)

  def map16[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, f(a15), a16, a17)

  }

  def mapAt[B](f: A16 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy16): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18) =
    mapN.map16(p)(f)

  def map17[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, f(a16), a17)

  }

  def mapAt[B](f: A17 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy17): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18) =
    mapN.map17(p)(f)

  def map18[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(f: A18 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, f(a17))

  }

  def mapAt[B](f: A18 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit @unused d: Dummy18): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B) =
    mapN.map18(p)(f)

}

trait Map18C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map7(c)(f)

  def map8[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

  def mapAt[B](f: A8 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map8(c)(f)

  def map9[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

  def mapAt[B](f: A9 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map9(c)(f)

  def map10[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

  def mapAt[B](f: A10 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map10(c)(f)

  def map11[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

  def mapAt[B](f: A11 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map11(c)(f)

  def map12[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

  def mapAt[B](f: A12 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map12(c)(f)

  def map13[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

  def mapAt[B](f: A13 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map13(c)(f)

  def map14[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

  def mapAt[B](f: A14 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map14(c)(f)

  def map15[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))

  def mapAt[B](f: A15 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map15(c)(f)

  def map16[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, A18]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))

  def mapAt[B](f: A16 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy16): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, A18]]]]]]]]]]]]]]]]] =
    mapN.map16(c)(f)

  def map17[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, A18]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))

  def mapAt[B](f: A17 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy17): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, A18]]]]]]]]]]]]]]]]] =
    mapN.map17(c)(f)

  def map18[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(f: A18 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, B]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))))

  def mapAt[B](f: A18 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy18): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, B]]]]]]]]]]]]]]]]] =
    mapN.map18(c)(f)

}

trait Map19P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
    mapN.map7(p)(f)

  def map8[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

  }

  def mapAt[B](f: A8 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
    mapN.map8(p)(f)

  def map9[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

  }

  def mapAt[B](f: A9 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
    mapN.map9(p)(f)

  def map10[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14, a15, a16, a17, a18)

  }

  def mapAt[B](f: A10 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
    mapN.map10(p)(f)

  def map11[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14, a15, a16, a17, a18)

  }

  def mapAt[B](f: A11 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19) =
    mapN.map11(p)(f)

  def map12[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14, a15, a16, a17, a18)

  }

  def mapAt[B](f: A12 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19) =
    mapN.map12(p)(f)

  def map13[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14, a15, a16, a17, a18)

  }

  def mapAt[B](f: A13 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19) =
    mapN.map13(p)(f)

  def map14[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14, a15, a16, a17, a18)

  }

  def mapAt[B](f: A14 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19) =
    mapN.map14(p)(f)

  def map15[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14), a15, a16, a17, a18)

  }

  def mapAt[B](f: A15 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19) =
    mapN.map15(p)(f)

  def map16[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, f(a15), a16, a17, a18)

  }

  def mapAt[B](f: A16 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy16): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19) =
    mapN.map16(p)(f)

  def map17[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, f(a16), a17, a18)

  }

  def mapAt[B](f: A17 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy17): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19) =
    mapN.map17(p)(f)

  def map18[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A18 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, f(a17), a18)

  }

  def mapAt[B](f: A18 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy18): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19) =
    mapN.map18(p)(f)

  def map19[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(f: A19 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, f(a18))

  }

  def mapAt[B](f: A19 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit @unused d: Dummy19): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B) =
    mapN.map19(p)(f)

}

trait Map19C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map7(c)(f)

  def map8[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

  def mapAt[B](f: A8 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map8(c)(f)

  def map9[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

  def mapAt[B](f: A9 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map9(c)(f)

  def map10[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

  def mapAt[B](f: A10 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map10(c)(f)

  def map11[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

  def mapAt[B](f: A11 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map11(c)(f)

  def map12[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

  def mapAt[B](f: A12 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map12(c)(f)

  def map13[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

  def mapAt[B](f: A13 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map13(c)(f)

  def map14[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

  def mapAt[B](f: A14 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map14(c)(f)

  def map15[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))

  def mapAt[B](f: A15 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map15(c)(f)

  def map16[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))

  def mapAt[B](f: A16 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy16): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map16(c)(f)

  def map17[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))

  def mapAt[B](f: A17 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy17): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, A19]]]]]]]]]]]]]]]]]] =
    mapN.map17(c)(f)

  def map18[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A18 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, A19]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))

  def mapAt[B](f: A18 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy18): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, A19]]]]]]]]]]]]]]]]]] =
    mapN.map18(c)(f)

  def map19[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(f: A19 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, B]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))))))

  def mapAt[B](f: A19 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy19): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, B]]]]]]]]]]]]]]]]]] =
    mapN.map19(c)(f)

}

trait Map20P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
    mapN.map7(p)(f)

  def map8[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

  }

  def mapAt[B](f: A8 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
    mapN.map8(p)(f)

  def map9[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

  }

  def mapAt[B](f: A9 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
    mapN.map9(p)(f)

  def map10[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

  }

  def mapAt[B](f: A10 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
    mapN.map10(p)(f)

  def map11[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14, a15, a16, a17, a18, a19)

  }

  def mapAt[B](f: A11 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
    mapN.map11(p)(f)

  def map12[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14, a15, a16, a17, a18, a19)

  }

  def mapAt[B](f: A12 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19, A20) =
    mapN.map12(p)(f)

  def map13[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14, a15, a16, a17, a18, a19)

  }

  def mapAt[B](f: A13 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19, A20) =
    mapN.map13(p)(f)

  def map14[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14, a15, a16, a17, a18, a19)

  }

  def mapAt[B](f: A14 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19, A20) =
    mapN.map14(p)(f)

  def map15[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14), a15, a16, a17, a18, a19)

  }

  def mapAt[B](f: A15 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19, A20) =
    mapN.map15(p)(f)

  def map16[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, f(a15), a16, a17, a18, a19)

  }

  def mapAt[B](f: A16 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy16): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19, A20) =
    mapN.map16(p)(f)

  def map17[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, f(a16), a17, a18, a19)

  }

  def mapAt[B](f: A17 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy17): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19, A20) =
    mapN.map17(p)(f)

  def map18[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A18 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, f(a17), a18, a19)

  }

  def mapAt[B](f: A18 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy18): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19, A20) =
    mapN.map18(p)(f)

  def map19[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A19 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B, A20) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, f(a18), a19)

  }

  def mapAt[B](f: A19 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy19): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B, A20) =
    mapN.map19(p)(f)

  def map20[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(f: A20 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, f(a19))

  }

  def mapAt[B](f: A20 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit @unused d: Dummy20): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B) =
    mapN.map20(p)(f)

}

trait Map20C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map7(c)(f)

  def map8[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

  def mapAt[B](f: A8 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map8(c)(f)

  def map9[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

  def mapAt[B](f: A9 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map9(c)(f)

  def map10[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

  def mapAt[B](f: A10 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map10(c)(f)

  def map11[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

  def mapAt[B](f: A11 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map11(c)(f)

  def map12[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

  def mapAt[B](f: A12 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map12(c)(f)

  def map13[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

  def mapAt[B](f: A13 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map13(c)(f)

  def map14[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

  def mapAt[B](f: A14 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map14(c)(f)

  def map15[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))

  def mapAt[B](f: A15 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map15(c)(f)

  def map16[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))

  def mapAt[B](f: A16 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy16): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map16(c)(f)

  def map17[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))

  def mapAt[B](f: A17 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy17): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map17(c)(f)

  def map18[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A18 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, Either[A19, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))

  def mapAt[B](f: A18 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy18): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map18(c)(f)

  def map19[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A19 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[B, A20]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))

  def mapAt[B](f: A19 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy19): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[B, A20]]]]]]]]]]]]]]]]]]] =
    mapN.map19(c)(f)

  def map20[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(f: A20 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, B]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))))))

  def mapAt[B](f: A20 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy20): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, B]]]]]]]]]]]]]]]]]]] =
    mapN.map20(c)(f)

}

trait Map21P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
    mapN.map7(p)(f)

  def map8[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A8 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
    mapN.map8(p)(f)

  def map9[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A9 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
    mapN.map9(p)(f)

  def map10[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A10 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
    mapN.map10(p)(f)

  def map11[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A11 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
    mapN.map11(p)(f)

  def map12[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14, a15, a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A12 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
    mapN.map12(p)(f)

  def map13[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14, a15, a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A13 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19, A20, A21) =
    mapN.map13(p)(f)

  def map14[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14, a15, a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A14 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19, A20, A21) =
    mapN.map14(p)(f)

  def map15[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14), a15, a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A15 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19, A20, A21) =
    mapN.map15(p)(f)

  def map16[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, f(a15), a16, a17, a18, a19, a20)

  }

  def mapAt[B](f: A16 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy16): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19, A20, A21) =
    mapN.map16(p)(f)

  def map17[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, f(a16), a17, a18, a19, a20)

  }

  def mapAt[B](f: A17 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy17): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19, A20, A21) =
    mapN.map17(p)(f)

  def map18[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A18 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, f(a17), a18, a19, a20)

  }

  def mapAt[B](f: A18 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy18): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19, A20, A21) =
    mapN.map18(p)(f)

  def map19[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A19 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B, A20, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, f(a18), a19, a20)

  }

  def mapAt[B](f: A19 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy19): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B, A20, A21) =
    mapN.map19(p)(f)

  def map20[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A20 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B, A21) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, f(a19), a20)

  }

  def mapAt[B](f: A20 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy20): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B, A21) =
    mapN.map20(p)(f)

  def map21[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(f: A21 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, f(a20))

  }

  def mapAt[B](f: A21 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit @unused d: Dummy21): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B) =
    mapN.map21(p)(f)

}

trait Map21C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map7(c)(f)

  def map8[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

  def mapAt[B](f: A8 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map8(c)(f)

  def map9[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

  def mapAt[B](f: A9 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map9(c)(f)

  def map10[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

  def mapAt[B](f: A10 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map10(c)(f)

  def map11[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

  def mapAt[B](f: A11 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map11(c)(f)

  def map12[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

  def mapAt[B](f: A12 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map12(c)(f)

  def map13[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

  def mapAt[B](f: A13 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map13(c)(f)

  def map14[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

  def mapAt[B](f: A14 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map14(c)(f)

  def map15[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))

  def mapAt[B](f: A15 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map15(c)(f)

  def map16[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))

  def mapAt[B](f: A16 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy16): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map16(c)(f)

  def map17[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))

  def mapAt[B](f: A17 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy17): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map17(c)(f)

  def map18[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A18 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))

  def mapAt[B](f: A18 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy18): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map18(c)(f)

  def map19[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A19 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[B, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))

  def mapAt[B](f: A19 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy19): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[B, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map19(c)(f)

  def map20[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A20 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[B, A21]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))

  def mapAt[B](f: A20 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy20): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[B, A21]]]]]]]]]]]]]]]]]]]] =
    mapN.map20(c)(f)

  def map21[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(f: A21 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, B]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))))))))

  def mapAt[B](f: A21 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy21): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, B]]]]]]]]]]]]]]]]]]]] =
    mapN.map21(c)(f)

}

trait Map22P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {
  private val mapN = this

  def map1[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (f(a0), a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A1 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
    mapN.map1(p)(f)

  def map2[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, f(a1), a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A2 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
    mapN.map2(p)(f)

  def map3[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, f(a2), a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A3 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
    mapN.map3(p)(f)

  def map4[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, f(a3), a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A4 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
    mapN.map4(p)(f)

  def map5[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, f(a4), a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A5 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
    mapN.map5(p)(f)

  def map6[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, f(a5), a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A6 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
    mapN.map6(p)(f)

  def map7[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, f(a6), a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A7 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
    mapN.map7(p)(f)

  def map8[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, a6, f(a7), a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A8 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
    mapN.map8(p)(f)

  def map9[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, a6, a7, f(a8), a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A9 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
    mapN.map9(p)(f)

  def map10[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, f(a9), a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A10 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
    mapN.map10(p)(f)

  def map11[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, f(a10), a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A11 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
    mapN.map11(p)(f)

  def map12[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, f(a11), a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A12 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
    mapN.map12(p)(f)

  def map13[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, f(a12), a13, a14, a15, a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A13 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
    mapN.map13(p)(f)

  def map14[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, f(a13), a14, a15, a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A14 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19, A20, A21, A22) =
    mapN.map14(p)(f)

  def map15[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, f(a14), a15, a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A15 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19, A20, A21, A22) =
    mapN.map15(p)(f)

  def map16[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, f(a15), a16, a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A16 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy16): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19, A20, A21, A22) =
    mapN.map16(p)(f)

  def map17[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, f(a16), a17, a18, a19, a20, a21)

  }

  def mapAt[B](f: A17 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy17): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19, A20, A21, A22) =
    mapN.map17(p)(f)

  def map18[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A18 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, f(a17), a18, a19, a20, a21)

  }

  def mapAt[B](f: A18 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy18): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19, A20, A21, A22) =
    mapN.map18(p)(f)

  def map19[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A19 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B, A20, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, f(a18), a19, a20, a21)

  }

  def mapAt[B](f: A19 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy19): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B, A20, A21, A22) =
    mapN.map19(p)(f)

  def map20[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A20 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B, A21, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, f(a19), a20, a21)

  }

  def mapAt[B](f: A20 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy20): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B, A21, A22) =
    mapN.map20(p)(f)

  def map21[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A21 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B, A22) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, f(a20), a21)

  }

  def mapAt[B](f: A21 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy21): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B, A22) =
    mapN.map21(p)(f)

  def map22[B](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(f: A22 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, B) = {

    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    val a9 = p.t10
    val a10 = p.t11
    val a11 = p.t12
    val a12 = p.t13
    val a13 = p.t14
    val a14 = p.t15
    val a15 = p.t16
    val a16 = p.t17
    val a17 = p.t18
    val a18 = p.t19
    val a19 = p.t20
    val a20 = p.t21
    val a21 = p.t22
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, f(a21))

  }

  def mapAt[B](f: A22 => B)(p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit @unused d: Dummy22): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, B) =
    mapN.map22(p)(f)

}

trait Map22C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {
  private val mapN = this

  def map1[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.leftMap(f)

  def mapAt[B](f: A1 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]]): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map1(c)(f)

  def map2[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.leftMap(f))

  def mapAt[B](f: A2 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map2(c)(f)

  def map3[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.leftMap(f)))

  def mapAt[B](f: A3 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map3(c)(f)

  def map4[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.leftMap(f))))

  def mapAt[B](f: A4 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map4(c)(f)

  def map5[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.leftMap(f)))))

  def mapAt[B](f: A5 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map5(c)(f)

  def map6[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

  def mapAt[B](f: A6 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map6(c)(f)

  def map7[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

  def mapAt[B](f: A7 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map7(c)(f)

  def map8[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

  def mapAt[B](f: A8 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map8(c)(f)

  def map9[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

  def mapAt[B](f: A9 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map9(c)(f)

  def map10[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

  def mapAt[B](f: A10 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map10(c)(f)

  def map11[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

  def mapAt[B](f: A11 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map11(c)(f)

  def map12[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

  def mapAt[B](f: A12 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map12(c)(f)

  def map13[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

  def mapAt[B](f: A13 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map13(c)(f)

  def map14[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

  def mapAt[B](f: A14 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map14(c)(f)

  def map15[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))

  def mapAt[B](f: A15 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map15(c)(f)

  def map16[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))

  def mapAt[B](f: A16 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy16): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map16(c)(f)

  def map17[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))

  def mapAt[B](f: A17 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy17): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map17(c)(f)

  def map18[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A18 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))

  def mapAt[B](f: A18 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy18): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map18(c)(f)

  def map19[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A19 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[B, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))

  def mapAt[B](f: A19 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy19): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[B, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map19(c)(f)

  def map20[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A20 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[B, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))

  def mapAt[B](f: A20 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy20): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[B, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map20(c)(f)

  def map21[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A21 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[B, A22]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))

  def mapAt[B](f: A21 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy21): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[B, A22]]]]]]]]]]]]]]]]]]]]] =
    mapN.map21(c)(f)

  def map22[B](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(f: A22 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, B]]]]]]]]]]]]]]]]]]]]] =

    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))))))))

  def mapAt[B](f: A22 => B)(c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]])(implicit @unused d: Dummy22): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, B]]]]]]]]]]]]]]]]]]]]] =
    mapN.map22(c)(f)

}

object MapN {
  object syntax {

    implicit class Map2POps[A1, A2](p: (A1, A2)) {
      private val mapN = new Map2P[A1, A2] {}

      def map1[B](f: A1 => B): (B, A2) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B) =
        mapN.map2(p)(f)

    }

    implicit class Map2COps[A1, A2](c: Either[A1, A2]) {
      private val mapN = new Map2C[A1, A2] {}

      def map1[B](f: A1 => B): Either[B, A2] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, A2] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, B] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, B] =
        mapN.map2(c)(f)

    }

    implicit class Map3POps[A1, A2, A3](p: (A1, A2, A3)) {
      private val mapN = new Map3P[A1, A2, A3] {}

      def map1[B](f: A1 => B): (B, A2, A3) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B) =
        mapN.map3(p)(f)

    }

    implicit class Map3COps[A1, A2, A3](c: Either[A1, Either[A2, A3]]) {
      private val mapN = new Map3C[A1, A2, A3] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, A3]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, A3]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, A3]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, A3]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, B]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, B]] =
        mapN.map3(c)(f)

    }

    implicit class Map4POps[A1, A2, A3, A4](p: (A1, A2, A3, A4)) {
      private val mapN = new Map4P[A1, A2, A3, A4] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B) =
        mapN.map4(p)(f)

    }

    implicit class Map4COps[A1, A2, A3, A4](c: Either[A1, Either[A2, Either[A3, A4]]]) {
      private val mapN = new Map4C[A1, A2, A3, A4] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, A4]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, A4]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, A4]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, A4]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, A4]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, A4]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, B]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, B]]] =
        mapN.map4(c)(f)

    }

    implicit class Map5POps[A1, A2, A3, A4, A5](p: (A1, A2, A3, A4, A5)) {
      private val mapN = new Map5P[A1, A2, A3, A4, A5] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B) =
        mapN.map5(p)(f)

    }

    implicit class Map5COps[A1, A2, A3, A4, A5](c: Either[A1, Either[A2, Either[A3, Either[A4, A5]]]]) {
      private val mapN = new Map5C[A1, A2, A3, A4, A5] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, A5]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, A5]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, A5]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, A5]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, A5]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, A5]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, A5]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, A5]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, B]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, B]]]] =
        mapN.map5(c)(f)

    }

    implicit class Map6POps[A1, A2, A3, A4, A5, A6](p: (A1, A2, A3, A4, A5, A6)) {
      private val mapN = new Map6P[A1, A2, A3, A4, A5, A6] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B) =
        mapN.map6(p)(f)

    }

    implicit class Map6COps[A1, A2, A3, A4, A5, A6](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]]) {
      private val mapN = new Map6C[A1, A2, A3, A4, A5, A6] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, A6]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, A6]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, A6]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, A6]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, A6]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, A6]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, A6]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, A6]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, B]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, B]]]]] =
        mapN.map6(c)(f)

    }

    implicit class Map7POps[A1, A2, A3, A4, A5, A6, A7](p: (A1, A2, A3, A4, A5, A6, A7)) {
      private val mapN = new Map7P[A1, A2, A3, A4, A5, A6, A7] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B) =
        mapN.map7(p)(f)

    }

    implicit class Map7COps[A1, A2, A3, A4, A5, A6, A7](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]]) {
      private val mapN = new Map7C[A1, A2, A3, A4, A5, A6, A7] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, A7]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, A7]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, A7]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, A7]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, A7]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, A7]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, A7]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, A7]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, B]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, B]]]]]] =
        mapN.map7(c)(f)

    }

    implicit class Map8POps[A1, A2, A3, A4, A5, A6, A7, A8](p: (A1, A2, A3, A4, A5, A6, A7, A8)) {
      private val mapN = new Map8P[A1, A2, A3, A4, A5, A6, A7, A8] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8) =
        mapN.map7(p)(f)

      def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B) =
        mapN.map8(p)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B) =
        mapN.map8(p)(f)

    }

    implicit class Map8COps[A1, A2, A3, A4, A5, A6, A7, A8](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]]) {
      private val mapN = new Map8C[A1, A2, A3, A4, A5, A6, A7, A8] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, A8]]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, A8]]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, A8]]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, A8]]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, A8]]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, A8]]]]]]] =
        mapN.map7(c)(f)

      def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, B]]]]]]] =
        mapN.map8(c)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, B]]]]]]] =
        mapN.map8(c)(f)

    }

    implicit class Map9POps[A1, A2, A3, A4, A5, A6, A7, A8, A9](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9)) {
      private val mapN = new Map9P[A1, A2, A3, A4, A5, A6, A7, A8, A9] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9) =
        mapN.map7(p)(f)

      def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9) =
        mapN.map8(p)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9) =
        mapN.map8(p)(f)

      def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B) =
        mapN.map9(p)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B) =
        mapN.map9(p)(f)

    }

    implicit class Map9COps[A1, A2, A3, A4, A5, A6, A7, A8, A9](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]]) {
      private val mapN = new Map9C[A1, A2, A3, A4, A5, A6, A7, A8, A9] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, A9]]]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, A9]]]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, A9]]]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, A9]]]]]]]] =
        mapN.map7(c)(f)

      def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, A9]]]]]]]] =
        mapN.map8(c)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, A9]]]]]]]] =
        mapN.map8(c)(f)

      def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, B]]]]]]]] =
        mapN.map9(c)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, B]]]]]]]] =
        mapN.map9(c)(f)

    }

    implicit class Map10POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) {
      private val mapN = new Map10P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10) =
        mapN.map7(p)(f)

      def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10) =
        mapN.map8(p)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10) =
        mapN.map8(p)(f)

      def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10) =
        mapN.map9(p)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10) =
        mapN.map9(p)(f)

      def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B) =
        mapN.map10(p)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B) =
        mapN.map10(p)(f)

    }

    implicit class Map10COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]]) {
      private val mapN = new Map10C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, A10]]]]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, A10]]]]]]]]] =
        mapN.map7(c)(f)

      def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, A10]]]]]]]]] =
        mapN.map8(c)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, A10]]]]]]]]] =
        mapN.map8(c)(f)

      def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, A10]]]]]]]]] =
        mapN.map9(c)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, A10]]]]]]]]] =
        mapN.map9(c)(f)

      def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, B]]]]]]]]] =
        mapN.map10(c)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, B]]]]]]]]] =
        mapN.map10(c)(f)

    }

    implicit class Map11POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)) {
      private val mapN = new Map11P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11) =
        mapN.map7(p)(f)

      def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11) =
        mapN.map8(p)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11) =
        mapN.map8(p)(f)

      def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11) =
        mapN.map9(p)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11) =
        mapN.map9(p)(f)

      def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11) =
        mapN.map10(p)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11) =
        mapN.map10(p)(f)

      def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B) =
        mapN.map11(p)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B) =
        mapN.map11(p)(f)

    }

    implicit class Map11COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]]) {
      private val mapN = new Map11C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map7(c)(f)

      def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map8(c)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, A11]]]]]]]]]] =
        mapN.map8(c)(f)

      def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, A11]]]]]]]]]] =
        mapN.map9(c)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, A11]]]]]]]]]] =
        mapN.map9(c)(f)

      def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, A11]]]]]]]]]] =
        mapN.map10(c)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, A11]]]]]]]]]] =
        mapN.map10(c)(f)

      def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, B]]]]]]]]]] =
        mapN.map11(c)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, B]]]]]]]]]] =
        mapN.map11(c)(f)

    }

    implicit class Map12POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)) {
      private val mapN = new Map12P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12) =
        mapN.map7(p)(f)

      def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12) =
        mapN.map8(p)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12) =
        mapN.map8(p)(f)

      def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12) =
        mapN.map9(p)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12) =
        mapN.map9(p)(f)

      def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12) =
        mapN.map10(p)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12) =
        mapN.map10(p)(f)

      def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12) =
        mapN.map11(p)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12) =
        mapN.map11(p)(f)

      def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B) =
        mapN.map12(p)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B) =
        mapN.map12(p)(f)

    }

    implicit class Map12COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]]) {
      private val mapN = new Map12C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map7(c)(f)

      def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map8(c)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map8(c)(f)

      def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map9(c)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, A12]]]]]]]]]]] =
        mapN.map9(c)(f)

      def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, A12]]]]]]]]]]] =
        mapN.map10(c)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, A12]]]]]]]]]]] =
        mapN.map10(c)(f)

      def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, A12]]]]]]]]]]] =
        mapN.map11(c)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, A12]]]]]]]]]]] =
        mapN.map11(c)(f)

      def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, B]]]]]]]]]]] =
        mapN.map12(c)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, B]]]]]]]]]]] =
        mapN.map12(c)(f)

    }

    implicit class Map13POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)) {
      private val mapN = new Map13P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13) =
        mapN.map7(p)(f)

      def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13) =
        mapN.map8(p)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13) =
        mapN.map8(p)(f)

      def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13) =
        mapN.map9(p)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13) =
        mapN.map9(p)(f)

      def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13) =
        mapN.map10(p)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13) =
        mapN.map10(p)(f)

      def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13) =
        mapN.map11(p)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13) =
        mapN.map11(p)(f)

      def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13) =
        mapN.map12(p)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13) =
        mapN.map12(p)(f)

      def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B) =
        mapN.map13(p)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B) =
        mapN.map13(p)(f)

    }

    implicit class Map13COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]]) {
      private val mapN = new Map13C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, A13]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, A13]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, A13]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, A13]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, A13]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, B]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, B]]]]]]]]]]]] =
        mapN.map13(c)(f)

    }

    implicit class Map14POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)) {
      private val mapN = new Map14P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14) =
        mapN.map7(p)(f)

      def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14) =
        mapN.map8(p)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14) =
        mapN.map8(p)(f)

      def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14) =
        mapN.map9(p)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14) =
        mapN.map9(p)(f)

      def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14) =
        mapN.map10(p)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14) =
        mapN.map10(p)(f)

      def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14) =
        mapN.map11(p)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14) =
        mapN.map11(p)(f)

      def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14) =
        mapN.map12(p)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14) =
        mapN.map12(p)(f)

      def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14) =
        mapN.map13(p)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14) =
        mapN.map13(p)(f)

      def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B) =
        mapN.map14(p)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B) =
        mapN.map14(p)(f)

    }

    implicit class Map14COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]]) {
      private val mapN = new Map14C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, A14]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, A14]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, A14]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, B]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, B]]]]]]]]]]]]] =
        mapN.map14(c)(f)

    }

    implicit class Map15POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)) {
      private val mapN = new Map15P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map7(p)(f)

      def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map8(p)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15) =
        mapN.map8(p)(f)

      def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15) =
        mapN.map9(p)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15) =
        mapN.map9(p)(f)

      def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15) =
        mapN.map10(p)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15) =
        mapN.map10(p)(f)

      def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15) =
        mapN.map11(p)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15) =
        mapN.map11(p)(f)

      def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15) =
        mapN.map12(p)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15) =
        mapN.map12(p)(f)

      def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15) =
        mapN.map13(p)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15) =
        mapN.map13(p)(f)

      def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15) =
        mapN.map14(p)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15) =
        mapN.map14(p)(f)

      def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B) =
        mapN.map15(p)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B) =
        mapN.map15(p)(f)

    }

    implicit class Map15COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]]) {
      private val mapN = new Map15C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, A15]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, A15]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, A15]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, B]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, B]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

    }

    implicit class Map16POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)) {
      private val mapN = new Map16P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map7(p)(f)

      def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map8(p)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map8(p)(f)

      def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map9(p)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16) =
        mapN.map9(p)(f)

      def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16) =
        mapN.map10(p)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16) =
        mapN.map10(p)(f)

      def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16) =
        mapN.map11(p)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16) =
        mapN.map11(p)(f)

      def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16) =
        mapN.map12(p)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16) =
        mapN.map12(p)(f)

      def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16) =
        mapN.map13(p)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16) =
        mapN.map13(p)(f)

      def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16) =
        mapN.map14(p)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16) =
        mapN.map14(p)(f)

      def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16) =
        mapN.map15(p)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16) =
        mapN.map15(p)(f)

      def map16[B](f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B) =
        mapN.map16(p)(f)

      def mapAt[B](f: A16 => B)(implicit @unused d: Dummy16): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B) =
        mapN.map16(p)(f)

    }

    implicit class Map16COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]]) {
      private val mapN = new Map16C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, A16]]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, A16]]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, A16]]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

      def map16[B](f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, B]]]]]]]]]]]]]]] =
        mapN.map16(c)(f)

      def mapAt[B](f: A16 => B)(implicit @unused d: Dummy16): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, B]]]]]]]]]]]]]]] =
        mapN.map16(c)(f)

    }

    implicit class Map17POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)) {
      private val mapN = new Map17P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map7(p)(f)

      def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map8(p)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map8(p)(f)

      def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map9(p)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map9(p)(f)

      def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map10(p)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17) =
        mapN.map10(p)(f)

      def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17) =
        mapN.map11(p)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17) =
        mapN.map11(p)(f)

      def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17) =
        mapN.map12(p)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17) =
        mapN.map12(p)(f)

      def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17) =
        mapN.map13(p)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17) =
        mapN.map13(p)(f)

      def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17) =
        mapN.map14(p)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17) =
        mapN.map14(p)(f)

      def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17) =
        mapN.map15(p)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17) =
        mapN.map15(p)(f)

      def map16[B](f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17) =
        mapN.map16(p)(f)

      def mapAt[B](f: A16 => B)(implicit @unused d: Dummy16): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17) =
        mapN.map16(p)(f)

      def map17[B](f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B) =
        mapN.map17(p)(f)

      def mapAt[B](f: A17 => B)(implicit @unused d: Dummy17): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B) =
        mapN.map17(p)(f)

    }

    implicit class Map17COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]]) {
      private val mapN = new Map17C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, A17]]]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

      def map16[B](f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, A17]]]]]]]]]]]]]]]] =
        mapN.map16(c)(f)

      def mapAt[B](f: A16 => B)(implicit @unused d: Dummy16): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, A17]]]]]]]]]]]]]]]] =
        mapN.map16(c)(f)

      def map17[B](f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, B]]]]]]]]]]]]]]]] =
        mapN.map17(c)(f)

      def mapAt[B](f: A17 => B)(implicit @unused d: Dummy17): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, B]]]]]]]]]]]]]]]] =
        mapN.map17(c)(f)

    }

    implicit class Map18POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)) {
      private val mapN = new Map18P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map7(p)(f)

      def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map8(p)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map8(p)(f)

      def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map9(p)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map9(p)(f)

      def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map10(p)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map10(p)(f)

      def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map11(p)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18) =
        mapN.map11(p)(f)

      def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18) =
        mapN.map12(p)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18) =
        mapN.map12(p)(f)

      def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18) =
        mapN.map13(p)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18) =
        mapN.map13(p)(f)

      def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18) =
        mapN.map14(p)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18) =
        mapN.map14(p)(f)

      def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18) =
        mapN.map15(p)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18) =
        mapN.map15(p)(f)

      def map16[B](f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18) =
        mapN.map16(p)(f)

      def mapAt[B](f: A16 => B)(implicit @unused d: Dummy16): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18) =
        mapN.map16(p)(f)

      def map17[B](f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18) =
        mapN.map17(p)(f)

      def mapAt[B](f: A17 => B)(implicit @unused d: Dummy17): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18) =
        mapN.map17(p)(f)

      def map18[B](f: A18 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B) =
        mapN.map18(p)(f)

      def mapAt[B](f: A18 => B)(implicit @unused d: Dummy18): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B) =
        mapN.map18(p)(f)

    }

    implicit class Map18COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]]) {
      private val mapN = new Map18C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

      def map16[B](f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map16(c)(f)

      def mapAt[B](f: A16 => B)(implicit @unused d: Dummy16): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, A18]]]]]]]]]]]]]]]]] =
        mapN.map16(c)(f)

      def map17[B](f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, A18]]]]]]]]]]]]]]]]] =
        mapN.map17(c)(f)

      def mapAt[B](f: A17 => B)(implicit @unused d: Dummy17): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, A18]]]]]]]]]]]]]]]]] =
        mapN.map17(c)(f)

      def map18[B](f: A18 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, B]]]]]]]]]]]]]]]]] =
        mapN.map18(c)(f)

      def mapAt[B](f: A18 => B)(implicit @unused d: Dummy18): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, B]]]]]]]]]]]]]]]]] =
        mapN.map18(c)(f)

    }

    implicit class Map19POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)) {
      private val mapN = new Map19P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map7(p)(f)

      def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map8(p)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map8(p)(f)

      def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map9(p)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map9(p)(f)

      def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map10(p)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map10(p)(f)

      def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map11(p)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map11(p)(f)

      def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map12(p)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19) =
        mapN.map12(p)(f)

      def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19) =
        mapN.map13(p)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19) =
        mapN.map13(p)(f)

      def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19) =
        mapN.map14(p)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19) =
        mapN.map14(p)(f)

      def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19) =
        mapN.map15(p)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19) =
        mapN.map15(p)(f)

      def map16[B](f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19) =
        mapN.map16(p)(f)

      def mapAt[B](f: A16 => B)(implicit @unused d: Dummy16): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19) =
        mapN.map16(p)(f)

      def map17[B](f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19) =
        mapN.map17(p)(f)

      def mapAt[B](f: A17 => B)(implicit @unused d: Dummy17): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19) =
        mapN.map17(p)(f)

      def map18[B](f: A18 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19) =
        mapN.map18(p)(f)

      def mapAt[B](f: A18 => B)(implicit @unused d: Dummy18): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19) =
        mapN.map18(p)(f)

      def map19[B](f: A19 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B) =
        mapN.map19(p)(f)

      def mapAt[B](f: A19 => B)(implicit @unused d: Dummy19): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B) =
        mapN.map19(p)(f)

    }

    implicit class Map19COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]]) {
      private val mapN = new Map19C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

      def map16[B](f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map16(c)(f)

      def mapAt[B](f: A16 => B)(implicit @unused d: Dummy16): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map16(c)(f)

      def map17[B](f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map17(c)(f)

      def mapAt[B](f: A17 => B)(implicit @unused d: Dummy17): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, A19]]]]]]]]]]]]]]]]]] =
        mapN.map17(c)(f)

      def map18[B](f: A18 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, A19]]]]]]]]]]]]]]]]]] =
        mapN.map18(c)(f)

      def mapAt[B](f: A18 => B)(implicit @unused d: Dummy18): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, A19]]]]]]]]]]]]]]]]]] =
        mapN.map18(c)(f)

      def map19[B](f: A19 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, B]]]]]]]]]]]]]]]]]] =
        mapN.map19(c)(f)

      def mapAt[B](f: A19 => B)(implicit @unused d: Dummy19): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, B]]]]]]]]]]]]]]]]]] =
        mapN.map19(c)(f)

    }

    implicit class Map20POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)) {
      private val mapN = new Map20P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map7(p)(f)

      def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map8(p)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map8(p)(f)

      def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map9(p)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map9(p)(f)

      def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map10(p)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map10(p)(f)

      def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map11(p)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map11(p)(f)

      def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map12(p)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map12(p)(f)

      def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map13(p)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19, A20) =
        mapN.map13(p)(f)

      def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19, A20) =
        mapN.map14(p)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19, A20) =
        mapN.map14(p)(f)

      def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19, A20) =
        mapN.map15(p)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19, A20) =
        mapN.map15(p)(f)

      def map16[B](f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19, A20) =
        mapN.map16(p)(f)

      def mapAt[B](f: A16 => B)(implicit @unused d: Dummy16): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19, A20) =
        mapN.map16(p)(f)

      def map17[B](f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19, A20) =
        mapN.map17(p)(f)

      def mapAt[B](f: A17 => B)(implicit @unused d: Dummy17): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19, A20) =
        mapN.map17(p)(f)

      def map18[B](f: A18 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19, A20) =
        mapN.map18(p)(f)

      def mapAt[B](f: A18 => B)(implicit @unused d: Dummy18): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19, A20) =
        mapN.map18(p)(f)

      def map19[B](f: A19 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B, A20) =
        mapN.map19(p)(f)

      def mapAt[B](f: A19 => B)(implicit @unused d: Dummy19): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B, A20) =
        mapN.map19(p)(f)

      def map20[B](f: A20 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B) =
        mapN.map20(p)(f)

      def mapAt[B](f: A20 => B)(implicit @unused d: Dummy20): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B) =
        mapN.map20(p)(f)

    }

    implicit class Map20COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]]) {
      private val mapN = new Map20C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

      def map16[B](f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map16(c)(f)

      def mapAt[B](f: A16 => B)(implicit @unused d: Dummy16): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map16(c)(f)

      def map17[B](f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map17(c)(f)

      def mapAt[B](f: A17 => B)(implicit @unused d: Dummy17): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map17(c)(f)

      def map18[B](f: A18 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map18(c)(f)

      def mapAt[B](f: A18 => B)(implicit @unused d: Dummy18): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map18(c)(f)

      def map19[B](f: A19 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[B, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map19(c)(f)

      def mapAt[B](f: A19 => B)(implicit @unused d: Dummy19): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[B, A20]]]]]]]]]]]]]]]]]]] =
        mapN.map19(c)(f)

      def map20[B](f: A20 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, B]]]]]]]]]]]]]]]]]]] =
        mapN.map20(c)(f)

      def mapAt[B](f: A20 => B)(implicit @unused d: Dummy20): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, B]]]]]]]]]]]]]]]]]]] =
        mapN.map20(c)(f)

    }

    implicit class Map21POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)) {
      private val mapN = new Map21P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map7(p)(f)

      def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map8(p)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map8(p)(f)

      def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map9(p)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map9(p)(f)

      def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map10(p)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map10(p)(f)

      def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map11(p)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map11(p)(f)

      def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map12(p)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map12(p)(f)

      def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map13(p)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map13(p)(f)

      def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map14(p)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19, A20, A21) =
        mapN.map14(p)(f)

      def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19, A20, A21) =
        mapN.map15(p)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19, A20, A21) =
        mapN.map15(p)(f)

      def map16[B](f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19, A20, A21) =
        mapN.map16(p)(f)

      def mapAt[B](f: A16 => B)(implicit @unused d: Dummy16): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19, A20, A21) =
        mapN.map16(p)(f)

      def map17[B](f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19, A20, A21) =
        mapN.map17(p)(f)

      def mapAt[B](f: A17 => B)(implicit @unused d: Dummy17): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19, A20, A21) =
        mapN.map17(p)(f)

      def map18[B](f: A18 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19, A20, A21) =
        mapN.map18(p)(f)

      def mapAt[B](f: A18 => B)(implicit @unused d: Dummy18): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19, A20, A21) =
        mapN.map18(p)(f)

      def map19[B](f: A19 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B, A20, A21) =
        mapN.map19(p)(f)

      def mapAt[B](f: A19 => B)(implicit @unused d: Dummy19): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B, A20, A21) =
        mapN.map19(p)(f)

      def map20[B](f: A20 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B, A21) =
        mapN.map20(p)(f)

      def mapAt[B](f: A20 => B)(implicit @unused d: Dummy20): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B, A21) =
        mapN.map20(p)(f)

      def map21[B](f: A21 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B) =
        mapN.map21(p)(f)

      def mapAt[B](f: A21 => B)(implicit @unused d: Dummy21): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B) =
        mapN.map21(p)(f)

    }

    implicit class Map21COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]]) {
      private val mapN = new Map21C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

      def map16[B](f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map16(c)(f)

      def mapAt[B](f: A16 => B)(implicit @unused d: Dummy16): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map16(c)(f)

      def map17[B](f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map17(c)(f)

      def mapAt[B](f: A17 => B)(implicit @unused d: Dummy17): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map17(c)(f)

      def map18[B](f: A18 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map18(c)(f)

      def mapAt[B](f: A18 => B)(implicit @unused d: Dummy18): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map18(c)(f)

      def map19[B](f: A19 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[B, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map19(c)(f)

      def mapAt[B](f: A19 => B)(implicit @unused d: Dummy19): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[B, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map19(c)(f)

      def map20[B](f: A20 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[B, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map20(c)(f)

      def mapAt[B](f: A20 => B)(implicit @unused d: Dummy20): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[B, A21]]]]]]]]]]]]]]]]]]]] =
        mapN.map20(c)(f)

      def map21[B](f: A21 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, B]]]]]]]]]]]]]]]]]]]] =
        mapN.map21(c)(f)

      def mapAt[B](f: A21 => B)(implicit @unused d: Dummy21): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, B]]]]]]]]]]]]]]]]]]]] =
        mapN.map21(c)(f)

    }

    implicit class Map22POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](p: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)) {
      private val mapN = new Map22P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {}

      def map1[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map1(p)(f)

      def mapAt[B](f: A1 => B): (B, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map1(p)(f)

      def map2[B](f: A2 => B): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map2(p)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): (A1, B, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map2(p)(f)

      def map3[B](f: A3 => B): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map3(p)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): (A1, A2, B, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map3(p)(f)

      def map4[B](f: A4 => B): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map4(p)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): (A1, A2, A3, B, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map4(p)(f)

      def map5[B](f: A5 => B): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map5(p)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): (A1, A2, A3, A4, B, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map5(p)(f)

      def map6[B](f: A6 => B): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map6(p)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): (A1, A2, A3, A4, A5, B, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map6(p)(f)

      def map7[B](f: A7 => B): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map7(p)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): (A1, A2, A3, A4, A5, A6, B, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map7(p)(f)

      def map8[B](f: A8 => B): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map8(p)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): (A1, A2, A3, A4, A5, A6, A7, B, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map8(p)(f)

      def map9[B](f: A9 => B): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map9(p)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): (A1, A2, A3, A4, A5, A6, A7, A8, B, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map9(p)(f)

      def map10[B](f: A10 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map10(p)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): (A1, A2, A3, A4, A5, A6, A7, A8, A9, B, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map10(p)(f)

      def map11[B](f: A11 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map11(p)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map11(p)(f)

      def map12[B](f: A12 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map12(p)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map12(p)(f)

      def map13[B](f: A13 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map13(p)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B, A14, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map13(p)(f)

      def map14[B](f: A14 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map14(p)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B, A15, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map14(p)(f)

      def map15[B](f: A15 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map15(p)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B, A16, A17, A18, A19, A20, A21, A22) =
        mapN.map15(p)(f)

      def map16[B](f: A16 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19, A20, A21, A22) =
        mapN.map16(p)(f)

      def mapAt[B](f: A16 => B)(implicit @unused d: Dummy16): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B, A17, A18, A19, A20, A21, A22) =
        mapN.map16(p)(f)

      def map17[B](f: A17 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19, A20, A21, A22) =
        mapN.map17(p)(f)

      def mapAt[B](f: A17 => B)(implicit @unused d: Dummy17): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B, A18, A19, A20, A21, A22) =
        mapN.map17(p)(f)

      def map18[B](f: A18 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19, A20, A21, A22) =
        mapN.map18(p)(f)

      def mapAt[B](f: A18 => B)(implicit @unused d: Dummy18): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B, A19, A20, A21, A22) =
        mapN.map18(p)(f)

      def map19[B](f: A19 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B, A20, A21, A22) =
        mapN.map19(p)(f)

      def mapAt[B](f: A19 => B)(implicit @unused d: Dummy19): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B, A20, A21, A22) =
        mapN.map19(p)(f)

      def map20[B](f: A20 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B, A21, A22) =
        mapN.map20(p)(f)

      def mapAt[B](f: A20 => B)(implicit @unused d: Dummy20): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B, A21, A22) =
        mapN.map20(p)(f)

      def map21[B](f: A21 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B, A22) =
        mapN.map21(p)(f)

      def mapAt[B](f: A21 => B)(implicit @unused d: Dummy21): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B, A22) =
        mapN.map21(p)(f)

      def map22[B](f: A22 => B): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, B) =
        mapN.map22(p)(f)

      def mapAt[B](f: A22 => B)(implicit @unused d: Dummy22): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, B) =
        mapN.map22(p)(f)

    }

    implicit class Map22COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]]) {
      private val mapN = new Map22C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {}

      def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def mapAt[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map1(c)(f)

      def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def mapAt[B](f: A2 => B)(implicit @unused d: Dummy2): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map2(c)(f)

      def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def mapAt[B](f: A3 => B)(implicit @unused d: Dummy3): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map3(c)(f)

      def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def mapAt[B](f: A4 => B)(implicit @unused d: Dummy4): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map4(c)(f)

      def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def mapAt[B](f: A5 => B)(implicit @unused d: Dummy5): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map5(c)(f)

      def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def mapAt[B](f: A6 => B)(implicit @unused d: Dummy6): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map6(c)(f)

      def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def mapAt[B](f: A7 => B)(implicit @unused d: Dummy7): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map7(c)(f)

      def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def mapAt[B](f: A8 => B)(implicit @unused d: Dummy8): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map8(c)(f)

      def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def mapAt[B](f: A9 => B)(implicit @unused d: Dummy9): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map9(c)(f)

      def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def mapAt[B](f: A10 => B)(implicit @unused d: Dummy10): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map10(c)(f)

      def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def mapAt[B](f: A11 => B)(implicit @unused d: Dummy11): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map11(c)(f)

      def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def mapAt[B](f: A12 => B)(implicit @unused d: Dummy12): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map12(c)(f)

      def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def mapAt[B](f: A13 => B)(implicit @unused d: Dummy13): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map13(c)(f)

      def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def mapAt[B](f: A14 => B)(implicit @unused d: Dummy14): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map14(c)(f)

      def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

      def mapAt[B](f: A15 => B)(implicit @unused d: Dummy15): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map15(c)(f)

      def map16[B](f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map16(c)(f)

      def mapAt[B](f: A16 => B)(implicit @unused d: Dummy16): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map16(c)(f)

      def map17[B](f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map17(c)(f)

      def mapAt[B](f: A17 => B)(implicit @unused d: Dummy17): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map17(c)(f)

      def map18[B](f: A18 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map18(c)(f)

      def mapAt[B](f: A18 => B)(implicit @unused d: Dummy18): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map18(c)(f)

      def map19[B](f: A19 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[B, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map19(c)(f)

      def mapAt[B](f: A19 => B)(implicit @unused d: Dummy19): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[B, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map19(c)(f)

      def map20[B](f: A20 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[B, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map20(c)(f)

      def mapAt[B](f: A20 => B)(implicit @unused d: Dummy20): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[B, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map20(c)(f)

      def map21[B](f: A21 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[B, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map21(c)(f)

      def mapAt[B](f: A21 => B)(implicit @unused d: Dummy21): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[B, A22]]]]]]]]]]]]]]]]]]]]] =
        mapN.map21(c)(f)

      def map22[B](f: A22 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, B]]]]]]]]]]]]]]]]]]]]] =
        mapN.map22(c)(f)

      def mapAt[B](f: A22 => B)(implicit @unused d: Dummy22): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, B]]]]]]]]]]]]]]]]]]]]] =
        mapN.map22(c)(f)

    }

  }
}
