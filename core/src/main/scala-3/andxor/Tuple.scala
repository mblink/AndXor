package andxor


import cats.Monoid

// These methods could be implemented without `.asInstanceOf`,
// but treating tuples as `Array`s yields significantly better performance
object tuple {

  extension [A1, T <: Tuple](t: A1 *: T) {
    inline final def t1: A1 = t.productElement(0).asInstanceOf[A1]

    inline final def map1[B](f: A1 => B): B *: T = {
      val arr = t.toArray
      arr.update(0, f(arr(0).asInstanceOf[A1]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[B *: T]
    }
  }


  extension [A1, A2, T <: Tuple](t: A1 *: A2 *: T) {
    inline final def t2: A2 = t.productElement(1).asInstanceOf[A2]

    inline final def map2[B](f: A2 => B): A1 *: B *: T = {
      val arr = t.toArray
      arr.update(1, f(arr(1).asInstanceOf[A2]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: B *: T]
    }
  }


  extension [A1, A2, A3, T <: Tuple](t: A1 *: A2 *: A3 *: T) {
    inline final def t3: A3 = t.productElement(2).asInstanceOf[A3]

    inline final def map3[B](f: A3 => B): A1 *: A2 *: B *: T = {
      val arr = t.toArray
      arr.update(2, f(arr(2).asInstanceOf[A3]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: T) {
    inline final def t4: A4 = t.productElement(3).asInstanceOf[A4]

    inline final def map4[B](f: A4 => B): A1 *: A2 *: A3 *: B *: T = {
      val arr = t.toArray
      arr.update(3, f(arr(3).asInstanceOf[A4]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: T) {
    inline final def t5: A5 = t.productElement(4).asInstanceOf[A5]

    inline final def map5[B](f: A5 => B): A1 *: A2 *: A3 *: A4 *: B *: T = {
      val arr = t.toArray
      arr.update(4, f(arr(4).asInstanceOf[A5]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: T) {
    inline final def t6: A6 = t.productElement(5).asInstanceOf[A6]

    inline final def map6[B](f: A6 => B): A1 *: A2 *: A3 *: A4 *: A5 *: B *: T = {
      val arr = t.toArray
      arr.update(5, f(arr(5).asInstanceOf[A6]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: T) {
    inline final def t7: A7 = t.productElement(6).asInstanceOf[A7]

    inline final def map7[B](f: A7 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: B *: T = {
      val arr = t.toArray
      arr.update(6, f(arr(6).asInstanceOf[A7]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: T) {
    inline final def t8: A8 = t.productElement(7).asInstanceOf[A8]

    inline final def map8[B](f: A8 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: B *: T = {
      val arr = t.toArray
      arr.update(7, f(arr(7).asInstanceOf[A8]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: T) {
    inline final def t9: A9 = t.productElement(8).asInstanceOf[A9]

    inline final def map9[B](f: A9 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: B *: T = {
      val arr = t.toArray
      arr.update(8, f(arr(8).asInstanceOf[A9]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: T) {
    inline final def t10: A10 = t.productElement(9).asInstanceOf[A10]

    inline final def map10[B](f: A10 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: B *: T = {
      val arr = t.toArray
      arr.update(9, f(arr(9).asInstanceOf[A10]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: T) {
    inline final def t11: A11 = t.productElement(10).asInstanceOf[A11]

    inline final def map11[B](f: A11 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: B *: T = {
      val arr = t.toArray
      arr.update(10, f(arr(10).asInstanceOf[A11]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: T) {
    inline final def t12: A12 = t.productElement(11).asInstanceOf[A12]

    inline final def map12[B](f: A12 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: B *: T = {
      val arr = t.toArray
      arr.update(11, f(arr(11).asInstanceOf[A12]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: T) {
    inline final def t13: A13 = t.productElement(12).asInstanceOf[A13]

    inline final def map13[B](f: A13 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: B *: T = {
      val arr = t.toArray
      arr.update(12, f(arr(12).asInstanceOf[A13]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: T) {
    inline final def t14: A14 = t.productElement(13).asInstanceOf[A14]

    inline final def map14[B](f: A14 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: B *: T = {
      val arr = t.toArray
      arr.update(13, f(arr(13).asInstanceOf[A14]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: T) {
    inline final def t15: A15 = t.productElement(14).asInstanceOf[A15]

    inline final def map15[B](f: A15 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: B *: T = {
      val arr = t.toArray
      arr.update(14, f(arr(14).asInstanceOf[A15]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: T) {
    inline final def t16: A16 = t.productElement(15).asInstanceOf[A16]

    inline final def map16[B](f: A16 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: B *: T = {
      val arr = t.toArray
      arr.update(15, f(arr(15).asInstanceOf[A16]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: T) {
    inline final def t17: A17 = t.productElement(16).asInstanceOf[A17]

    inline final def map17[B](f: A17 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: B *: T = {
      val arr = t.toArray
      arr.update(16, f(arr(16).asInstanceOf[A17]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: T) {
    inline final def t18: A18 = t.productElement(17).asInstanceOf[A18]

    inline final def map18[B](f: A18 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: B *: T = {
      val arr = t.toArray
      arr.update(17, f(arr(17).asInstanceOf[A18]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: T) {
    inline final def t19: A19 = t.productElement(18).asInstanceOf[A19]

    inline final def map19[B](f: A19 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: B *: T = {
      val arr = t.toArray
      arr.update(18, f(arr(18).asInstanceOf[A19]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: T) {
    inline final def t20: A20 = t.productElement(19).asInstanceOf[A20]

    inline final def map20[B](f: A20 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: B *: T = {
      val arr = t.toArray
      arr.update(19, f(arr(19).asInstanceOf[A20]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: T) {
    inline final def t21: A21 = t.productElement(20).asInstanceOf[A21]

    inline final def map21[B](f: A21 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: B *: T = {
      val arr = t.toArray
      arr.update(20, f(arr(20).asInstanceOf[A21]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: T) {
    inline final def t22: A22 = t.productElement(21).asInstanceOf[A22]

    inline final def map22[B](f: A22 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: B *: T = {
      val arr = t.toArray
      arr.update(21, f(arr(21).asInstanceOf[A22]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: T) {
    inline final def t23: A23 = t.productElement(22).asInstanceOf[A23]

    inline final def map23[B](f: A23 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: B *: T = {
      val arr = t.toArray
      arr.update(22, f(arr(22).asInstanceOf[A23]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: T) {
    inline final def t24: A24 = t.productElement(23).asInstanceOf[A24]

    inline final def map24[B](f: A24 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: B *: T = {
      val arr = t.toArray
      arr.update(23, f(arr(23).asInstanceOf[A24]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: T) {
    inline final def t25: A25 = t.productElement(24).asInstanceOf[A25]

    inline final def map25[B](f: A25 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: B *: T = {
      val arr = t.toArray
      arr.update(24, f(arr(24).asInstanceOf[A25]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: T) {
    inline final def t26: A26 = t.productElement(25).asInstanceOf[A26]

    inline final def map26[B](f: A26 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: B *: T = {
      val arr = t.toArray
      arr.update(25, f(arr(25).asInstanceOf[A26]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: T) {
    inline final def t27: A27 = t.productElement(26).asInstanceOf[A27]

    inline final def map27[B](f: A27 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: B *: T = {
      val arr = t.toArray
      arr.update(26, f(arr(26).asInstanceOf[A27]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: T) {
    inline final def t28: A28 = t.productElement(27).asInstanceOf[A28]

    inline final def map28[B](f: A28 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: B *: T = {
      val arr = t.toArray
      arr.update(27, f(arr(27).asInstanceOf[A28]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: T) {
    inline final def t29: A29 = t.productElement(28).asInstanceOf[A29]

    inline final def map29[B](f: A29 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: B *: T = {
      val arr = t.toArray
      arr.update(28, f(arr(28).asInstanceOf[A29]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: T) {
    inline final def t30: A30 = t.productElement(29).asInstanceOf[A30]

    inline final def map30[B](f: A30 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: B *: T = {
      val arr = t.toArray
      arr.update(29, f(arr(29).asInstanceOf[A30]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: T) {
    inline final def t31: A31 = t.productElement(30).asInstanceOf[A31]

    inline final def map31[B](f: A31 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: B *: T = {
      val arr = t.toArray
      arr.update(30, f(arr(30).asInstanceOf[A31]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: T) {
    inline final def t32: A32 = t.productElement(31).asInstanceOf[A32]

    inline final def map32[B](f: A32 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: B *: T = {
      val arr = t.toArray
      arr.update(31, f(arr(31).asInstanceOf[A32]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: T) {
    inline final def t33: A33 = t.productElement(32).asInstanceOf[A33]

    inline final def map33[B](f: A33 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: B *: T = {
      val arr = t.toArray
      arr.update(32, f(arr(32).asInstanceOf[A33]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: T) {
    inline final def t34: A34 = t.productElement(33).asInstanceOf[A34]

    inline final def map34[B](f: A34 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: B *: T = {
      val arr = t.toArray
      arr.update(33, f(arr(33).asInstanceOf[A34]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: T) {
    inline final def t35: A35 = t.productElement(34).asInstanceOf[A35]

    inline final def map35[B](f: A35 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: B *: T = {
      val arr = t.toArray
      arr.update(34, f(arr(34).asInstanceOf[A35]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: T) {
    inline final def t36: A36 = t.productElement(35).asInstanceOf[A36]

    inline final def map36[B](f: A36 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: B *: T = {
      val arr = t.toArray
      arr.update(35, f(arr(35).asInstanceOf[A36]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: T) {
    inline final def t37: A37 = t.productElement(36).asInstanceOf[A37]

    inline final def map37[B](f: A37 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: B *: T = {
      val arr = t.toArray
      arr.update(36, f(arr(36).asInstanceOf[A37]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: T) {
    inline final def t38: A38 = t.productElement(37).asInstanceOf[A38]

    inline final def map38[B](f: A38 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: B *: T = {
      val arr = t.toArray
      arr.update(37, f(arr(37).asInstanceOf[A38]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: T) {
    inline final def t39: A39 = t.productElement(38).asInstanceOf[A39]

    inline final def map39[B](f: A39 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: B *: T = {
      val arr = t.toArray
      arr.update(38, f(arr(38).asInstanceOf[A39]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: T) {
    inline final def t40: A40 = t.productElement(39).asInstanceOf[A40]

    inline final def map40[B](f: A40 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: B *: T = {
      val arr = t.toArray
      arr.update(39, f(arr(39).asInstanceOf[A40]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: T) {
    inline final def t41: A41 = t.productElement(40).asInstanceOf[A41]

    inline final def map41[B](f: A41 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: B *: T = {
      val arr = t.toArray
      arr.update(40, f(arr(40).asInstanceOf[A41]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: T) {
    inline final def t42: A42 = t.productElement(41).asInstanceOf[A42]

    inline final def map42[B](f: A42 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: B *: T = {
      val arr = t.toArray
      arr.update(41, f(arr(41).asInstanceOf[A42]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: T) {
    inline final def t43: A43 = t.productElement(42).asInstanceOf[A43]

    inline final def map43[B](f: A43 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: B *: T = {
      val arr = t.toArray
      arr.update(42, f(arr(42).asInstanceOf[A43]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: T) {
    inline final def t44: A44 = t.productElement(43).asInstanceOf[A44]

    inline final def map44[B](f: A44 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: B *: T = {
      val arr = t.toArray
      arr.update(43, f(arr(43).asInstanceOf[A44]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: T) {
    inline final def t45: A45 = t.productElement(44).asInstanceOf[A45]

    inline final def map45[B](f: A45 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: B *: T = {
      val arr = t.toArray
      arr.update(44, f(arr(44).asInstanceOf[A45]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: T) {
    inline final def t46: A46 = t.productElement(45).asInstanceOf[A46]

    inline final def map46[B](f: A46 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: B *: T = {
      val arr = t.toArray
      arr.update(45, f(arr(45).asInstanceOf[A46]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: T) {
    inline final def t47: A47 = t.productElement(46).asInstanceOf[A47]

    inline final def map47[B](f: A47 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: B *: T = {
      val arr = t.toArray
      arr.update(46, f(arr(46).asInstanceOf[A47]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: T) {
    inline final def t48: A48 = t.productElement(47).asInstanceOf[A48]

    inline final def map48[B](f: A48 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: B *: T = {
      val arr = t.toArray
      arr.update(47, f(arr(47).asInstanceOf[A48]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: T) {
    inline final def t49: A49 = t.productElement(48).asInstanceOf[A49]

    inline final def map49[B](f: A49 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: B *: T = {
      val arr = t.toArray
      arr.update(48, f(arr(48).asInstanceOf[A49]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: T) {
    inline final def t50: A50 = t.productElement(49).asInstanceOf[A50]

    inline final def map50[B](f: A50 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: B *: T = {
      val arr = t.toArray
      arr.update(49, f(arr(49).asInstanceOf[A50]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: T) {
    inline final def t51: A51 = t.productElement(50).asInstanceOf[A51]

    inline final def map51[B](f: A51 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: B *: T = {
      val arr = t.toArray
      arr.update(50, f(arr(50).asInstanceOf[A51]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: T) {
    inline final def t52: A52 = t.productElement(51).asInstanceOf[A52]

    inline final def map52[B](f: A52 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: B *: T = {
      val arr = t.toArray
      arr.update(51, f(arr(51).asInstanceOf[A52]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: T) {
    inline final def t53: A53 = t.productElement(52).asInstanceOf[A53]

    inline final def map53[B](f: A53 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: B *: T = {
      val arr = t.toArray
      arr.update(52, f(arr(52).asInstanceOf[A53]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: T) {
    inline final def t54: A54 = t.productElement(53).asInstanceOf[A54]

    inline final def map54[B](f: A54 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: B *: T = {
      val arr = t.toArray
      arr.update(53, f(arr(53).asInstanceOf[A54]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: T) {
    inline final def t55: A55 = t.productElement(54).asInstanceOf[A55]

    inline final def map55[B](f: A55 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: B *: T = {
      val arr = t.toArray
      arr.update(54, f(arr(54).asInstanceOf[A55]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: T) {
    inline final def t56: A56 = t.productElement(55).asInstanceOf[A56]

    inline final def map56[B](f: A56 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: B *: T = {
      val arr = t.toArray
      arr.update(55, f(arr(55).asInstanceOf[A56]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: T) {
    inline final def t57: A57 = t.productElement(56).asInstanceOf[A57]

    inline final def map57[B](f: A57 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: B *: T = {
      val arr = t.toArray
      arr.update(56, f(arr(56).asInstanceOf[A57]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: T) {
    inline final def t58: A58 = t.productElement(57).asInstanceOf[A58]

    inline final def map58[B](f: A58 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: B *: T = {
      val arr = t.toArray
      arr.update(57, f(arr(57).asInstanceOf[A58]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: T) {
    inline final def t59: A59 = t.productElement(58).asInstanceOf[A59]

    inline final def map59[B](f: A59 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: B *: T = {
      val arr = t.toArray
      arr.update(58, f(arr(58).asInstanceOf[A59]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: T) {
    inline final def t60: A60 = t.productElement(59).asInstanceOf[A60]

    inline final def map60[B](f: A60 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: B *: T = {
      val arr = t.toArray
      arr.update(59, f(arr(59).asInstanceOf[A60]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: T) {
    inline final def t61: A61 = t.productElement(60).asInstanceOf[A61]

    inline final def map61[B](f: A61 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: B *: T = {
      val arr = t.toArray
      arr.update(60, f(arr(60).asInstanceOf[A61]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: T) {
    inline final def t62: A62 = t.productElement(61).asInstanceOf[A62]

    inline final def map62[B](f: A62 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: B *: T = {
      val arr = t.toArray
      arr.update(61, f(arr(61).asInstanceOf[A62]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: T) {
    inline final def t63: A63 = t.productElement(62).asInstanceOf[A63]

    inline final def map63[B](f: A63 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: B *: T = {
      val arr = t.toArray
      arr.update(62, f(arr(62).asInstanceOf[A63]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: T) {
    inline final def t64: A64 = t.productElement(63).asInstanceOf[A64]

    inline final def map64[B](f: A64 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: B *: T = {
      val arr = t.toArray
      arr.update(63, f(arr(63).asInstanceOf[A64]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: T) {
    inline final def t65: A65 = t.productElement(64).asInstanceOf[A65]

    inline final def map65[B](f: A65 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: B *: T = {
      val arr = t.toArray
      arr.update(64, f(arr(64).asInstanceOf[A65]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: T) {
    inline final def t66: A66 = t.productElement(65).asInstanceOf[A66]

    inline final def map66[B](f: A66 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: B *: T = {
      val arr = t.toArray
      arr.update(65, f(arr(65).asInstanceOf[A66]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: T) {
    inline final def t67: A67 = t.productElement(66).asInstanceOf[A67]

    inline final def map67[B](f: A67 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: B *: T = {
      val arr = t.toArray
      arr.update(66, f(arr(66).asInstanceOf[A67]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: T) {
    inline final def t68: A68 = t.productElement(67).asInstanceOf[A68]

    inline final def map68[B](f: A68 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: B *: T = {
      val arr = t.toArray
      arr.update(67, f(arr(67).asInstanceOf[A68]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: T) {
    inline final def t69: A69 = t.productElement(68).asInstanceOf[A69]

    inline final def map69[B](f: A69 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: B *: T = {
      val arr = t.toArray
      arr.update(68, f(arr(68).asInstanceOf[A69]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: T) {
    inline final def t70: A70 = t.productElement(69).asInstanceOf[A70]

    inline final def map70[B](f: A70 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: B *: T = {
      val arr = t.toArray
      arr.update(69, f(arr(69).asInstanceOf[A70]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: T) {
    inline final def t71: A71 = t.productElement(70).asInstanceOf[A71]

    inline final def map71[B](f: A71 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: B *: T = {
      val arr = t.toArray
      arr.update(70, f(arr(70).asInstanceOf[A71]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: T) {
    inline final def t72: A72 = t.productElement(71).asInstanceOf[A72]

    inline final def map72[B](f: A72 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: B *: T = {
      val arr = t.toArray
      arr.update(71, f(arr(71).asInstanceOf[A72]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: T) {
    inline final def t73: A73 = t.productElement(72).asInstanceOf[A73]

    inline final def map73[B](f: A73 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: B *: T = {
      val arr = t.toArray
      arr.update(72, f(arr(72).asInstanceOf[A73]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: T) {
    inline final def t74: A74 = t.productElement(73).asInstanceOf[A74]

    inline final def map74[B](f: A74 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: B *: T = {
      val arr = t.toArray
      arr.update(73, f(arr(73).asInstanceOf[A74]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: T) {
    inline final def t75: A75 = t.productElement(74).asInstanceOf[A75]

    inline final def map75[B](f: A75 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: B *: T = {
      val arr = t.toArray
      arr.update(74, f(arr(74).asInstanceOf[A75]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: T) {
    inline final def t76: A76 = t.productElement(75).asInstanceOf[A76]

    inline final def map76[B](f: A76 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: B *: T = {
      val arr = t.toArray
      arr.update(75, f(arr(75).asInstanceOf[A76]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: T) {
    inline final def t77: A77 = t.productElement(76).asInstanceOf[A77]

    inline final def map77[B](f: A77 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: B *: T = {
      val arr = t.toArray
      arr.update(76, f(arr(76).asInstanceOf[A77]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: T) {
    inline final def t78: A78 = t.productElement(77).asInstanceOf[A78]

    inline final def map78[B](f: A78 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: B *: T = {
      val arr = t.toArray
      arr.update(77, f(arr(77).asInstanceOf[A78]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: T) {
    inline final def t79: A79 = t.productElement(78).asInstanceOf[A79]

    inline final def map79[B](f: A79 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: B *: T = {
      val arr = t.toArray
      arr.update(78, f(arr(78).asInstanceOf[A79]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: T) {
    inline final def t80: A80 = t.productElement(79).asInstanceOf[A80]

    inline final def map80[B](f: A80 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: B *: T = {
      val arr = t.toArray
      arr.update(79, f(arr(79).asInstanceOf[A80]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: T) {
    inline final def t81: A81 = t.productElement(80).asInstanceOf[A81]

    inline final def map81[B](f: A81 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: B *: T = {
      val arr = t.toArray
      arr.update(80, f(arr(80).asInstanceOf[A81]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: T) {
    inline final def t82: A82 = t.productElement(81).asInstanceOf[A82]

    inline final def map82[B](f: A82 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: B *: T = {
      val arr = t.toArray
      arr.update(81, f(arr(81).asInstanceOf[A82]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: T) {
    inline final def t83: A83 = t.productElement(82).asInstanceOf[A83]

    inline final def map83[B](f: A83 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: B *: T = {
      val arr = t.toArray
      arr.update(82, f(arr(82).asInstanceOf[A83]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: T) {
    inline final def t84: A84 = t.productElement(83).asInstanceOf[A84]

    inline final def map84[B](f: A84 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: B *: T = {
      val arr = t.toArray
      arr.update(83, f(arr(83).asInstanceOf[A84]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: T) {
    inline final def t85: A85 = t.productElement(84).asInstanceOf[A85]

    inline final def map85[B](f: A85 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: B *: T = {
      val arr = t.toArray
      arr.update(84, f(arr(84).asInstanceOf[A85]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: T) {
    inline final def t86: A86 = t.productElement(85).asInstanceOf[A86]

    inline final def map86[B](f: A86 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: B *: T = {
      val arr = t.toArray
      arr.update(85, f(arr(85).asInstanceOf[A86]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: T) {
    inline final def t87: A87 = t.productElement(86).asInstanceOf[A87]

    inline final def map87[B](f: A87 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: B *: T = {
      val arr = t.toArray
      arr.update(86, f(arr(86).asInstanceOf[A87]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: T) {
    inline final def t88: A88 = t.productElement(87).asInstanceOf[A88]

    inline final def map88[B](f: A88 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: B *: T = {
      val arr = t.toArray
      arr.update(87, f(arr(87).asInstanceOf[A88]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: T) {
    inline final def t89: A89 = t.productElement(88).asInstanceOf[A89]

    inline final def map89[B](f: A89 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: B *: T = {
      val arr = t.toArray
      arr.update(88, f(arr(88).asInstanceOf[A89]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: T) {
    inline final def t90: A90 = t.productElement(89).asInstanceOf[A90]

    inline final def map90[B](f: A90 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: B *: T = {
      val arr = t.toArray
      arr.update(89, f(arr(89).asInstanceOf[A90]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: T) {
    inline final def t91: A91 = t.productElement(90).asInstanceOf[A91]

    inline final def map91[B](f: A91 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: B *: T = {
      val arr = t.toArray
      arr.update(90, f(arr(90).asInstanceOf[A91]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: T) {
    inline final def t92: A92 = t.productElement(91).asInstanceOf[A92]

    inline final def map92[B](f: A92 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: B *: T = {
      val arr = t.toArray
      arr.update(91, f(arr(91).asInstanceOf[A92]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: T) {
    inline final def t93: A93 = t.productElement(92).asInstanceOf[A93]

    inline final def map93[B](f: A93 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: B *: T = {
      val arr = t.toArray
      arr.update(92, f(arr(92).asInstanceOf[A93]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, A94, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: T) {
    inline final def t94: A94 = t.productElement(93).asInstanceOf[A94]

    inline final def map94[B](f: A94 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: B *: T = {
      val arr = t.toArray
      arr.update(93, f(arr(93).asInstanceOf[A94]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, A94, A95, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: T) {
    inline final def t95: A95 = t.productElement(94).asInstanceOf[A95]

    inline final def map95[B](f: A95 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: B *: T = {
      val arr = t.toArray
      arr.update(94, f(arr(94).asInstanceOf[A95]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, A94, A95, A96, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: T) {
    inline final def t96: A96 = t.productElement(95).asInstanceOf[A96]

    inline final def map96[B](f: A96 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: B *: T = {
      val arr = t.toArray
      arr.update(95, f(arr(95).asInstanceOf[A96]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, A94, A95, A96, A97, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: T) {
    inline final def t97: A97 = t.productElement(96).asInstanceOf[A97]

    inline final def map97[B](f: A97 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: B *: T = {
      val arr = t.toArray
      arr.update(96, f(arr(96).asInstanceOf[A97]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, A94, A95, A96, A97, A98, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: A98 *: T) {
    inline final def t98: A98 = t.productElement(97).asInstanceOf[A98]

    inline final def map98[B](f: A98 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: B *: T = {
      val arr = t.toArray
      arr.update(97, f(arr(97).asInstanceOf[A98]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, A94, A95, A96, A97, A98, A99, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: A98 *: A99 *: T) {
    inline final def t99: A99 = t.productElement(98).asInstanceOf[A99]

    inline final def map99[B](f: A99 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: A98 *: B *: T = {
      val arr = t.toArray
      arr.update(98, f(arr(98).asInstanceOf[A99]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: A98 *: B *: T]
    }
  }


  extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, A94, A95, A96, A97, A98, A99, A100, T <: Tuple](t: A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: A98 *: A99 *: A100 *: T) {
    inline final def t100: A100 = t.productElement(99).asInstanceOf[A100]

    inline final def map100[B](f: A100 => B): A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: A98 *: A99 *: B *: T = {
      val arr = t.toArray
      arr.update(99, f(arr(99).asInstanceOf[A100]).asInstanceOf[Object])
      Tuple.fromArray(arr).asInstanceOf[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: A98 *: A99 *: B *: T]
    }
  }

}

trait TupleInj {
  import andxor.tuple._


  implicit def injT1[A1, T <: Tuple](
    using M: Monoid[A1 *: T],
  ): Inj[A1 *: T, A1] =
    Inj.instance((a: A1) => M.empty.map1(_ => a))


  implicit def injT2[A1, A2, T <: Tuple](
    using M: Monoid[A1 *: A2 *: T],
  ): Inj[A1 *: A2 *: T, A2] =
    Inj.instance((a: A2) => M.empty.map2(_ => a))


  implicit def injT3[A1, A2, A3, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: T],
  ): Inj[A1 *: A2 *: A3 *: T, A3] =
    Inj.instance((a: A3) => M.empty.map3(_ => a))


  implicit def injT4[A1, A2, A3, A4, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: T, A4] =
    Inj.instance((a: A4) => M.empty.map4(_ => a))


  implicit def injT5[A1, A2, A3, A4, A5, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: T, A5] =
    Inj.instance((a: A5) => M.empty.map5(_ => a))


  implicit def injT6[A1, A2, A3, A4, A5, A6, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: T, A6] =
    Inj.instance((a: A6) => M.empty.map6(_ => a))


  implicit def injT7[A1, A2, A3, A4, A5, A6, A7, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: T, A7] =
    Inj.instance((a: A7) => M.empty.map7(_ => a))


  implicit def injT8[A1, A2, A3, A4, A5, A6, A7, A8, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: T, A8] =
    Inj.instance((a: A8) => M.empty.map8(_ => a))


  implicit def injT9[A1, A2, A3, A4, A5, A6, A7, A8, A9, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: T, A9] =
    Inj.instance((a: A9) => M.empty.map9(_ => a))


  implicit def injT10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: T, A10] =
    Inj.instance((a: A10) => M.empty.map10(_ => a))


  implicit def injT11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: T, A11] =
    Inj.instance((a: A11) => M.empty.map11(_ => a))


  implicit def injT12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: T, A12] =
    Inj.instance((a: A12) => M.empty.map12(_ => a))


  implicit def injT13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: T, A13] =
    Inj.instance((a: A13) => M.empty.map13(_ => a))


  implicit def injT14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: T, A14] =
    Inj.instance((a: A14) => M.empty.map14(_ => a))


  implicit def injT15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: T, A15] =
    Inj.instance((a: A15) => M.empty.map15(_ => a))


  implicit def injT16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: T, A16] =
    Inj.instance((a: A16) => M.empty.map16(_ => a))


  implicit def injT17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: T, A17] =
    Inj.instance((a: A17) => M.empty.map17(_ => a))


  implicit def injT18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: T, A18] =
    Inj.instance((a: A18) => M.empty.map18(_ => a))


  implicit def injT19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: T, A19] =
    Inj.instance((a: A19) => M.empty.map19(_ => a))


  implicit def injT20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: T, A20] =
    Inj.instance((a: A20) => M.empty.map20(_ => a))


  implicit def injT21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: T, A21] =
    Inj.instance((a: A21) => M.empty.map21(_ => a))


  implicit def injT22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: T, A22] =
    Inj.instance((a: A22) => M.empty.map22(_ => a))


  implicit def injT23[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: T, A23] =
    Inj.instance((a: A23) => M.empty.map23(_ => a))


  implicit def injT24[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: T, A24] =
    Inj.instance((a: A24) => M.empty.map24(_ => a))


  implicit def injT25[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: T, A25] =
    Inj.instance((a: A25) => M.empty.map25(_ => a))


  implicit def injT26[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: T, A26] =
    Inj.instance((a: A26) => M.empty.map26(_ => a))


  implicit def injT27[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: T, A27] =
    Inj.instance((a: A27) => M.empty.map27(_ => a))


  implicit def injT28[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: T, A28] =
    Inj.instance((a: A28) => M.empty.map28(_ => a))


  implicit def injT29[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: T, A29] =
    Inj.instance((a: A29) => M.empty.map29(_ => a))


  implicit def injT30[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: T, A30] =
    Inj.instance((a: A30) => M.empty.map30(_ => a))


  implicit def injT31[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: T, A31] =
    Inj.instance((a: A31) => M.empty.map31(_ => a))


  implicit def injT32[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: T, A32] =
    Inj.instance((a: A32) => M.empty.map32(_ => a))


  implicit def injT33[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: T, A33] =
    Inj.instance((a: A33) => M.empty.map33(_ => a))


  implicit def injT34[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: T, A34] =
    Inj.instance((a: A34) => M.empty.map34(_ => a))


  implicit def injT35[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: T, A35] =
    Inj.instance((a: A35) => M.empty.map35(_ => a))


  implicit def injT36[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: T, A36] =
    Inj.instance((a: A36) => M.empty.map36(_ => a))


  implicit def injT37[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: T, A37] =
    Inj.instance((a: A37) => M.empty.map37(_ => a))


  implicit def injT38[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: T, A38] =
    Inj.instance((a: A38) => M.empty.map38(_ => a))


  implicit def injT39[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: T, A39] =
    Inj.instance((a: A39) => M.empty.map39(_ => a))


  implicit def injT40[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: T, A40] =
    Inj.instance((a: A40) => M.empty.map40(_ => a))


  implicit def injT41[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: T, A41] =
    Inj.instance((a: A41) => M.empty.map41(_ => a))


  implicit def injT42[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: T, A42] =
    Inj.instance((a: A42) => M.empty.map42(_ => a))


  implicit def injT43[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: T, A43] =
    Inj.instance((a: A43) => M.empty.map43(_ => a))


  implicit def injT44[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: T, A44] =
    Inj.instance((a: A44) => M.empty.map44(_ => a))


  implicit def injT45[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: T, A45] =
    Inj.instance((a: A45) => M.empty.map45(_ => a))


  implicit def injT46[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: T, A46] =
    Inj.instance((a: A46) => M.empty.map46(_ => a))


  implicit def injT47[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: T, A47] =
    Inj.instance((a: A47) => M.empty.map47(_ => a))


  implicit def injT48[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: T, A48] =
    Inj.instance((a: A48) => M.empty.map48(_ => a))


  implicit def injT49[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: T, A49] =
    Inj.instance((a: A49) => M.empty.map49(_ => a))


  implicit def injT50[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: T, A50] =
    Inj.instance((a: A50) => M.empty.map50(_ => a))


  implicit def injT51[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: T, A51] =
    Inj.instance((a: A51) => M.empty.map51(_ => a))


  implicit def injT52[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: T, A52] =
    Inj.instance((a: A52) => M.empty.map52(_ => a))


  implicit def injT53[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: T, A53] =
    Inj.instance((a: A53) => M.empty.map53(_ => a))


  implicit def injT54[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: T, A54] =
    Inj.instance((a: A54) => M.empty.map54(_ => a))


  implicit def injT55[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: T, A55] =
    Inj.instance((a: A55) => M.empty.map55(_ => a))


  implicit def injT56[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: T, A56] =
    Inj.instance((a: A56) => M.empty.map56(_ => a))


  implicit def injT57[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: T, A57] =
    Inj.instance((a: A57) => M.empty.map57(_ => a))


  implicit def injT58[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: T, A58] =
    Inj.instance((a: A58) => M.empty.map58(_ => a))


  implicit def injT59[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: T, A59] =
    Inj.instance((a: A59) => M.empty.map59(_ => a))


  implicit def injT60[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: T, A60] =
    Inj.instance((a: A60) => M.empty.map60(_ => a))


  implicit def injT61[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: T, A61] =
    Inj.instance((a: A61) => M.empty.map61(_ => a))


  implicit def injT62[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: T, A62] =
    Inj.instance((a: A62) => M.empty.map62(_ => a))


  implicit def injT63[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: T, A63] =
    Inj.instance((a: A63) => M.empty.map63(_ => a))


  implicit def injT64[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: T, A64] =
    Inj.instance((a: A64) => M.empty.map64(_ => a))


  implicit def injT65[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: T, A65] =
    Inj.instance((a: A65) => M.empty.map65(_ => a))


  implicit def injT66[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: T, A66] =
    Inj.instance((a: A66) => M.empty.map66(_ => a))


  implicit def injT67[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: T, A67] =
    Inj.instance((a: A67) => M.empty.map67(_ => a))


  implicit def injT68[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: T, A68] =
    Inj.instance((a: A68) => M.empty.map68(_ => a))


  implicit def injT69[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: T, A69] =
    Inj.instance((a: A69) => M.empty.map69(_ => a))


  implicit def injT70[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: T, A70] =
    Inj.instance((a: A70) => M.empty.map70(_ => a))


  implicit def injT71[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: T, A71] =
    Inj.instance((a: A71) => M.empty.map71(_ => a))


  implicit def injT72[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: T, A72] =
    Inj.instance((a: A72) => M.empty.map72(_ => a))


  implicit def injT73[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: T, A73] =
    Inj.instance((a: A73) => M.empty.map73(_ => a))


  implicit def injT74[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: T, A74] =
    Inj.instance((a: A74) => M.empty.map74(_ => a))


  implicit def injT75[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: T, A75] =
    Inj.instance((a: A75) => M.empty.map75(_ => a))


  implicit def injT76[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: T, A76] =
    Inj.instance((a: A76) => M.empty.map76(_ => a))


  implicit def injT77[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: T, A77] =
    Inj.instance((a: A77) => M.empty.map77(_ => a))


  implicit def injT78[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: T, A78] =
    Inj.instance((a: A78) => M.empty.map78(_ => a))


  implicit def injT79[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: T, A79] =
    Inj.instance((a: A79) => M.empty.map79(_ => a))


  implicit def injT80[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: T, A80] =
    Inj.instance((a: A80) => M.empty.map80(_ => a))


  implicit def injT81[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: T, A81] =
    Inj.instance((a: A81) => M.empty.map81(_ => a))


  implicit def injT82[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: T, A82] =
    Inj.instance((a: A82) => M.empty.map82(_ => a))


  implicit def injT83[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: T, A83] =
    Inj.instance((a: A83) => M.empty.map83(_ => a))


  implicit def injT84[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: T, A84] =
    Inj.instance((a: A84) => M.empty.map84(_ => a))


  implicit def injT85[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: T, A85] =
    Inj.instance((a: A85) => M.empty.map85(_ => a))


  implicit def injT86[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: T, A86] =
    Inj.instance((a: A86) => M.empty.map86(_ => a))


  implicit def injT87[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: T, A87] =
    Inj.instance((a: A87) => M.empty.map87(_ => a))


  implicit def injT88[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: T, A88] =
    Inj.instance((a: A88) => M.empty.map88(_ => a))


  implicit def injT89[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: T, A89] =
    Inj.instance((a: A89) => M.empty.map89(_ => a))


  implicit def injT90[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: T, A90] =
    Inj.instance((a: A90) => M.empty.map90(_ => a))


  implicit def injT91[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: T, A91] =
    Inj.instance((a: A91) => M.empty.map91(_ => a))


  implicit def injT92[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: T, A92] =
    Inj.instance((a: A92) => M.empty.map92(_ => a))


  implicit def injT93[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: T, A93] =
    Inj.instance((a: A93) => M.empty.map93(_ => a))


  implicit def injT94[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, A94, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: T, A94] =
    Inj.instance((a: A94) => M.empty.map94(_ => a))


  implicit def injT95[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, A94, A95, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: T, A95] =
    Inj.instance((a: A95) => M.empty.map95(_ => a))


  implicit def injT96[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, A94, A95, A96, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: T, A96] =
    Inj.instance((a: A96) => M.empty.map96(_ => a))


  implicit def injT97[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, A94, A95, A96, A97, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: T, A97] =
    Inj.instance((a: A97) => M.empty.map97(_ => a))


  implicit def injT98[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, A94, A95, A96, A97, A98, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: A98 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: A98 *: T, A98] =
    Inj.instance((a: A98) => M.empty.map98(_ => a))


  implicit def injT99[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, A94, A95, A96, A97, A98, A99, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: A98 *: A99 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: A98 *: A99 *: T, A99] =
    Inj.instance((a: A99) => M.empty.map99(_ => a))


  implicit def injT100[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40, A41, A42, A43, A44, A45, A46, A47, A48, A49, A50, A51, A52, A53, A54, A55, A56, A57, A58, A59, A60, A61, A62, A63, A64, A65, A66, A67, A68, A69, A70, A71, A72, A73, A74, A75, A76, A77, A78, A79, A80, A81, A82, A83, A84, A85, A86, A87, A88, A89, A90, A91, A92, A93, A94, A95, A96, A97, A98, A99, A100, T <: Tuple](
    using M: Monoid[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: A98 *: A99 *: A100 *: T],
  ): Inj[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: A22 *: A23 *: A24 *: A25 *: A26 *: A27 *: A28 *: A29 *: A30 *: A31 *: A32 *: A33 *: A34 *: A35 *: A36 *: A37 *: A38 *: A39 *: A40 *: A41 *: A42 *: A43 *: A44 *: A45 *: A46 *: A47 *: A48 *: A49 *: A50 *: A51 *: A52 *: A53 *: A54 *: A55 *: A56 *: A57 *: A58 *: A59 *: A60 *: A61 *: A62 *: A63 *: A64 *: A65 *: A66 *: A67 *: A68 *: A69 *: A70 *: A71 *: A72 *: A73 *: A74 *: A75 *: A76 *: A77 *: A78 *: A79 *: A80 *: A81 *: A82 *: A83 *: A84 *: A85 *: A86 *: A87 *: A88 *: A89 *: A90 *: A91 *: A92 *: A93 *: A94 *: A95 *: A96 *: A97 *: A98 *: A99 *: A100 *: T, A100] =
    Inj.instance((a: A100) => M.empty.map100(_ => a))

}
