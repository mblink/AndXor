package andxor

import cats.syntax.either._

object either {

  final implicit class Either2Ops[A1, A2](private val c: Either[A1, A2]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, A2] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, B] =
      c.map(f)

  }

  final implicit class Either3Ops[A1, A2, A3](private val c: Either[A1, Either[A2, A3]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, A3]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, A3]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, B]] =
      c.map(_.map(f))

  }

  final implicit class Either4Ops[A1, A2, A3, A4](private val c: Either[A1, Either[A2, Either[A3, A4]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, A4]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, A4]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, A4]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, B]]] =
      c.map(_.map(_.map(f)))

  }

  final implicit class Either5Ops[A1, A2, A3, A4, A5](private val c: Either[A1, Either[A2, Either[A3, Either[A4, A5]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, A5]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, A5]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, A5]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, A5]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, B]]]] =
      c.map(_.map(_.map(_.map(f))))

  }

  final implicit class Either6Ops[A1, A2, A3, A4, A5, A6](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, A6]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, A6]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, A6]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, A6]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, A6]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, B]]]]] =
      c.map(_.map(_.map(_.map(_.map(f)))))

  }

  final implicit class Either7Ops[A1, A2, A3, A4, A5, A6, A7](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, A7]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, A7]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, A7]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, A7]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, A7]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, B]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(f))))))

  }

  final implicit class Either8Ops[A1, A2, A3, A4, A5, A6, A7, A8](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, A8]]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, A8]]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, A8]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, A8]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

    def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, B]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))

  }

  final implicit class Either9Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, A9]]]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, A9]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, A9]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

    def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, A9]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

    def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, B]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))

  }

  final implicit class Either10Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, A10]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, A10]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

    def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, A10]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

    def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, A10]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

    def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, B]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))

  }

  final implicit class Either11Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, A11]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

    def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, A11]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

    def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, A11]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

    def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, A11]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

    def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, B]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))

  }

  final implicit class Either12Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

    def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, A12]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

    def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, A12]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

    def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, A12]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

    def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, A12]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

    def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, B]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))

  }

  final implicit class Either13Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

    def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

    def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, A13]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

    def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, A13]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

    def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, A13]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

    def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, A13]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

    def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, B]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))

  }

  final implicit class Either14Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

    def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

    def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

    def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

    def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, A14]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

    def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, A14]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

    def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, A14]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

    def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, B]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))

  }

  final implicit class Either15Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

    def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

    def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

    def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

    def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

    def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, A15]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

    def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, A15]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

    def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, A15]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

    def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, B]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))

  }

  final implicit class Either16Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

    def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

    def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

    def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

    def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

    def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

    def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, A16]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

    def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, A16]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

    def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, A16]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))

    def map16[B](f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, B]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))

  }

  final implicit class Either17Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

    def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

    def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

    def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

    def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

    def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

    def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

    def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, A17]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

    def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, A17]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))

    def map16[B](f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, A17]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))

    def map17[B](f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, B]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))))

  }

  final implicit class Either18Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

    def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

    def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

    def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

    def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

    def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

    def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

    def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

    def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))

    def map16[B](f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, A18]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))

    def map17[B](f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, A18]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))

    def map18[B](f: A18 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, B]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))))

  }

  final implicit class Either19Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

    def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

    def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

    def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

    def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

    def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

    def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

    def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

    def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))

    def map16[B](f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))

    def map17[B](f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))

    def map18[B](f: A18 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, A19]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))

    def map19[B](f: A19 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, B]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))))))

  }

  final implicit class Either20Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

    def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

    def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

    def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

    def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

    def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

    def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

    def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

    def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))

    def map16[B](f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))

    def map17[B](f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))

    def map18[B](f: A18 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, Either[A19, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))

    def map19[B](f: A19 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[B, A20]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))

    def map20[B](f: A20 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, B]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))))))

  }

  final implicit class Either21Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

    def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

    def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

    def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

    def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

    def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

    def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

    def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

    def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))

    def map16[B](f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))

    def map17[B](f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))

    def map18[B](f: A18 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, Either[A19, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))

    def map19[B](f: A19 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[B, Either[A20, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))

    def map20[B](f: A20 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[B, A21]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))

    def map21[B](f: A21 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, B]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))))))))

  }

  final implicit class Either22Ops[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](private val c: Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]]) extends AnyVal {

    def map1[B](f: A1 => B): Either[B, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.leftMap(f)

    def map2[B](f: A2 => B): Either[A1, Either[B, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.leftMap(f))

    def map3[B](f: A3 => B): Either[A1, Either[A2, Either[B, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.leftMap(f)))

    def map4[B](f: A4 => B): Either[A1, Either[A2, Either[A3, Either[B, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.leftMap(f))))

    def map5[B](f: A5 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[B, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.leftMap(f)))))

    def map6[B](f: A6 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[B, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))

    def map7[B](f: A7 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[B, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))

    def map8[B](f: A8 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[B, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))

    def map9[B](f: A9 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[B, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))

    def map10[B](f: A10 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[B, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))

    def map11[B](f: A11 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[B, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))

    def map12[B](f: A12 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[B, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))

    def map13[B](f: A13 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[B, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))

    def map14[B](f: A14 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[B, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))

    def map15[B](f: A15 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[B, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))

    def map16[B](f: A16 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[B, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))

    def map17[B](f: A17 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[B, Either[A18, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))

    def map18[B](f: A18 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[B, Either[A19, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))

    def map19[B](f: A19 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[B, Either[A20, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))

    def map20[B](f: A20 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[B, Either[A21, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))

    def map21[B](f: A21 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[B, A22]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))

    def map22[B](f: A22 => B): Either[A1, Either[A2, Either[A3, Either[A4, Either[A5, Either[A6, Either[A7, Either[A8, Either[A9, Either[A10, Either[A11, Either[A12, Either[A13, Either[A14, Either[A15, Either[A16, Either[A17, Either[A18, Either[A19, Either[A20, Either[A21, B]]]]]]]]]]]]]]]]]]]]] =
      c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))))))))

  }

}
