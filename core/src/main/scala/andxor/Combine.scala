package andxor

import scala.language.higherKinds
import scalaz.{Apply, \/}

object Combine {
  def divide2[F[_], A1, A2, Z](a1: => F[A1], a2: => F[A2])(f: Z => (A1, A2))(implicit D: Divide[F]): F[Z] = D.divide2(a1, a2)(f)

  def divide3[F[_], A1, A2, A3, Z](a0: => F[A1], a1: => F[A2], a2: => F[A3])(
      f: Z => (A1, A2, A3)
  )(implicit D: Divide[F]): F[Z] = D.divide2(a0, D.tuple2(a1, a2)) { z =>
    val t = f(z)
    (t._1, (t._2, t._3))
  }

  def divide4[F[_], A1, A2, A3, A4, Z](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4])(
      f: Z => (A1, A2, A3, A4)
  )(implicit D: Divide[F]): F[Z] = D.divide2(a0, D.tuple2(a1, D.tuple2(a2, a3))) { z =>
    val t = f(z)
    (t._1, (t._2, (t._3, t._4)))
  }

  def divide5[F[_], A1, A2, A3, A4, A5, Z](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5])(
      f: Z => (A1, A2, A3, A4, A5)
  )(implicit D: Divide[F]): F[Z] = D.divide2(a0, D.tuple2(a1, D.tuple2(a2, D.tuple2(a3, a4)))) { z =>
    val t = f(z)
    (t._1, (t._2, (t._3, (t._4, t._5))))
  }

  def divide6[F[_], A1, A2, A3, A4, A5, A6, Z](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6])(
      f: Z => (A1, A2, A3, A4, A5, A6)
  )(implicit D: Divide[F]): F[Z] = D.divide2(a0, D.tuple2(a1, D.tuple2(a2, D.tuple2(a3, D.tuple2(a4, a5))))) { z =>
    val t = f(z)
    (t._1, (t._2, (t._3, (t._4, (t._5, t._6)))))
  }

  def divide7[F[_], A1, A2, A3, A4, A5, A6, A7, Z](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6], a6: => F[A7])(
      f: Z => (A1, A2, A3, A4, A5, A6, A7)
  )(implicit D: Divide[F]): F[Z] = D.divide2(a0, D.tuple2(a1, D.tuple2(a2, D.tuple2(a3, D.tuple2(a4, D.tuple2(a5, a6)))))) { z =>
    val t = f(z)
    (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, t._7))))))
  }

  def divide8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, Z](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6], a6: => F[A7], a7: => F[A8])(
      f: Z => (A1, A2, A3, A4, A5, A6, A7, A8)
  )(implicit D: Divide[F]): F[Z] = D.divide2(a0, D.tuple2(a1, D.tuple2(a2, D.tuple2(a3, D.tuple2(a4, D.tuple2(a5, D.tuple2(a6, a7))))))) { z =>
    val t = f(z)
    (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, (t._7, t._8)))))))
  }

  def divide9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6], a6: => F[A7], a7: => F[A8], a8: => F[A9])(
      f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9)
  )(implicit D: Divide[F]): F[Z] = D.divide2(a0, D.tuple2(a1, D.tuple2(a2, D.tuple2(a3, D.tuple2(a4, D.tuple2(a5, D.tuple2(a6, D.tuple2(a7, a8)))))))) { z =>
    val t = f(z)
    (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, (t._7, (t._8, t._9))))))))
  }

  def divide10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10]
  )(
      f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)
  )(implicit D: Divide[F]): F[Z] = D.divide2(a0, D.tuple2(a1, D.tuple2(a2, D.tuple2(a3, D.tuple2(a4, D.tuple2(a5, D.tuple2(a6, D.tuple2(a7, D.tuple2(a8, a9))))))))) { z =>
    val t = f(z)
    (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, (t._7, (t._8, (t._9, t._10)))))))))
  }

  def divide11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11]
  )(
      f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)
  )(implicit D: Divide[F]): F[Z] = D.divide2(a0, D.tuple2(a1, D.tuple2(a2, D.tuple2(a3, D.tuple2(a4, D.tuple2(a5, D.tuple2(a6, D.tuple2(a7, D.tuple2(a8, D.tuple2(a9, a10)))))))))) { z =>
    val t = f(z)
    (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, (t._7, (t._8, (t._9, (t._10, t._11))))))))))
  }

  def divide12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12]
  )(
      f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)
  )(implicit D: Divide[F]): F[Z] = D.divide2(a0, D.tuple2(a1, D.tuple2(a2, D.tuple2(a3, D.tuple2(a4, D.tuple2(a5, D.tuple2(a6, D.tuple2(a7, D.tuple2(a8, D.tuple2(a9, D.tuple2(a10, a11))))))))))) {
    z =>
      val t = f(z)
      (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, (t._7, (t._8, (t._9, (t._10, (t._11, t._12)))))))))))
  }

  def divide13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13]
  )(
      f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)
  )(implicit D: Divide[F]): F[Z] =
    D.divide2(
      a0,
      D.tuple2(a1, D.tuple2(a2, D.tuple2(a3, D.tuple2(a4, D.tuple2(a5, D.tuple2(a6, D.tuple2(a7, D.tuple2(a8, D.tuple2(a9, D.tuple2(a10, D.tuple2(a11, a12)))))))))))
    ) { z =>
      val t = f(z)
      (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, (t._7, (t._8, (t._9, (t._10, (t._11, (t._12, t._13))))))))))))
    }

  def divide14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14]
  )(
      f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)
  )(implicit D: Divide[F]): F[Z] =
    D.divide2(
      a0,
      D.tuple2(
        a1,
        D.tuple2(a2, D.tuple2(a3, D.tuple2(a4, D.tuple2(a5, D.tuple2(a6, D.tuple2(a7, D.tuple2(a8, D.tuple2(a9, D.tuple2(a10, D.tuple2(a11, D.tuple2(a12, a13)))))))))))
      )
    ) { z =>
      val t = f(z)
      (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, (t._7, (t._8, (t._9, (t._10, (t._11, (t._12, (t._13, t._14)))))))))))))
    }

  def divide15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15]
  )(
      f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)
  )(implicit D: Divide[F]): F[Z] =
    D.divide2(
      a0,
      D.tuple2(
        a1,
        D.tuple2(
          a2,
          D.tuple2(a3, D.tuple2(a4, D.tuple2(a5, D.tuple2(a6, D.tuple2(a7, D.tuple2(a8, D.tuple2(a9, D.tuple2(a10, D.tuple2(a11, D.tuple2(a12, D.tuple2(a13, a14)))))))))))
        )
      )
    ) { z =>
      val t = f(z)
      (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, (t._7, (t._8, (t._9, (t._10, (t._11, (t._12, (t._13, (t._14, t._15))))))))))))))
    }

  def divide16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16]
  )(
      f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)
  )(implicit D: Divide[F]): F[Z] =
    D.divide2(
      a0,
      D.tuple2(
        a1,
        D.tuple2(
          a2,
          D.tuple2(
            a3,
            D.tuple2(a4, D.tuple2(a5, D.tuple2(a6, D.tuple2(a7, D.tuple2(a8, D.tuple2(a9, D.tuple2(a10, D.tuple2(a11, D.tuple2(a12, D.tuple2(a13, D.tuple2(a14, a15)))))))))))
          )
        )
      )
    ) { z =>
      val t = f(z)
      (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, (t._7, (t._8, (t._9, (t._10, (t._11, (t._12, (t._13, (t._14, (t._15, t._16)))))))))))))))
    }

  def divide17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17]
  )(
      f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)
  )(implicit D: Divide[F]): F[Z] =
    D.divide2(
      a0,
      D.tuple2(
        a1,
        D.tuple2(
          a2,
          D.tuple2(
            a3,
            D.tuple2(
              a4,
              D.tuple2(a5, D.tuple2(a6, D.tuple2(a7, D.tuple2(a8, D.tuple2(a9, D.tuple2(a10, D.tuple2(a11, D.tuple2(a12, D.tuple2(a13, D.tuple2(a14, D.tuple2(a15, a16)))))))))))
            )
          )
        )
      )
    ) { z =>
      val t = f(z)
      (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, (t._7, (t._8, (t._9, (t._10, (t._11, (t._12, (t._13, (t._14, (t._15, (t._16, t._17))))))))))))))))
    }

  def divide18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18]
  )(
      f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)
  )(implicit D: Divide[F]): F[Z] =
    D.divide2(
      a0,
      D.tuple2(
        a1,
        D.tuple2(
          a2,
          D.tuple2(
            a3,
            D.tuple2(
              a4,
              D.tuple2(
                a5,
                D.tuple2(a6, D.tuple2(a7, D.tuple2(a8, D.tuple2(a9, D.tuple2(a10, D.tuple2(a11, D.tuple2(a12, D.tuple2(a13, D.tuple2(a14, D.tuple2(a15, D.tuple2(a16, a17)))))))))))
              )
            )
          )
        )
      )
    ) { z =>
      val t = f(z)
      (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, (t._7, (t._8, (t._9, (t._10, (t._11, (t._12, (t._13, (t._14, (t._15, (t._16, (t._17, t._18)))))))))))))))))
    }

  def divide19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19]
  )(
      f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)
  )(implicit D: Divide[F]): F[Z] =
    D.divide2(
      a0,
      D.tuple2(
        a1,
        D.tuple2(
          a2,
          D.tuple2(
            a3,
            D.tuple2(
              a4,
              D.tuple2(
                a5,
                D.tuple2(
                  a6,
                  D.tuple2(a7, D.tuple2(a8, D.tuple2(a9, D.tuple2(a10, D.tuple2(a11, D.tuple2(a12, D.tuple2(a13, D.tuple2(a14, D.tuple2(a15, D.tuple2(a16, D.tuple2(a17, a18)))))))))))
                )
              )
            )
          )
        )
      )
    ) { z =>
      val t = f(z)
      (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, (t._7, (t._8, (t._9, (t._10, (t._11, (t._12, (t._13, (t._14, (t._15, (t._16, (t._17, (t._18, t._19))))))))))))))))))
    }

  def divide20[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19],
      a19: => F[A20]
  )(
      f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)
  )(implicit D: Divide[F]): F[Z] =
    D.divide2(
      a0,
      D.tuple2(
        a1,
        D.tuple2(
          a2,
          D.tuple2(
            a3,
            D.tuple2(
              a4,
              D.tuple2(
                a5,
                D.tuple2(
                  a6,
                  D.tuple2(
                    a7,
                    D.tuple2(a8, D.tuple2(a9, D.tuple2(a10, D.tuple2(a11, D.tuple2(a12, D.tuple2(a13, D.tuple2(a14, D.tuple2(a15, D.tuple2(a16, D.tuple2(a17, D.tuple2(a18, a19)))))))))))
                  )
                )
              )
            )
          )
        )
      )
    ) { z =>
      val t = f(z)
      (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, (t._7, (t._8, (t._9, (t._10, (t._11, (t._12, (t._13, (t._14, (t._15, (t._16, (t._17, (t._18, (t._19, t._20)))))))))))))))))))
    }

  def divide21[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19],
      a19: => F[A20],
      a20: => F[A21]
  )(
      f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)
  )(implicit D: Divide[F]): F[Z] =
    D.divide2(
      a0,
      D.tuple2(
        a1,
        D.tuple2(
          a2,
          D.tuple2(
            a3,
            D.tuple2(
              a4,
              D.tuple2(
                a5,
                D.tuple2(
                  a6,
                  D.tuple2(
                    a7,
                    D.tuple2(
                      a8,
                      D.tuple2(a9, D.tuple2(a10, D.tuple2(a11, D.tuple2(a12, D.tuple2(a13, D.tuple2(a14, D.tuple2(a15, D.tuple2(a16, D.tuple2(a17, D.tuple2(a18, D.tuple2(a19, a20)))))))))))
                    )
                  )
                )
              )
            )
          )
        )
      )
    ) { z =>
      val t = f(z)
      (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, (t._7, (t._8, (t._9, (t._10, (t._11, (t._12, (t._13, (t._14, (t._15, (t._16, (t._17, (t._18, (t._19, (t._20, t._21))))))))))))))))))))
    }

  def divide22[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19],
      a19: => F[A20],
      a20: => F[A21],
      a21: => F[A22]
  )(
      f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)
  )(implicit D: Divide[F]): F[Z] =
    D.divide2(
      a0,
      D.tuple2(
        a1,
        D.tuple2(
          a2,
          D.tuple2(
            a3,
            D.tuple2(
              a4,
              D.tuple2(
                a5,
                D.tuple2(
                  a6,
                  D.tuple2(
                    a7,
                    D.tuple2(
                      a8,
                      D.tuple2(
                        a9,
                        D.tuple2(a10, D.tuple2(a11, D.tuple2(a12, D.tuple2(a13, D.tuple2(a14, D.tuple2(a15, D.tuple2(a16, D.tuple2(a17, D.tuple2(a18, D.tuple2(a19, D.tuple2(a20, a21)))))))))))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ) { z =>
      val t = f(z)
      (t._1, (t._2, (t._3, (t._4, (t._5, (t._6, (t._7, (t._8, (t._9, (t._10, (t._11, (t._12, (t._13, (t._14, (t._15, (t._16, (t._17, (t._18, (t._19, (t._20, (t._21, t._22)))))))))))))))))))))
    }

  def choose2[F[_], Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: Z => (A1 \/ A2))(implicit D: Decidable[F]): F[Z] = D.choose2(a1, a2)(f)

  def choose3[F[_], Z, A1, A2, A3](a0: => F[A1], a1: => F[A2], a2: => F[A3])(
      f: Z => (A1 \/ (A2 \/ A3))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ A3)] = D.choose2(a1, a2)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose4[F[_], Z, A1, A2, A3, A4](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4])(
      f: Z => (A1 \/ (A2 \/ (A3 \/ A4)))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ A4))] = choose3(a1, a2, a3)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose5[F[_], Z, A1, A2, A3, A4, A5](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5])(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ A5))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ A5)))] = choose4(a1, a2, a3, a4)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose6[F[_], Z, A1, A2, A3, A4, A5, A6](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6])(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6)))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ A6))))] = choose5(a1, a2, a3, a4, a5)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose7[F[_], Z, A1, A2, A3, A4, A5, A6, A7](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6], a6: => F[A7])(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7)))))] = choose6(a1, a2, a3, a4, a5, a6)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose8[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6], a6: => F[A7], a7: => F[A8])(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8)))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))))] = choose7(a1, a2, a3, a4, a5, a6, a7)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose9[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6], a6: => F[A7], a7: => F[A8], a8: => F[A9])(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9))))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))] = choose8(a1, a2, a3, a4, a5, a6, a7, a8)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose10[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10]
  )(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10)))))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))] = choose9(a1, a2, a3, a4, a5, a6, a7, a8, a9)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose11[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11]
  )(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11))))))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))] = choose10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose12[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12]
  )(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))] = choose11(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose13[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13]
  )(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))] = choose12(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose14[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14]
  )(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))] = choose13(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose15[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15]
  )(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15))))))))))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))] =
      choose14(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose16[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16]
  )(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16)))))))))))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))] =
      choose15(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose17[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17]
  )(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17))))))))))))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))] =
      choose16(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose18[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18]
  )(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))] =
      choose17(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose19[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19]
  )(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))] =
      choose18(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose20[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19],
      a19: => F[A20]
  )(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))] =
      choose19(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose21[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19],
      a19: => F[A20],
      a20: => F[A21]
  )(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))] =
      choose20(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(identity)
    D.choose2(a0, tail)(f)
  }

  def choose22[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19],
      a19: => F[A20],
      a20: => F[A21],
      a21: => F[A22]
  )(
      f: Z => (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))
  )(implicit D: Decidable[F]): F[Z] = {
    val tail: F[(A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))] =
      choose21(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)(identity)
    D.choose2(a0, tail)(f)
  }

  def altly2[F[_], Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: A1 \/ A2 => Z)(implicit A: Alt[F]): F[Z] = A.altly2(a1, a2)(f)

  def altly3[F[_], Z, A1, A2, A3](a0: => F[A1], a1: => F[A2], a2: => F[A3])(
      f: (A1 \/ (A2 \/ A3)) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, A.altly2(a1, a2)(identity))(f)

  def altly4[F[_], Z, A1, A2, A3, A4](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4])(
      f: (A1 \/ (A2 \/ (A3 \/ A4))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly3(a1, a2, a3)(identity))(f)

  def altly5[F[_], Z, A1, A2, A3, A4, A5](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5])(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ A5)))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly4(a1, a2, a3, a4)(identity))(f)

  def altly6[F[_], Z, A1, A2, A3, A4, A5, A6](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6])(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly5(a1, a2, a3, a4, a5)(identity))(f)

  def altly7[F[_], Z, A1, A2, A3, A4, A5, A6, A7](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6], a6: => F[A7])(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7)))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly6(a1, a2, a3, a4, a5, a6)(identity))(f)

  def altly8[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6], a6: => F[A7], a7: => F[A8])(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly7(a1, a2, a3, a4, a5, a6, a7)(identity))(f)

  def altly9[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6], a6: => F[A7], a7: => F[A8], a8: => F[A9])(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly8(a1, a2, a3, a4, a5, a6, a7, a8)(identity))(f)

  def altly10[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10]
  )(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly9(a1, a2, a3, a4, a5, a6, a7, a8, a9)(identity))(f)

  def altly11[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11]
  )(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)(identity))(f)

  def altly12[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12]
  )(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly11(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(identity))(f)

  def altly13[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13]
  )(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly12(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(identity))(f)

  def altly14[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14]
  )(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly13(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(identity))(f)

  def altly15[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15]
  )(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly14(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(identity))(f)

  def altly16[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16]
  )(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly15(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)(identity))(f)

  def altly17[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17]
  )(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly16(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)(identity))(f)

  def altly18[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18]
  )(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly17(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)(identity))(f)

  def altly19[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19]
  )(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly18(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)(identity))(f)

  def altly20[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19],
      a19: => F[A20]
  )(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly19(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)(identity))(f)

  def altly21[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19],
      a19: => F[A20],
      a20: => F[A21]
  )(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly20(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(identity))(f)

  def altly22[F[_], Z, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19],
      a19: => F[A20],
      a20: => F[A21],
      a21: => F[A22]
  )(
      f: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) => Z
  )(implicit A: Alt[F]): F[Z] =
    A.altly2(a0, altly21(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)(identity))(f)

  def apply2[F[_], A1, A2, R](a1: => F[A1], a2: => F[A2])(f: (A1, A2) => R)(implicit A: Apply[F]): F[R] = A.apply2(a1, a2)(f)

  def apply3[F[_], A1, A2, A3, R](a0: => F[A1], a1: => F[A2], a2: => F[A3])(f: (A1, A2, A3) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a2)(ap(a1)(map(a0)(f.curried)))
  }

  def apply4[F[_], A1, A2, A3, A4, R](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4])(f: (A1, A2, A3, A4) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried))))
  }

  def apply5[F[_], A1, A2, A3, A4, A5, R](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5])(f: (A1, A2, A3, A4, A5) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried)))))
  }

  def apply6[F[_], A1, A2, A3, A4, A5, A6, R](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6])(f: (A1, A2, A3, A4, A5, A6) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried))))))
  }

  def apply7[F[_], A1, A2, A3, A4, A5, A6, A7, R](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6], a6: => F[A7])(
      f: (A1, A2, A3, A4, A5, A6, A7) => R
  )(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried)))))))
  }

  def apply8[F[_], A1, A2, A3, A4, A5, A6, A7, A8, R](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6], a6: => F[A7], a7: => F[A8])(
      f: (A1, A2, A3, A4, A5, A6, A7, A8) => R
  )(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried))))))))
  }

  def apply9[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, R](a0: => F[A1], a1: => F[A2], a2: => F[A3], a3: => F[A4], a4: => F[A5], a5: => F[A6], a6: => F[A7], a7: => F[A8], a8: => F[A9])(
      f: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => R
  )(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried)))))))))
  }

  def apply10[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, R](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10]
  )(f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried))))))))))
  }

  def apply11[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, R](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11]
  )(f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a10)(ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried)))))))))))
  }

  def apply12[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, R](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12]
  )(f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a11)(ap(a10)(ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried))))))))))))
  }

  def apply13[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, R](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13]
  )(f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a12)(ap(a11)(ap(a10)(ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried)))))))))))))
  }

  def apply14[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, R](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14]
  )(f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a13)(ap(a12)(ap(a11)(ap(a10)(ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried))))))))))))))
  }

  def apply15[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, R](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15]
  )(f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried)))))))))))))))
  }

  def apply16[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, R](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16]
  )(f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a15)(ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried))))))))))))))))
  }

  def apply17[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, R](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17]
  )(f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a16)(ap(a15)(ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried)))))))))))))))))
  }

  def apply18[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, R](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18]
  )(f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a17)(ap(a16)(ap(a15)(ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried))))))))))))))))))
  }

  def apply19[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, R](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19]
  )(f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a18)(ap(a17)(ap(a16)(ap(a15)(ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried)))))))))))))))))))
  }

  def apply20[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, R](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19],
      a19: => F[A20]
  )(f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a19)(ap(a18)(ap(a17)(ap(a16)(ap(a15)(ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried))))))))))))))))))))
  }

  def apply21[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, R](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19],
      a19: => F[A20],
      a20: => F[A21]
  )(f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a20)(ap(a19)(ap(a18)(ap(a17)(ap(a16)(ap(a15)(ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried)))))))))))))))))))))
  }

  def apply22[F[_], A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, R](
      a0: => F[A1],
      a1: => F[A2],
      a2: => F[A3],
      a3: => F[A4],
      a4: => F[A5],
      a5: => F[A6],
      a6: => F[A7],
      a7: => F[A8],
      a8: => F[A9],
      a9: => F[A10],
      a10: => F[A11],
      a11: => F[A12],
      a12: => F[A13],
      a13: => F[A14],
      a14: => F[A15],
      a15: => F[A16],
      a16: => F[A17],
      a17: => F[A18],
      a18: => F[A19],
      a19: => F[A20],
      a20: => F[A21],
      a21: => F[A22]
  )(f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => R)(implicit A: Apply[F]): F[R] = {
    import A._
    ap(a21)(
      ap(a20)(ap(a19)(ap(a18)(ap(a17)(ap(a16)(ap(a15)(ap(a14)(ap(a13)(ap(a12)(ap(a11)(ap(a10)(ap(a9)(ap(a8)(ap(a7)(ap(a6)(ap(a5)(ap(a4)(ap(a3)(ap(a2)(ap(a1)(map(a0)(f.curried)))))))))))))))))))))
    )
  }

}
