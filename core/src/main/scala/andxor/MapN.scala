package andxor


import andxor.tuple._
import scalaz.\/

trait Map2P[A1, A2] {
    
  def map1[B](p: (A1, A2))(f: A1 => B): (B, A2) = {
    val a0 = p.t1
    val a1 = p.t2
    (f(a0), a1)
  }


  def map2[B](p: (A1, A2))(f: A2 => B): (A1, B) = {
    val a0 = p.t1
    val a1 = p.t2
    (a0, f(a1))
  }

  }


  trait Map2C[A1, A2] {
    
  def map1[B](c: (A1 \/ A2))(f: A1 => B): (B \/ A2) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ A2))(f: A2 => B): (A1 \/ B) =
    c.map(f)

  }

trait Map3P[A1, A2, A3] {
    
  def map1[B](p: (A1, (A2, A3)))(f: A1 => B): (B, (A2, A3)) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    (f(a0), (a1, a2))
  }


  def map2[B](p: (A1, (A2, A3)))(f: A2 => B): (A1, (B, A3)) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    (a0, (f(a1), a2))
  }


  def map3[B](p: (A1, (A2, A3)))(f: A3 => B): (A1, (A2, B)) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    (a0, (a1, f(a2)))
  }

  }


  trait Map3C[A1, A2, A3] {
    
  def map1[B](c: (A1 \/ (A2 \/ A3)))(f: A1 => B): (B \/ (A2 \/ A3)) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ A3)))(f: A2 => B): (A1 \/ (B \/ A3)) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ A3)))(f: A3 => B): (A1 \/ (A2 \/ B)) =
    c.map(_.map(f))

  }

trait Map4P[A1, A2, A3, A4] {
    
  def map1[B](p: (A1, (A2, (A3, A4))))(f: A1 => B): (B, (A2, (A3, A4))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    (f(a0), (a1, (a2, a3)))
  }


  def map2[B](p: (A1, (A2, (A3, A4))))(f: A2 => B): (A1, (B, (A3, A4))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    (a0, (f(a1), (a2, a3)))
  }


  def map3[B](p: (A1, (A2, (A3, A4))))(f: A3 => B): (A1, (A2, (B, A4))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    (a0, (a1, (f(a2), a3)))
  }


  def map4[B](p: (A1, (A2, (A3, A4))))(f: A4 => B): (A1, (A2, (A3, B))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    (a0, (a1, (a2, f(a3))))
  }

  }


  trait Map4C[A1, A2, A3, A4] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ A4))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ A4))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ A4))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ A4))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ A4))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ A4))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ A4))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ B))) =
    c.map(_.map(_.map(f)))

  }

trait Map5P[A1, A2, A3, A4, A5] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, A5)))))(f: A1 => B): (B, (A2, (A3, (A4, A5)))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    (f(a0), (a1, (a2, (a3, a4))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, A5)))))(f: A2 => B): (A1, (B, (A3, (A4, A5)))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    (a0, (f(a1), (a2, (a3, a4))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, A5)))))(f: A3 => B): (A1, (A2, (B, (A4, A5)))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    (a0, (a1, (f(a2), (a3, a4))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, A5)))))(f: A4 => B): (A1, (A2, (A3, (B, A5)))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    (a0, (a1, (a2, (f(a3), a4))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, A5)))))(f: A5 => B): (A1, (A2, (A3, (A4, B)))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    (a0, (a1, (a2, (a3, f(a4)))))
  }

  }


  trait Map5C[A1, A2, A3, A4, A5] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ A5)))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ A5)))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ A5)))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ A5)))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ A5)))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ A5)))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ A5)))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ A5)))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ A5)))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ B)))) =
    c.map(_.map(_.map(_.map(f))))

  }

trait Map6P[A1, A2, A3, A4, A5, A6] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, A6))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, A6))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    (f(a0), (a1, (a2, (a3, (a4, a5)))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, A6))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, A6))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    (a0, (f(a1), (a2, (a3, (a4, a5)))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, A6))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, A6))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    (a0, (a1, (f(a2), (a3, (a4, a5)))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, A6))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, A6))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    (a0, (a1, (a2, (f(a3), (a4, a5)))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, A6))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, A6))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    (a0, (a1, (a2, (a3, (f(a4), a5)))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, A6))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, B))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    (a0, (a1, (a2, (a3, (a4, f(a5))))))
  }

  }


  trait Map6C[A1, A2, A3, A4, A5, A6] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ A6))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ A6))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ A6))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ A6))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ B))))) =
    c.map(_.map(_.map(_.map(_.map(f)))))

  }

trait Map7P[A1, A2, A3, A4, A5, A6, A7] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, A7)))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, A7)))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    (f(a0), (a1, (a2, (a3, (a4, (a5, a6))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, A7)))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, A7)))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    (a0, (f(a1), (a2, (a3, (a4, (a5, a6))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, A7)))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, A7)))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    (a0, (a1, (f(a2), (a3, (a4, (a5, a6))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, A7)))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, A7)))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    (a0, (a1, (a2, (f(a3), (a4, (a5, a6))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, A7)))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, A7)))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    (a0, (a1, (a2, (a3, (f(a4), (a5, a6))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, A7)))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, A7)))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    (a0, (a1, (a2, (a3, (a4, (f(a5), a6))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, A7)))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, B)))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    (a0, (a1, (a2, (a3, (a4, (a5, f(a6)))))))
  }

  }


  trait Map7C[A1, A2, A3, A4, A5, A6, A7] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7)))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7)))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7)))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7)))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7)))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ A7)))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7)))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ A7)))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7)))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ A7)))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7)))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ A7)))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7)))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ B)))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(f))))))

  }

trait Map8P[A1, A2, A3, A4, A5, A6, A7, A8] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, A8))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, A8))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, a7)))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, A8))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, A8))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, a7)))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, A8))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, A8))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, a7)))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, A8))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, A8))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, a7)))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, A8))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, A8))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, a7)))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, A8))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, A8))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, a7)))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, A8))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, A8))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), a7)))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, A8))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, B))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, f(a7))))))))
  }

  }


  trait Map8C[A1, A2, A3, A4, A5, A6, A7, A8] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ A8))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ A8))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ A8))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ A8))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ B))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))

  }

trait Map9P[A1, A2, A3, A4, A5, A6, A7, A8, A9] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, A9)))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, A9)))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, a8))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, A9)))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, A9)))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, a8))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, A9)))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, A9)))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, a8))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, A9)))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, A9)))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, a8))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, A9)))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, A9)))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, a8))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, A9)))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, A9)))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, a8))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, A9)))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, A9)))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, a8))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, A9)))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, A9)))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), a8))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, A9)))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, B)))))))) = {
    val a0 = p.t1
    val a1 = p.t2
    val a2 = p.t3
    val a3 = p.t4
    val a4 = p.t5
    val a5 = p.t6
    val a6 = p.t7
    val a7 = p.t8
    val a8 = p.t9
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, f(a8)))))))))
  }

  }


  trait Map9C[A1, A2, A3, A4, A5, A6, A7, A8, A9] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ A9)))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ A9)))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ A9)))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9)))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ B)))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))

  }

trait Map10P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, A10))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, A10))))))))) = {
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
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, a9)))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, A10))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, A10))))))))) = {
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
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, a9)))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, A10))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, A10))))))))) = {
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
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, a9)))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, A10))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, A10))))))))) = {
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
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, a9)))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, A10))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, A10))))))))) = {
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
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, a9)))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, A10))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, A10))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, a9)))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, A10))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, A10))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, a9)))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, A10))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, A10))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, a9)))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, A10))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, A10))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), a9)))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, A10))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, B))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, f(a9))))))))))
  }

  }


  trait Map10C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ A10))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ A10))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ A10))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ B))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))

  }

trait Map11P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11)))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11)))))))))) = {
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
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, a10))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11)))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11)))))))))) = {
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
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, a10))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11)))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11)))))))))) = {
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
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, a10))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11)))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, A11)))))))))) = {
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
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, a10))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11)))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, A11)))))))))) = {
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
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, a10))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11)))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, A11)))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, a10))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11)))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, A11)))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, a10))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11)))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, A11)))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, a10))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11)))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, A11)))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, a10))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11)))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, A11)))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), a10))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11)))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, B)))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, f(a10)))))))))))
  }

  }


  trait Map11C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ A11)))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ A11)))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ A11)))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11)))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ B)))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))

  }

trait Map12P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))) = {
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
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, a11)))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))) = {
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
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, a11)))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))) = {
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
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, a11)))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))) = {
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
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, a11)))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))) = {
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
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, a11)))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, A12))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, a11)))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, A12))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, a11)))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, A12))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, a11)))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, A12))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, a11)))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, A12))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, a11)))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, A12))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), a11)))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, B))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, f(a11))))))))))))
  }

  }


  trait Map12C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ A12))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ A12))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ A12))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ B))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))

  }

trait Map13P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))) = {
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
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, a12))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))) = {
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
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, a12))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))) = {
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
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, a12))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))) = {
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
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, a12))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))) = {
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
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, a12))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, a12))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, A13)))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, a12))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, A13)))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, a12))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, A13)))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, a12))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, A13)))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, a12))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, A13)))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, a12))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, A13)))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), a12))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13)))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, B)))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, f(a12)))))))))))))
  }

  }


  trait Map13C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ A13)))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ A13)))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ A13)))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13)))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ B)))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))

  }

trait Map14P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))) = {
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
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, a13)))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))) = {
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
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, a13)))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))) = {
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
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, a13)))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))) = {
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
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, a13)))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, a13)))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, a13)))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, a13)))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, A14))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, a13)))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, A14))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, a13)))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, A14))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, a13)))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, A14))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, a13)))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, A14))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, a13)))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, A14))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), a13)))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, B))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, f(a13))))))))))))))
  }

  }


  trait Map14C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ A14))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ A14))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ A14))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ B))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))

  }

trait Map15P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))) = {
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
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, a14))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))) = {
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
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, a14))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))) = {
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
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, a14))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))) = {
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
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, a14))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, a14))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, a14))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, a14))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, a14))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, a14))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, A15)))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, a14))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, A15)))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, a14))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, A15)))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, a14))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, A15)))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, a14))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, A15)))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), a14))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15)))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, B)))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, f(a14)))))))))))))))
  }

  }


  trait Map15C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ A15)))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ A15)))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ A15)))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15)))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ B)))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))

  }

trait Map16P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))) = {
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
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, a15)))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))) = {
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
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, a15)))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))) = {
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
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, a15)))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))) = {
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
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, a15)))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, a15)))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, a15)))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, a15)))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, (a14, a15)))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, (a14, a15)))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, (a14, a15)))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, A16))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, (a14, a15)))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, A16))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, (a14, a15)))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, A16))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, (a14, a15)))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, A16))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), (a14, a15)))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, A16))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (f(a14), a15)))))))))))))))
  }


  def map16[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16))))))))))))))))(f: A16 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, B))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, f(a15))))))))))))))))
  }

  }


  trait Map16C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ A16))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ A16))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ A16))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))


  def map16[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16))))))))))))))))(f: A16 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ B))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))

  }

trait Map17P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))) = {
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
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, a16))))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))) = {
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
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, a16))))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))) = {
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
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, a16))))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))) = {
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
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, a16))))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, a16))))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, a16))))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, a16))))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, a16))))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, (a14, (a15, a16))))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, (a14, (a15, a16))))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, (a14, (a15, a16))))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, A17)))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, (a14, (a15, a16))))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, A17)))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, (a14, (a15, a16))))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, A17)))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), (a14, (a15, a16))))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, A17)))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (f(a14), (a15, a16))))))))))))))))
  }


  def map16[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A16 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, A17)))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (f(a15), a16))))))))))))))))
  }


  def map17[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17)))))))))))))))))(f: A17 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, B)))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, f(a16)))))))))))))))))
  }

  }


  trait Map17C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ A17)))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ A17)))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))


  def map16[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A16 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ A17)))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))


  def map17[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17)))))))))))))))))(f: A17 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ B)))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))))

  }

trait Map18P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))) = {
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
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, a17)))))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))) = {
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
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, a17)))))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))) = {
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
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, a17)))))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))) = {
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
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, a17)))))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, a17)))))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, a17)))))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, a17)))))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, a17)))))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, a17)))))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, (a14, (a15, (a16, a17)))))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, (a14, (a15, (a16, a17)))))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, (a14, (a15, (a16, a17)))))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, A18))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, (a14, (a15, (a16, a17)))))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, A18))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), (a14, (a15, (a16, a17)))))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, A18))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (f(a14), (a15, (a16, a17)))))))))))))))))
  }


  def map16[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A16 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, A18))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (f(a15), (a16, a17)))))))))))))))))
  }


  def map17[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A17 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, A18))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (f(a16), a17)))))))))))))))))
  }


  def map18[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18))))))))))))))))))(f: A18 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, B))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, f(a17))))))))))))))))))
  }

  }


  trait Map18C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ A18))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))


  def map16[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A16 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ A18))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))


  def map17[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A17 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ A18))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))


  def map18[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18))))))))))))))))))(f: A18 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ B))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))))

  }

trait Map19P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))) = {
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
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, a18))))))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))) = {
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
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, a18))))))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))) = {
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
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, a18))))))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))) = {
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
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, a18))))))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, a18))))))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, a18))))))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, a18))))))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, a18))))))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, a18))))))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, a18))))))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, (a14, (a15, (a16, (a17, a18))))))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, (a14, (a15, (a16, (a17, a18))))))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, (a14, (a15, (a16, (a17, a18))))))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, A19)))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), (a14, (a15, (a16, (a17, a18))))))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, A19)))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (f(a14), (a15, (a16, (a17, a18))))))))))))))))))
  }


  def map16[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A16 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, A19)))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (f(a15), (a16, (a17, a18))))))))))))))))))
  }


  def map17[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A17 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, A19)))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (f(a16), (a17, a18))))))))))))))))))
  }


  def map18[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A18 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, A19)))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (f(a17), a18))))))))))))))))))
  }


  def map19[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19)))))))))))))))))))(f: A19 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, B)))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, f(a18)))))))))))))))))))
  }

  }


  trait Map19C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))


  def map16[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A16 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))


  def map17[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A17 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ A19)))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))


  def map18[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A18 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ A19)))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))


  def map19[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19)))))))))))))))))))(f: A19 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ B)))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))))))

  }

trait Map20P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, (a14, (a15, (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, (a14, (a15, (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), (a14, (a15, (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (f(a14), (a15, (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map16[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A16 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (f(a15), (a16, (a17, (a18, a19)))))))))))))))))))
  }


  def map17[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A17 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (f(a16), (a17, (a18, a19)))))))))))))))))))
  }


  def map18[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A18 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (f(a17), (a18, a19)))))))))))))))))))
  }


  def map19[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A19 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, A20))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (f(a18), a19)))))))))))))))))))
  }


  def map20[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20))))))))))))))))))))(f: A20 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, B))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, f(a19))))))))))))))))))))
  }

  }


  trait Map20C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))


  def map16[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A16 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))


  def map17[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A17 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))


  def map18[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A18 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))


  def map19[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A19 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ A20))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))


  def map20[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20))))))))))))))))))))(f: A20 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ B))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))))))

  }

trait Map21P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, (a14, (a15, (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), (a14, (a15, (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (f(a14), (a15, (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map16[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A16 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (f(a15), (a16, (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map17[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A17 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (f(a16), (a17, (a18, (a19, a20))))))))))))))))))))
  }


  def map18[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A18 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (f(a17), (a18, (a19, a20))))))))))))))))))))
  }


  def map19[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A19 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (f(a18), (a19, a20))))))))))))))))))))
  }


  def map20[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A20 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, A21)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (f(a19), a20))))))))))))))))))))
  }


  def map21[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21)))))))))))))))))))))(f: A21 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, B)))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, f(a20)))))))))))))))))))))
  }

  }


  trait Map21C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))


  def map16[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A16 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))


  def map17[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A17 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))


  def map18[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A18 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))


  def map19[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A19 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))


  def map20[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A20 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ A21)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))


  def map21[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21)))))))))))))))))))))(f: A21 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ B)))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))))))))

  }

trait Map22P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), (a14, (a15, (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (f(a14), (a15, (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map16[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A16 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (f(a15), (a16, (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map17[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A17 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (f(a16), (a17, (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map18[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A18 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (f(a17), (a18, (a19, (a20, a21)))))))))))))))))))))
  }


  def map19[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A19 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (f(a18), (a19, (a20, a21)))))))))))))))))))))
  }


  def map20[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A20 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (f(a19), (a20, a21)))))))))))))))))))))
  }


  def map21[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A21 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, A22))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (f(a20), a21)))))))))))))))))))))
  }


  def map22[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22))))))))))))))))))))))(f: A22 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, B))))))))))))))))))))) = {
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
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, f(a21))))))))))))))))))))))
  }

  }


  trait Map22C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))


  def map16[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A16 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))


  def map17[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A17 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))


  def map18[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A18 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))


  def map19[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A19 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))


  def map20[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A20 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))


  def map21[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A21 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ A22))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))


  def map22[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22))))))))))))))))))))))(f: A22 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ B))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))))))))

  }

trait Map23P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (f(a14), (a15, (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map16[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A16 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (f(a15), (a16, (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map17[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A17 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (f(a16), (a17, (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map18[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A18 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (f(a17), (a18, (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map19[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A19 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (f(a18), (a19, (a20, (a21, a22))))))))))))))))))))))
  }


  def map20[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A20 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (f(a19), (a20, (a21, a22))))))))))))))))))))))
  }


  def map21[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A21 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (f(a20), (a21, a22))))))))))))))))))))))
  }


  def map22[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A22 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, A23)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (f(a21), a22))))))))))))))))))))))
  }


  def map23[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23)))))))))))))))))))))))(f: A23 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, B)))))))))))))))))))))) = {
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
    val a22 = p.t23
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, f(a22)))))))))))))))))))))))
  }

  }


  trait Map23C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))


  def map16[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A16 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))


  def map17[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A17 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))


  def map18[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A18 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))


  def map19[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A19 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))


  def map20[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A20 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))


  def map21[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A21 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))


  def map22[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A22 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ A23)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))


  def map23[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23)))))))))))))))))))))))(f: A23 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ B)))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))))))))))

  }

trait Map24P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (f(a14), (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map16[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A16 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (f(a15), (a16, (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map17[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A17 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (f(a16), (a17, (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map18[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A18 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (f(a17), (a18, (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map19[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A19 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (f(a18), (a19, (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map20[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A20 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (f(a19), (a20, (a21, (a22, a23)))))))))))))))))))))))
  }


  def map21[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A21 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (f(a20), (a21, (a22, a23)))))))))))))))))))))))
  }


  def map22[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A22 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, (A23, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (f(a21), (a22, a23)))))))))))))))))))))))
  }


  def map23[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A23 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (B, A24))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (f(a22), a23)))))))))))))))))))))))
  }


  def map24[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24))))))))))))))))))))))))(f: A24 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, B))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, f(a23))))))))))))))))))))))))
  }

  }


  trait Map24C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))


  def map16[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A16 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))


  def map17[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A17 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))


  def map18[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A18 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))


  def map19[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A19 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))


  def map20[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A20 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))


  def map21[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A21 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))


  def map22[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A22 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ (A23 \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))


  def map23[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A23 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (B \/ A24))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))


  def map24[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24))))))))))))))))))))))))(f: A24 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ B))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))))))))))

  }

trait Map25P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (f(a14), (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map16[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A16 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (f(a15), (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map17[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A17 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (f(a16), (a17, (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map18[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A18 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (f(a17), (a18, (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map19[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A19 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (f(a18), (a19, (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map20[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A20 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (f(a19), (a20, (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map21[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A21 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (f(a20), (a21, (a22, (a23, a24))))))))))))))))))))))))
  }


  def map22[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A22 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, (A23, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (f(a21), (a22, (a23, a24))))))))))))))))))))))))
  }


  def map23[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A23 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (B, (A24, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (f(a22), (a23, a24))))))))))))))))))))))))
  }


  def map24[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A24 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (B, A25)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (f(a23), a24))))))))))))))))))))))))
  }


  def map25[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25)))))))))))))))))))))))))(f: A25 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, B)))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, f(a24)))))))))))))))))))))))))
  }

  }


  trait Map25C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))


  def map16[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A16 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))


  def map17[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A17 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))


  def map18[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A18 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))


  def map19[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A19 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))


  def map20[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A20 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))


  def map21[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A21 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))


  def map22[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A22 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))


  def map23[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A23 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (B \/ (A24 \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))


  def map24[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A24 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (B \/ A25)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))))


  def map25[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25)))))))))))))))))))))))))(f: A25 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ B)))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))))))))))))

  }

trait Map26P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (f(a14), (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map16[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A16 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (f(a15), (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map17[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A17 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (f(a16), (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map18[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A18 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (f(a17), (a18, (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map19[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A19 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (f(a18), (a19, (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map20[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A20 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (f(a19), (a20, (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map21[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A21 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (f(a20), (a21, (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map22[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A22 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, (A23, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (f(a21), (a22, (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map23[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A23 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (B, (A24, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (f(a22), (a23, (a24, a25)))))))))))))))))))))))))
  }


  def map24[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A24 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (B, (A25, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (f(a23), (a24, a25)))))))))))))))))))))))))
  }


  def map25[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A25 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (B, A26))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (f(a24), a25)))))))))))))))))))))))))
  }


  def map26[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26))))))))))))))))))))))))))(f: A26 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, B))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, f(a25))))))))))))))))))))))))))
  }

  }


  trait Map26C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))


  def map16[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A16 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))


  def map17[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A17 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))


  def map18[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A18 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))


  def map19[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A19 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))


  def map20[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A20 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))


  def map21[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A21 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))


  def map22[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A22 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))


  def map23[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A23 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (B \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))


  def map24[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A24 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (B \/ (A25 \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))))


  def map25[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A25 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (B \/ A26))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))))


  def map26[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26))))))))))))))))))))))))))(f: A26 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ B))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))))))))))))

  }

trait Map27P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (f(a14), (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map16[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A16 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (f(a15), (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map17[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A17 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (f(a16), (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map18[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A18 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (f(a17), (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map19[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A19 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (f(a18), (a19, (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map20[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A20 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (f(a19), (a20, (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map21[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A21 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (f(a20), (a21, (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map22[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A22 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (f(a21), (a22, (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map23[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A23 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (B, (A24, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (f(a22), (a23, (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map24[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A24 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (B, (A25, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (f(a23), (a24, (a25, a26))))))))))))))))))))))))))
  }


  def map25[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A25 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (B, (A26, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (f(a24), (a25, a26))))))))))))))))))))))))))
  }


  def map26[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A26 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (B, A27)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (f(a25), a26))))))))))))))))))))))))))
  }


  def map27[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27)))))))))))))))))))))))))))(f: A27 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, B)))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, f(a26)))))))))))))))))))))))))))
  }

  }


  trait Map27C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))


  def map16[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A16 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))


  def map17[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A17 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))


  def map18[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A18 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))


  def map19[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A19 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))


  def map20[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A20 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))


  def map21[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A21 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))


  def map22[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A22 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))


  def map23[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A23 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (B \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))


  def map24[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A24 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (B \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))))


  def map25[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A25 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (B \/ (A26 \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))))


  def map26[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A26 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (B \/ A27)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))))))


  def map27[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27)))))))))))))))))))))))))))(f: A27 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ B)))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))))))))))))))

  }

trait Map28P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (f(a14), (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map16[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A16 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (f(a15), (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map17[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A17 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (f(a16), (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map18[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A18 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (f(a17), (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map19[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A19 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (f(a18), (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map20[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A20 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (f(a19), (a20, (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map21[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A21 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (f(a20), (a21, (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map22[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A22 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (f(a21), (a22, (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map23[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A23 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (B, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (f(a22), (a23, (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map24[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A24 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (B, (A25, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (f(a23), (a24, (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map25[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A25 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (B, (A26, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (f(a24), (a25, (a26, a27)))))))))))))))))))))))))))
  }


  def map26[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A26 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (B, (A27, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (f(a25), (a26, a27)))))))))))))))))))))))))))
  }


  def map27[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A27 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (B, A28))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (f(a26), a27)))))))))))))))))))))))))))
  }


  def map28[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28))))))))))))))))))))))))))))(f: A28 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, B))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, f(a27))))))))))))))))))))))))))))
  }

  }


  trait Map28C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))


  def map16[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A16 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))


  def map17[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A17 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))


  def map18[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A18 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))


  def map19[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A19 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))


  def map20[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A20 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))


  def map21[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A21 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))


  def map22[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A22 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))


  def map23[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A23 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (B \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))


  def map24[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A24 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (B \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))))


  def map25[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A25 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (B \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))))


  def map26[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A26 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (B \/ (A27 \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))))))


  def map27[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A27 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (B \/ A28))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))))))


  def map28[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28))))))))))))))))))))))))))))(f: A28 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ B))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))))))))))))))

  }

trait Map29P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (f(a14), (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map16[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A16 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (f(a15), (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map17[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A17 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (f(a16), (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map18[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A18 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (f(a17), (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map19[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A19 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (f(a18), (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map20[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A20 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (f(a19), (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map21[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A21 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (f(a20), (a21, (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map22[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A22 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (f(a21), (a22, (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map23[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A23 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (B, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (f(a22), (a23, (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map24[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A24 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (B, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (f(a23), (a24, (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map25[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A25 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (B, (A26, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (f(a24), (a25, (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map26[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A26 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (B, (A27, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (f(a25), (a26, (a27, a28))))))))))))))))))))))))))))
  }


  def map27[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A27 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (B, (A28, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (f(a26), (a27, a28))))))))))))))))))))))))))))
  }


  def map28[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A28 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (B, A29)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (f(a27), a28))))))))))))))))))))))))))))
  }


  def map29[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29)))))))))))))))))))))))))))))(f: A29 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, B)))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, f(a28)))))))))))))))))))))))))))))
  }

  }


  trait Map29C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))


  def map16[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A16 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))


  def map17[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A17 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))


  def map18[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A18 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))


  def map19[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A19 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))


  def map20[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A20 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))


  def map21[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A21 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))


  def map22[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A22 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))


  def map23[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A23 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (B \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))


  def map24[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A24 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (B \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))))


  def map25[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A25 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (B \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))))


  def map26[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A26 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (B \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))))))


  def map27[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A27 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (B \/ (A28 \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))))))


  def map28[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A28 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (B \/ A29)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))))))))


  def map29[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29)))))))))))))))))))))))))))))(f: A29 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ B)))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f))))))))))))))))))))))))))))

  }

trait Map30P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30] {
    
  def map1[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A1 => B): (B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (f(a0), (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map2[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A2 => B): (A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (f(a1), (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map3[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A3 => B): (A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (f(a2), (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map4[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A4 => B): (A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (f(a3), (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map5[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A5 => B): (A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (f(a4), (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map6[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A6 => B): (A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (f(a5), (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map7[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A7 => B): (A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (f(a6), (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map8[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A8 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (f(a7), (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map9[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A9 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (f(a8), (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map10[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A10 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (f(a9), (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map11[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A11 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (f(a10), (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map12[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A12 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (f(a11), (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map13[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A13 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (f(a12), (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map14[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A14 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (f(a13), (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map15[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A15 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (f(a14), (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map16[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A16 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (f(a15), (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map17[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A17 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (f(a16), (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map18[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A18 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (f(a17), (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map19[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A19 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (f(a18), (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map20[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A20 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (f(a19), (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map21[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A21 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (f(a20), (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map22[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A22 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (f(a21), (a22, (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map23[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A23 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (B, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (f(a22), (a23, (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map24[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A24 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (B, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (f(a23), (a24, (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map25[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A25 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (B, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (f(a24), (a25, (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map26[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A26 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (B, (A27, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (f(a25), (a26, (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map27[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A27 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (B, (A28, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (f(a26), (a27, (a28, a29)))))))))))))))))))))))))))))
  }


  def map28[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A28 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (B, (A29, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (f(a27), (a28, a29)))))))))))))))))))))))))))))
  }


  def map29[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A29 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (B, A30))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (f(a28), a29)))))))))))))))))))))))))))))
  }


  def map30[B](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30))))))))))))))))))))))))))))))(f: A30 => B): (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, B))))))))))))))))))))))))))))) = {
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
    val a22 = p.t23
    val a23 = p.t24
    val a24 = p.t25
    val a25 = p.t26
    val a26 = p.t27
    val a27 = p.t28
    val a28 = p.t29
    val a29 = p.t30
    (a0, (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, (a11, (a12, (a13, (a14, (a15, (a16, (a17, (a18, (a19, (a20, (a21, (a22, (a23, (a24, (a25, (a26, (a27, (a28, f(a29))))))))))))))))))))))))))))))
  }

  }


  trait Map30C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30] {
    
  def map1[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A1 => B): (B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.leftMap(f)


  def map2[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A2 => B): (A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.leftMap(f))


  def map3[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A3 => B): (A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.leftMap(f)))


  def map4[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A4 => B): (A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.leftMap(f))))


  def map5[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A5 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.leftMap(f)))))


  def map6[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A6 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))


  def map7[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A7 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))


  def map8[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A8 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))


  def map9[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A9 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))


  def map10[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A10 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))


  def map11[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A11 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))


  def map12[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A12 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))


  def map13[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A13 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))


  def map14[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A14 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))


  def map15[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A15 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))


  def map16[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A16 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))


  def map17[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A17 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))


  def map18[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A18 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))


  def map19[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A19 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))


  def map20[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A20 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))


  def map21[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A21 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))


  def map22[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A22 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))


  def map23[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A23 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (B \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))


  def map24[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A24 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (B \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))))


  def map25[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A25 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (B \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))))


  def map26[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A26 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (B \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))))))


  def map27[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A27 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (B \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))))))


  def map28[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A28 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (B \/ (A29 \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f))))))))))))))))))))))))))))


  def map29[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A29 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (B \/ A30))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.leftMap(f)))))))))))))))))))))))))))))


  def map30[B](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30))))))))))))))))))))))))))))))(f: A30 => B): (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ B))))))))))))))))))))))))))))) =
    c.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(_.map(f)))))))))))))))))))))))))))))

  }



object MapN {

  object syntax {
    implicit class Map2POps[A1, A2](p: (A1, A2)) {
        val mapN: Map2P[A1, A2] = new Map2P[A1, A2] {}
        
  def map1[B](f: A1 => B): ((B, A2)) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, B)) =
    mapN.map2(p)(f)


      }


      implicit class Map2COps[A1, A2](c: (A1 \/ A2)) {
        val mapN: Map2C[A1, A2] = new Map2C[A1, A2] {}
        
  def map1[B](f: A1 => B): ((B \/ A2)) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ B)) =
    mapN.map2(c)(f)



      }

implicit class Map3POps[A1, A2, A3](p: (A1, (A2, A3))) {
        val mapN: Map3P[A1, A2, A3] = new Map3P[A1, A2, A3] {}
        
  def map1[B](f: A1 => B): ((B, (A2, A3))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, A3))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, B))) =
    mapN.map3(p)(f)


      }


      implicit class Map3COps[A1, A2, A3](c: (A1 \/ (A2 \/ A3))) {
        val mapN: Map3C[A1, A2, A3] = new Map3C[A1, A2, A3] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ A3))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ A3))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ B))) =
    mapN.map3(c)(f)



      }

implicit class Map4POps[A1, A2, A3, A4](p: (A1, (A2, (A3, A4)))) {
        val mapN: Map4P[A1, A2, A3, A4] = new Map4P[A1, A2, A3, A4] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, A4)))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, A4)))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, A4)))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, B)))) =
    mapN.map4(p)(f)


      }


      implicit class Map4COps[A1, A2, A3, A4](c: (A1 \/ (A2 \/ (A3 \/ A4)))) {
        val mapN: Map4C[A1, A2, A3, A4] = new Map4C[A1, A2, A3, A4] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ A4)))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ A4)))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ A4)))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ B)))) =
    mapN.map4(c)(f)



      }

implicit class Map5POps[A1, A2, A3, A4, A5](p: (A1, (A2, (A3, (A4, A5))))) {
        val mapN: Map5P[A1, A2, A3, A4, A5] = new Map5P[A1, A2, A3, A4, A5] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, A5))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, A5))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, A5))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, A5))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, B))))) =
    mapN.map5(p)(f)


      }


      implicit class Map5COps[A1, A2, A3, A4, A5](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ A5))))) {
        val mapN: Map5C[A1, A2, A3, A4, A5] = new Map5C[A1, A2, A3, A4, A5] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ A5))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ A5))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ A5))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ A5))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ B))))) =
    mapN.map5(c)(f)



      }

implicit class Map6POps[A1, A2, A3, A4, A5, A6](p: (A1, (A2, (A3, (A4, (A5, A6)))))) {
        val mapN: Map6P[A1, A2, A3, A4, A5, A6] = new Map6P[A1, A2, A3, A4, A5, A6] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, A6)))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, A6)))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, A6)))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, A6)))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, A6)))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, B)))))) =
    mapN.map6(p)(f)


      }


      implicit class Map6COps[A1, A2, A3, A4, A5, A6](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6)))))) {
        val mapN: Map6C[A1, A2, A3, A4, A5, A6] = new Map6C[A1, A2, A3, A4, A5, A6] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ A6)))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ A6)))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ A6)))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ A6)))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ A6)))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ B)))))) =
    mapN.map6(c)(f)



      }

implicit class Map7POps[A1, A2, A3, A4, A5, A6, A7](p: (A1, (A2, (A3, (A4, (A5, (A6, A7))))))) {
        val mapN: Map7P[A1, A2, A3, A4, A5, A6, A7] = new Map7P[A1, A2, A3, A4, A5, A6, A7] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, A7))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, A7))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, A7))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, A7))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, A7))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, A7))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, B))))))) =
    mapN.map7(p)(f)


      }


      implicit class Map7COps[A1, A2, A3, A4, A5, A6, A7](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7))))))) {
        val mapN: Map7C[A1, A2, A3, A4, A5, A6, A7] = new Map7C[A1, A2, A3, A4, A5, A6, A7] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ A7))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ A7))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ A7))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ A7))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ A7))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ B))))))) =
    mapN.map7(c)(f)



      }

implicit class Map8POps[A1, A2, A3, A4, A5, A6, A7, A8](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, A8)))))))) {
        val mapN: Map8P[A1, A2, A3, A4, A5, A6, A7, A8] = new Map8P[A1, A2, A3, A4, A5, A6, A7, A8] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, A8)))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, A8)))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, A8)))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, A8)))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, A8)))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, A8)))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, A8)))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, B)))))))) =
    mapN.map8(p)(f)


      }


      implicit class Map8COps[A1, A2, A3, A4, A5, A6, A7, A8](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8)))))))) {
        val mapN: Map8C[A1, A2, A3, A4, A5, A6, A7, A8] = new Map8C[A1, A2, A3, A4, A5, A6, A7, A8] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8)))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8)))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ A8)))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ A8)))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ A8)))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ A8)))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ A8)))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ B)))))))) =
    mapN.map8(c)(f)



      }

implicit class Map9POps[A1, A2, A3, A4, A5, A6, A7, A8, A9](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, A9))))))))) {
        val mapN: Map9P[A1, A2, A3, A4, A5, A6, A7, A8, A9] = new Map9P[A1, A2, A3, A4, A5, A6, A7, A8, A9] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, A9))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, A9))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, A9))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, A9))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, A9))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, A9))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, A9))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, A9))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, B))))))))) =
    mapN.map9(p)(f)


      }


      implicit class Map9COps[A1, A2, A3, A4, A5, A6, A7, A8, A9](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9))))))))) {
        val mapN: Map9C[A1, A2, A3, A4, A5, A6, A7, A8, A9] = new Map9C[A1, A2, A3, A4, A5, A6, A7, A8, A9] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ A9))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ A9))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ A9))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ A9))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ A9))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ B))))))))) =
    mapN.map9(c)(f)



      }

implicit class Map10POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, A10)))))))))) {
        val mapN: Map10P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = new Map10P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, A10)))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, A10)))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, A10)))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, A10)))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, A10)))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, A10)))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, A10)))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, A10)))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, A10)))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, B)))))))))) =
    mapN.map10(p)(f)


      }


      implicit class Map10COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10)))))))))) {
        val mapN: Map10C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = new Map10C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10)))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10)))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10)))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10)))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ A10)))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ A10)))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ A10)))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ A10)))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ A10)))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ B)))))))))) =
    mapN.map10(c)(f)



      }

implicit class Map11POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11))))))))))) {
        val mapN: Map11P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = new Map11P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, A11))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, A11))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, A11))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, A11))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, A11))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, A11))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, A11))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, A11))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, B))))))))))) =
    mapN.map11(p)(f)


      }


      implicit class Map11COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11))))))))))) {
        val mapN: Map11C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = new Map11C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ A11))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ A11))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ A11))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ A11))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ A11))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ B))))))))))) =
    mapN.map11(c)(f)



      }

implicit class Map12POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12)))))))))))) {
        val mapN: Map12P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = new Map12P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12)))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12)))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12)))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, A12)))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, A12)))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, A12)))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, A12)))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, A12)))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, A12)))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, A12)))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, A12)))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, B)))))))))))) =
    mapN.map12(p)(f)


      }


      implicit class Map12COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))))) {
        val mapN: Map12C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = new Map12C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ A12)))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ A12)))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ A12)))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ A12)))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ B)))))))))))) =
    mapN.map12(c)(f)



      }

implicit class Map13POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13))))))))))))) {
        val mapN: Map13P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = new Map13P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, A13))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, A13))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, A13))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, A13))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, A13))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, A13))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, A13))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, A13))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, B))))))))))))) =
    mapN.map13(p)(f)


      }


      implicit class Map13COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))))) {
        val mapN: Map13C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = new Map13C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ A13))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ A13))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ A13))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ A13))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ B))))))))))))) =
    mapN.map13(c)(f)



      }

implicit class Map14POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14)))))))))))))) {
        val mapN: Map14P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = new Map14P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14)))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14)))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14)))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14)))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14)))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, A14)))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, A14)))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, A14)))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, A14)))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, A14)))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, A14)))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, A14)))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, A14)))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, B)))))))))))))) =
    mapN.map14(p)(f)


      }


      implicit class Map14COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))))) {
        val mapN: Map14C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = new Map14C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ A14)))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ A14)))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ A14)))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ A14)))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ B)))))))))))))) =
    mapN.map14(c)(f)



      }

implicit class Map15POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15))))))))))))))) {
        val mapN: Map15P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = new Map15P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, A15))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, A15))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, A15))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, A15))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, A15))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, A15))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, A15))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, A15))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, B))))))))))))))) =
    mapN.map15(p)(f)


      }


      implicit class Map15COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15))))))))))))))) {
        val mapN: Map15C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = new Map15C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ A15))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ A15))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ A15))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ A15))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ A15))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ B))))))))))))))) =
    mapN.map15(c)(f)



      }

implicit class Map16POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16)))))))))))))))) {
        val mapN: Map16P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = new Map16P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16)))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16)))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16)))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16)))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16)))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16)))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16)))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, A16)))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, A16)))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, A16)))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, A16)))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, A16)))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, A16)))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, A16)))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, A16)))))))))))))))) =
    mapN.map15(p)(f)



  def map16[B](f: A16 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, B)))))))))))))))) =
    mapN.map16(p)(f)


      }


      implicit class Map16COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16)))))))))))))))) {
        val mapN: Map16C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = new Map16C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16)))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16)))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16)))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16)))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16)))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16)))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16)))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16)))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16)))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16)))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ A16)))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ A16)))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ A16)))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ A16)))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ A16)))))))))))))))) =
    mapN.map15(c)(f)




  def map16[B](f: A16 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ B)))))))))))))))) =
    mapN.map16(c)(f)



      }

implicit class Map17POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17))))))))))))))))) {
        val mapN: Map17P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = new Map17P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17))))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17))))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17))))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17))))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17))))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17))))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17))))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17))))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, A17))))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, A17))))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, A17))))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, A17))))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, A17))))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, A17))))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, A17))))))))))))))))) =
    mapN.map15(p)(f)



  def map16[B](f: A16 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, A17))))))))))))))))) =
    mapN.map16(p)(f)



  def map17[B](f: A17 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, B))))))))))))))))) =
    mapN.map17(p)(f)


      }


      implicit class Map17COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17))))))))))))))))) {
        val mapN: Map17C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = new Map17C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17))))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17))))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17))))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17))))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17))))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17))))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17))))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17))))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17))))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17))))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17))))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ A17))))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ A17))))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ A17))))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ A17))))))))))))))))) =
    mapN.map15(c)(f)




  def map16[B](f: A16 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ A17))))))))))))))))) =
    mapN.map16(c)(f)




  def map17[B](f: A17 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ B))))))))))))))))) =
    mapN.map17(c)(f)



      }

implicit class Map18POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18)))))))))))))))))) {
        val mapN: Map18P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = new Map18P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18)))))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18)))))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18)))))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18)))))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18)))))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18)))))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18)))))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18)))))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18)))))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, A18)))))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, A18)))))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, A18)))))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, A18)))))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, A18)))))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, A18)))))))))))))))))) =
    mapN.map15(p)(f)



  def map16[B](f: A16 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, A18)))))))))))))))))) =
    mapN.map16(p)(f)



  def map17[B](f: A17 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, A18)))))))))))))))))) =
    mapN.map17(p)(f)



  def map18[B](f: A18 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, B)))))))))))))))))) =
    mapN.map18(p)(f)


      }


      implicit class Map18COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))) {
        val mapN: Map18C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = new Map18C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map15(c)(f)




  def map16[B](f: A16 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ A18)))))))))))))))))) =
    mapN.map16(c)(f)




  def map17[B](f: A17 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ A18)))))))))))))))))) =
    mapN.map17(c)(f)




  def map18[B](f: A18 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ B)))))))))))))))))) =
    mapN.map18(c)(f)



      }

implicit class Map19POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19))))))))))))))))))) {
        val mapN: Map19P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = new Map19P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19))))))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19))))))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19))))))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19))))))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19))))))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19))))))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19))))))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19))))))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19))))))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19))))))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, A19))))))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, A19))))))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, A19))))))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, A19))))))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, A19))))))))))))))))))) =
    mapN.map15(p)(f)



  def map16[B](f: A16 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, A19))))))))))))))))))) =
    mapN.map16(p)(f)



  def map17[B](f: A17 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, A19))))))))))))))))))) =
    mapN.map17(p)(f)



  def map18[B](f: A18 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, A19))))))))))))))))))) =
    mapN.map18(p)(f)



  def map19[B](f: A19 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, B))))))))))))))))))) =
    mapN.map19(p)(f)


      }


      implicit class Map19COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) {
        val mapN: Map19C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = new Map19C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map15(c)(f)




  def map16[B](f: A16 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map16(c)(f)




  def map17[B](f: A17 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ A19))))))))))))))))))) =
    mapN.map17(c)(f)




  def map18[B](f: A18 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ A19))))))))))))))))))) =
    mapN.map18(c)(f)




  def map19[B](f: A19 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ B))))))))))))))))))) =
    mapN.map19(c)(f)



      }

implicit class Map20POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) {
        val mapN: Map20P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = new Map20P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map15(p)(f)



  def map16[B](f: A16 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map16(p)(f)



  def map17[B](f: A17 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, A20)))))))))))))))))))) =
    mapN.map17(p)(f)



  def map18[B](f: A18 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, A20)))))))))))))))))))) =
    mapN.map18(p)(f)



  def map19[B](f: A19 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, A20)))))))))))))))))))) =
    mapN.map19(p)(f)



  def map20[B](f: A20 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, B)))))))))))))))))))) =
    mapN.map20(p)(f)


      }


      implicit class Map20COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) {
        val mapN: Map20C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = new Map20C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map15(c)(f)




  def map16[B](f: A16 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map16(c)(f)




  def map17[B](f: A17 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map17(c)(f)




  def map18[B](f: A18 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ A20)))))))))))))))))))) =
    mapN.map18(c)(f)




  def map19[B](f: A19 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ A20)))))))))))))))))))) =
    mapN.map19(c)(f)




  def map20[B](f: A20 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ B)))))))))))))))))))) =
    mapN.map20(c)(f)



      }

implicit class Map21POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) {
        val mapN: Map21P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = new Map21P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map15(p)(f)



  def map16[B](f: A16 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map16(p)(f)



  def map17[B](f: A17 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map17(p)(f)



  def map18[B](f: A18 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, A21))))))))))))))))))))) =
    mapN.map18(p)(f)



  def map19[B](f: A19 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, A21))))))))))))))))))))) =
    mapN.map19(p)(f)



  def map20[B](f: A20 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, A21))))))))))))))))))))) =
    mapN.map20(p)(f)



  def map21[B](f: A21 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, B))))))))))))))))))))) =
    mapN.map21(p)(f)


      }


      implicit class Map21COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) {
        val mapN: Map21C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = new Map21C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map15(c)(f)




  def map16[B](f: A16 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map16(c)(f)




  def map17[B](f: A17 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map17(c)(f)




  def map18[B](f: A18 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map18(c)(f)




  def map19[B](f: A19 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ A21))))))))))))))))))))) =
    mapN.map19(c)(f)




  def map20[B](f: A20 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ A21))))))))))))))))))))) =
    mapN.map20(c)(f)




  def map21[B](f: A21 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ B))))))))))))))))))))) =
    mapN.map21(c)(f)



      }

implicit class Map22POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) {
        val mapN: Map22P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = new Map22P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map15(p)(f)



  def map16[B](f: A16 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map16(p)(f)



  def map17[B](f: A17 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map17(p)(f)



  def map18[B](f: A18 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map18(p)(f)



  def map19[B](f: A19 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, A22)))))))))))))))))))))) =
    mapN.map19(p)(f)



  def map20[B](f: A20 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, A22)))))))))))))))))))))) =
    mapN.map20(p)(f)



  def map21[B](f: A21 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, A22)))))))))))))))))))))) =
    mapN.map21(p)(f)



  def map22[B](f: A22 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, B)))))))))))))))))))))) =
    mapN.map22(p)(f)


      }


      implicit class Map22COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) {
        val mapN: Map22C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] = new Map22C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map15(c)(f)




  def map16[B](f: A16 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map16(c)(f)




  def map17[B](f: A17 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map17(c)(f)




  def map18[B](f: A18 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map18(c)(f)




  def map19[B](f: A19 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map19(c)(f)




  def map20[B](f: A20 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ A22)))))))))))))))))))))) =
    mapN.map20(c)(f)




  def map21[B](f: A21 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ A22)))))))))))))))))))))) =
    mapN.map21(c)(f)




  def map22[B](f: A22 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ B)))))))))))))))))))))) =
    mapN.map22(c)(f)



      }

implicit class Map23POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) {
        val mapN: Map23P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23] = new Map23P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map15(p)(f)



  def map16[B](f: A16 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map16(p)(f)



  def map17[B](f: A17 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map17(p)(f)



  def map18[B](f: A18 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map18(p)(f)



  def map19[B](f: A19 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map19(p)(f)



  def map20[B](f: A20 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, A23))))))))))))))))))))))) =
    mapN.map20(p)(f)



  def map21[B](f: A21 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, A23))))))))))))))))))))))) =
    mapN.map21(p)(f)



  def map22[B](f: A22 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, A23))))))))))))))))))))))) =
    mapN.map22(p)(f)



  def map23[B](f: A23 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, B))))))))))))))))))))))) =
    mapN.map23(p)(f)


      }


      implicit class Map23COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) {
        val mapN: Map23C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23] = new Map23C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map15(c)(f)




  def map16[B](f: A16 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map16(c)(f)




  def map17[B](f: A17 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map17(c)(f)




  def map18[B](f: A18 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map18(c)(f)




  def map19[B](f: A19 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map19(c)(f)




  def map20[B](f: A20 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map20(c)(f)




  def map21[B](f: A21 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ A23))))))))))))))))))))))) =
    mapN.map21(c)(f)




  def map22[B](f: A22 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ A23))))))))))))))))))))))) =
    mapN.map22(c)(f)




  def map23[B](f: A23 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ B))))))))))))))))))))))) =
    mapN.map23(c)(f)



      }

implicit class Map24POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) {
        val mapN: Map24P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24] = new Map24P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map15(p)(f)



  def map16[B](f: A16 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map16(p)(f)



  def map17[B](f: A17 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map17(p)(f)



  def map18[B](f: A18 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map18(p)(f)



  def map19[B](f: A19 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map19(p)(f)



  def map20[B](f: A20 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map20(p)(f)



  def map21[B](f: A21 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, (A23, A24)))))))))))))))))))))))) =
    mapN.map21(p)(f)



  def map22[B](f: A22 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, (A23, A24)))))))))))))))))))))))) =
    mapN.map22(p)(f)



  def map23[B](f: A23 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (B, A24)))))))))))))))))))))))) =
    mapN.map23(p)(f)



  def map24[B](f: A24 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, B)))))))))))))))))))))))) =
    mapN.map24(p)(f)


      }


      implicit class Map24COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) {
        val mapN: Map24C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24] = new Map24C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map15(c)(f)




  def map16[B](f: A16 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map16(c)(f)




  def map17[B](f: A17 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map17(c)(f)




  def map18[B](f: A18 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map18(c)(f)




  def map19[B](f: A19 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map19(c)(f)




  def map20[B](f: A20 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map20(c)(f)




  def map21[B](f: A21 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map21(c)(f)




  def map22[B](f: A22 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ (A23 \/ A24)))))))))))))))))))))))) =
    mapN.map22(c)(f)




  def map23[B](f: A23 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (B \/ A24)))))))))))))))))))))))) =
    mapN.map23(c)(f)




  def map24[B](f: A24 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ B)))))))))))))))))))))))) =
    mapN.map24(c)(f)



      }

implicit class Map25POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) {
        val mapN: Map25P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25] = new Map25P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map15(p)(f)



  def map16[B](f: A16 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map16(p)(f)



  def map17[B](f: A17 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map17(p)(f)



  def map18[B](f: A18 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map18(p)(f)



  def map19[B](f: A19 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map19(p)(f)



  def map20[B](f: A20 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map20(p)(f)



  def map21[B](f: A21 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map21(p)(f)



  def map22[B](f: A22 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, (A23, (A24, A25))))))))))))))))))))))))) =
    mapN.map22(p)(f)



  def map23[B](f: A23 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (B, (A24, A25))))))))))))))))))))))))) =
    mapN.map23(p)(f)



  def map24[B](f: A24 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (B, A25))))))))))))))))))))))))) =
    mapN.map24(p)(f)



  def map25[B](f: A25 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, B))))))))))))))))))))))))) =
    mapN.map25(p)(f)


      }


      implicit class Map25COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) {
        val mapN: Map25C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25] = new Map25C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map15(c)(f)




  def map16[B](f: A16 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map16(c)(f)




  def map17[B](f: A17 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map17(c)(f)




  def map18[B](f: A18 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map18(c)(f)




  def map19[B](f: A19 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map19(c)(f)




  def map20[B](f: A20 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map20(c)(f)




  def map21[B](f: A21 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map21(c)(f)




  def map22[B](f: A22 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ (A23 \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map22(c)(f)




  def map23[B](f: A23 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (B \/ (A24 \/ A25))))))))))))))))))))))))) =
    mapN.map23(c)(f)




  def map24[B](f: A24 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (B \/ A25))))))))))))))))))))))))) =
    mapN.map24(c)(f)




  def map25[B](f: A25 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ B))))))))))))))))))))))))) =
    mapN.map25(c)(f)



      }

implicit class Map26POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) {
        val mapN: Map26P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26] = new Map26P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map15(p)(f)



  def map16[B](f: A16 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map16(p)(f)



  def map17[B](f: A17 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map17(p)(f)



  def map18[B](f: A18 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map18(p)(f)



  def map19[B](f: A19 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map19(p)(f)



  def map20[B](f: A20 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map20(p)(f)



  def map21[B](f: A21 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map21(p)(f)



  def map22[B](f: A22 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, (A23, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map22(p)(f)



  def map23[B](f: A23 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (B, (A24, (A25, A26)))))))))))))))))))))))))) =
    mapN.map23(p)(f)



  def map24[B](f: A24 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (B, (A25, A26)))))))))))))))))))))))))) =
    mapN.map24(p)(f)



  def map25[B](f: A25 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (B, A26)))))))))))))))))))))))))) =
    mapN.map25(p)(f)



  def map26[B](f: A26 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, B)))))))))))))))))))))))))) =
    mapN.map26(p)(f)


      }


      implicit class Map26COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) {
        val mapN: Map26C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26] = new Map26C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map15(c)(f)




  def map16[B](f: A16 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map16(c)(f)




  def map17[B](f: A17 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map17(c)(f)




  def map18[B](f: A18 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map18(c)(f)




  def map19[B](f: A19 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map19(c)(f)




  def map20[B](f: A20 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map20(c)(f)




  def map21[B](f: A21 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map21(c)(f)




  def map22[B](f: A22 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ (A23 \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map22(c)(f)




  def map23[B](f: A23 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (B \/ (A24 \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map23(c)(f)




  def map24[B](f: A24 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (B \/ (A25 \/ A26)))))))))))))))))))))))))) =
    mapN.map24(c)(f)




  def map25[B](f: A25 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (B \/ A26)))))))))))))))))))))))))) =
    mapN.map25(c)(f)




  def map26[B](f: A26 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ B)))))))))))))))))))))))))) =
    mapN.map26(c)(f)



      }

implicit class Map27POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) {
        val mapN: Map27P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27] = new Map27P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map15(p)(f)



  def map16[B](f: A16 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map16(p)(f)



  def map17[B](f: A17 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map17(p)(f)



  def map18[B](f: A18 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map18(p)(f)



  def map19[B](f: A19 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map19(p)(f)



  def map20[B](f: A20 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map20(p)(f)



  def map21[B](f: A21 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map21(p)(f)



  def map22[B](f: A22 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, (A23, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map22(p)(f)



  def map23[B](f: A23 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (B, (A24, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map23(p)(f)



  def map24[B](f: A24 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (B, (A25, (A26, A27))))))))))))))))))))))))))) =
    mapN.map24(p)(f)



  def map25[B](f: A25 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (B, (A26, A27))))))))))))))))))))))))))) =
    mapN.map25(p)(f)



  def map26[B](f: A26 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (B, A27))))))))))))))))))))))))))) =
    mapN.map26(p)(f)



  def map27[B](f: A27 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, B))))))))))))))))))))))))))) =
    mapN.map27(p)(f)


      }


      implicit class Map27COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) {
        val mapN: Map27C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27] = new Map27C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map15(c)(f)




  def map16[B](f: A16 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map16(c)(f)




  def map17[B](f: A17 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map17(c)(f)




  def map18[B](f: A18 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map18(c)(f)




  def map19[B](f: A19 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map19(c)(f)




  def map20[B](f: A20 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map20(c)(f)




  def map21[B](f: A21 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map21(c)(f)




  def map22[B](f: A22 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map22(c)(f)




  def map23[B](f: A23 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (B \/ (A24 \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map23(c)(f)




  def map24[B](f: A24 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (B \/ (A25 \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map24(c)(f)




  def map25[B](f: A25 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (B \/ (A26 \/ A27))))))))))))))))))))))))))) =
    mapN.map25(c)(f)




  def map26[B](f: A26 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (B \/ A27))))))))))))))))))))))))))) =
    mapN.map26(c)(f)




  def map27[B](f: A27 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ B))))))))))))))))))))))))))) =
    mapN.map27(c)(f)



      }

implicit class Map28POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) {
        val mapN: Map28P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28] = new Map28P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map15(p)(f)



  def map16[B](f: A16 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map16(p)(f)



  def map17[B](f: A17 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map17(p)(f)



  def map18[B](f: A18 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map18(p)(f)



  def map19[B](f: A19 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map19(p)(f)



  def map20[B](f: A20 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map20(p)(f)



  def map21[B](f: A21 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map21(p)(f)



  def map22[B](f: A22 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, (A23, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map22(p)(f)



  def map23[B](f: A23 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (B, (A24, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map23(p)(f)



  def map24[B](f: A24 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (B, (A25, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map24(p)(f)



  def map25[B](f: A25 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (B, (A26, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map25(p)(f)



  def map26[B](f: A26 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (B, (A27, A28)))))))))))))))))))))))))))) =
    mapN.map26(p)(f)



  def map27[B](f: A27 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (B, A28)))))))))))))))))))))))))))) =
    mapN.map27(p)(f)



  def map28[B](f: A28 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, B)))))))))))))))))))))))))))) =
    mapN.map28(p)(f)


      }


      implicit class Map28COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) {
        val mapN: Map28C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28] = new Map28C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map15(c)(f)




  def map16[B](f: A16 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map16(c)(f)




  def map17[B](f: A17 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map17(c)(f)




  def map18[B](f: A18 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map18(c)(f)




  def map19[B](f: A19 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map19(c)(f)




  def map20[B](f: A20 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map20(c)(f)




  def map21[B](f: A21 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map21(c)(f)




  def map22[B](f: A22 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map22(c)(f)




  def map23[B](f: A23 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (B \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map23(c)(f)




  def map24[B](f: A24 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (B \/ (A25 \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map24(c)(f)




  def map25[B](f: A25 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (B \/ (A26 \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map25(c)(f)




  def map26[B](f: A26 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (B \/ (A27 \/ A28)))))))))))))))))))))))))))) =
    mapN.map26(c)(f)




  def map27[B](f: A27 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (B \/ A28)))))))))))))))))))))))))))) =
    mapN.map27(c)(f)




  def map28[B](f: A28 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ B)))))))))))))))))))))))))))) =
    mapN.map28(c)(f)



      }

implicit class Map29POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) {
        val mapN: Map29P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29] = new Map29P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map15(p)(f)



  def map16[B](f: A16 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map16(p)(f)



  def map17[B](f: A17 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map17(p)(f)



  def map18[B](f: A18 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map18(p)(f)



  def map19[B](f: A19 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map19(p)(f)



  def map20[B](f: A20 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map20(p)(f)



  def map21[B](f: A21 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map21(p)(f)



  def map22[B](f: A22 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, (A23, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map22(p)(f)



  def map23[B](f: A23 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (B, (A24, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map23(p)(f)



  def map24[B](f: A24 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (B, (A25, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map24(p)(f)



  def map25[B](f: A25 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (B, (A26, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map25(p)(f)



  def map26[B](f: A26 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (B, (A27, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map26(p)(f)



  def map27[B](f: A27 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (B, (A28, A29))))))))))))))))))))))))))))) =
    mapN.map27(p)(f)



  def map28[B](f: A28 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (B, A29))))))))))))))))))))))))))))) =
    mapN.map28(p)(f)



  def map29[B](f: A29 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, B))))))))))))))))))))))))))))) =
    mapN.map29(p)(f)


      }


      implicit class Map29COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) {
        val mapN: Map29C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29] = new Map29C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map15(c)(f)




  def map16[B](f: A16 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map16(c)(f)




  def map17[B](f: A17 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map17(c)(f)




  def map18[B](f: A18 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map18(c)(f)




  def map19[B](f: A19 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map19(c)(f)




  def map20[B](f: A20 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map20(c)(f)




  def map21[B](f: A21 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map21(c)(f)




  def map22[B](f: A22 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map22(c)(f)




  def map23[B](f: A23 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (B \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map23(c)(f)




  def map24[B](f: A24 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (B \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map24(c)(f)




  def map25[B](f: A25 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (B \/ (A26 \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map25(c)(f)




  def map26[B](f: A26 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (B \/ (A27 \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map26(c)(f)




  def map27[B](f: A27 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (B \/ (A28 \/ A29))))))))))))))))))))))))))))) =
    mapN.map27(c)(f)




  def map28[B](f: A28 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (B \/ A29))))))))))))))))))))))))))))) =
    mapN.map28(c)(f)




  def map29[B](f: A29 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ B))))))))))))))))))))))))))))) =
    mapN.map29(c)(f)



      }

implicit class Map30POps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30](p: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) {
        val mapN: Map30P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30] = new Map30P[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30] {}
        
  def map1[B](f: A1 => B): ((B, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map1(p)(f)



  def map2[B](f: A2 => B): ((A1, (B, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map2(p)(f)



  def map3[B](f: A3 => B): ((A1, (A2, (B, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map3(p)(f)



  def map4[B](f: A4 => B): ((A1, (A2, (A3, (B, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map4(p)(f)



  def map5[B](f: A5 => B): ((A1, (A2, (A3, (A4, (B, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map5(p)(f)



  def map6[B](f: A6 => B): ((A1, (A2, (A3, (A4, (A5, (B, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map6(p)(f)



  def map7[B](f: A7 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (B, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map7(p)(f)



  def map8[B](f: A8 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (B, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map8(p)(f)



  def map9[B](f: A9 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (B, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map9(p)(f)



  def map10[B](f: A10 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (B, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map10(p)(f)



  def map11[B](f: A11 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (B, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map11(p)(f)



  def map12[B](f: A12 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (B, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map12(p)(f)



  def map13[B](f: A13 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (B, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map13(p)(f)



  def map14[B](f: A14 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (B, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map14(p)(f)



  def map15[B](f: A15 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (B, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map15(p)(f)



  def map16[B](f: A16 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (B, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map16(p)(f)



  def map17[B](f: A17 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (B, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map17(p)(f)



  def map18[B](f: A18 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (B, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map18(p)(f)



  def map19[B](f: A19 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (B, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map19(p)(f)



  def map20[B](f: A20 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (B, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map20(p)(f)



  def map21[B](f: A21 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (B, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map21(p)(f)



  def map22[B](f: A22 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (B, (A23, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map22(p)(f)



  def map23[B](f: A23 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (B, (A24, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map23(p)(f)



  def map24[B](f: A24 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (B, (A25, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map24(p)(f)



  def map25[B](f: A25 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (B, (A26, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map25(p)(f)



  def map26[B](f: A26 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (B, (A27, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map26(p)(f)



  def map27[B](f: A27 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (B, (A28, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map27(p)(f)



  def map28[B](f: A28 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (B, (A29, A30)))))))))))))))))))))))))))))) =
    mapN.map28(p)(f)



  def map29[B](f: A29 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (B, A30)))))))))))))))))))))))))))))) =
    mapN.map29(p)(f)



  def map30[B](f: A30 => B): ((A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, (A23, (A24, (A25, (A26, (A27, (A28, (A29, B)))))))))))))))))))))))))))))) =
    mapN.map30(p)(f)


      }


      implicit class Map30COps[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30](c: (A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) {
        val mapN: Map30C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30] = new Map30C[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30] {}
        
  def map1[B](f: A1 => B): ((B \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map1(c)(f)




  def map2[B](f: A2 => B): ((A1 \/ (B \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map2(c)(f)




  def map3[B](f: A3 => B): ((A1 \/ (A2 \/ (B \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map3(c)(f)




  def map4[B](f: A4 => B): ((A1 \/ (A2 \/ (A3 \/ (B \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map4(c)(f)




  def map5[B](f: A5 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (B \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map5(c)(f)




  def map6[B](f: A6 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (B \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map6(c)(f)




  def map7[B](f: A7 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (B \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map7(c)(f)




  def map8[B](f: A8 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (B \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map8(c)(f)




  def map9[B](f: A9 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (B \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map9(c)(f)




  def map10[B](f: A10 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (B \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map10(c)(f)




  def map11[B](f: A11 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (B \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map11(c)(f)




  def map12[B](f: A12 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (B \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map12(c)(f)




  def map13[B](f: A13 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (B \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map13(c)(f)




  def map14[B](f: A14 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (B \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map14(c)(f)




  def map15[B](f: A15 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (B \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map15(c)(f)




  def map16[B](f: A16 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (B \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map16(c)(f)




  def map17[B](f: A17 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (B \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map17(c)(f)




  def map18[B](f: A18 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (B \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map18(c)(f)




  def map19[B](f: A19 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (B \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map19(c)(f)




  def map20[B](f: A20 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (B \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map20(c)(f)




  def map21[B](f: A21 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (B \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map21(c)(f)




  def map22[B](f: A22 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (B \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map22(c)(f)




  def map23[B](f: A23 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (B \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map23(c)(f)




  def map24[B](f: A24 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (B \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map24(c)(f)




  def map25[B](f: A25 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (B \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map25(c)(f)




  def map26[B](f: A26 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (B \/ (A27 \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map26(c)(f)




  def map27[B](f: A27 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (B \/ (A28 \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map27(c)(f)




  def map28[B](f: A28 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (B \/ (A29 \/ A30)))))))))))))))))))))))))))))) =
    mapN.map28(c)(f)




  def map29[B](f: A29 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (B \/ A30)))))))))))))))))))))))))))))) =
    mapN.map29(c)(f)




  def map30[B](f: A30 => B): ((A1 \/ (A2 \/ (A3 \/ (A4 \/ (A5 \/ (A6 \/ (A7 \/ (A8 \/ (A9 \/ (A10 \/ (A11 \/ (A12 \/ (A13 \/ (A14 \/ (A15 \/ (A16 \/ (A17 \/ (A18 \/ (A19 \/ (A20 \/ (A21 \/ (A22 \/ (A23 \/ (A24 \/ (A25 \/ (A26 \/ (A27 \/ (A28 \/ (A29 \/ B)))))))))))))))))))))))))))))) =
    mapN.map30(c)(f)



      }


  }
}
