package andxor
package routes


import scalaz.Id.Id

sealed trait Route[P <: AndXor, QS <: AndXor] {
  def mkPath: P#Cop[Id] => Path
  def mkQueryString: QS#Cop[Id] => QueryString

  def apply(path: P#Prod[Id], queryString: QS#Prod[Id]): String
}



object Route {
  
  sealed trait RouteP1QS0[P1]
  extends Route[AndXor1[P1], AndXor0] {
    
      def /[P <: Path](next: P): RouteP2QS0[P1, P] =
        new RouteP2QS0[P1, P] {}
    

    
      def ?[QS <: QueryString](next: QS): RouteP1QS1[P1, QS] =
        new RouteP1QS1[P1, QS] {}
    
  }

  sealed trait RouteP1QS1[P1, QS1]
  extends Route[AndXor1[P1], AndXor1[QS1]] {
    
      def /[P <: Path](next: P): RouteP2QS1[P1, P, QS1] =
        new RouteP2QS1[P1, P, QS1] {}
    

    
      def ?[QS <: QueryString](next: QS): RouteP1QS2[P1, QS1, QS] =
        new RouteP1QS2[P1, QS1, QS] {}
    
  }

  sealed trait RouteP1QS2[P1, QS1, QS2]
  extends Route[AndXor1[P1], AndXor2[QS1, QS2]] {
    
      def /[P <: Path](next: P): RouteP2QS2[P1, P, QS1, QS2] =
        new RouteP2QS2[P1, P, QS1, QS2] {}
    

    
      def ?[QS <: QueryString](next: QS): RouteP1QS3[P1, QS1, QS2, QS] =
        new RouteP1QS3[P1, QS1, QS2, QS] {}
    
  }

  sealed trait RouteP1QS3[P1, QS1, QS2, QS3]
  extends Route[AndXor1[P1], AndXor3[QS1, QS2, QS3]] {
    
      def /[P <: Path](next: P): RouteP2QS3[P1, P, QS1, QS2, QS3] =
        new RouteP2QS3[P1, P, QS1, QS2, QS3] {}
    

    
      def ?[QS <: QueryString](last: QS): Route[AndXor1[P1], AndXor4[QS1, QS2, QS3, QS]]
    
  }

  sealed trait RouteP2QS0[P1, P2]
  extends Route[AndXor2[P1, P2], AndXor0] {
    
      def /[P <: Path](next: P): RouteP3QS0[P1, P2, P] =
        new RouteP3QS0[P1, P2, P] {}
    

    
      def ?[QS <: QueryString](next: QS): RouteP2QS1[P1, P2, QS] =
        new RouteP2QS1[P1, P2, QS] {}
    
  }

  sealed trait RouteP2QS1[P1, P2, QS1]
  extends Route[AndXor2[P1, P2], AndXor1[QS1]] {
    
      def /[P <: Path](next: P): RouteP3QS1[P1, P2, P, QS1] =
        new RouteP3QS1[P1, P2, P, QS1] {}
    

    
      def ?[QS <: QueryString](next: QS): RouteP2QS2[P1, P2, QS1, QS] =
        new RouteP2QS2[P1, P2, QS1, QS] {}
    
  }

  sealed trait RouteP2QS2[P1, P2, QS1, QS2]
  extends Route[AndXor2[P1, P2], AndXor2[QS1, QS2]] {
    
      def /[P <: Path](next: P): RouteP3QS2[P1, P2, P, QS1, QS2] =
        new RouteP3QS2[P1, P2, P, QS1, QS2] {}
    

    
      def ?[QS <: QueryString](next: QS): RouteP2QS3[P1, P2, QS1, QS2, QS] =
        new RouteP2QS3[P1, P2, QS1, QS2, QS] {}
    
  }

  sealed trait RouteP2QS3[P1, P2, QS1, QS2, QS3]
  extends Route[AndXor2[P1, P2], AndXor3[QS1, QS2, QS3]] {
    
      def /[P <: Path](next: P): RouteP3QS3[P1, P2, P, QS1, QS2, QS3] =
        new RouteP3QS3[P1, P2, P, QS1, QS2, QS3] {}
    

    
      def ?[QS <: QueryString](last: QS): Route[AndXor2[P1, P2], AndXor4[QS1, QS2, QS3, QS]]
    
  }

  sealed trait RouteP3QS0[P1, P2, P3]
  extends Route[AndXor3[P1, P2, P3], AndXor0] {
    
      def /[P <: Path](last: P): Route[AndXor4[P1, P2, P3, P], AndXor0]
    

    
      def ?[QS <: QueryString](next: QS): RouteP3QS1[P1, P2, P3, QS] =
        new RouteP3QS1[P1, P2, P3, QS] {}
    
  }

  sealed trait RouteP3QS1[P1, P2, P3, QS1]
  extends Route[AndXor3[P1, P2, P3], AndXor1[QS1]] {
    
      def /[P <: Path](last: P): Route[AndXor4[P1, P2, P3, P], AndXor1[QS1]]
    

    
      def ?[QS <: QueryString](next: QS): RouteP3QS2[P1, P2, P3, QS1, QS] =
        new RouteP3QS2[P1, P2, P3, QS1, QS] {}
    
  }

  sealed trait RouteP3QS2[P1, P2, P3, QS1, QS2]
  extends Route[AndXor3[P1, P2, P3], AndXor2[QS1, QS2]] {
    
      def /[P <: Path](last: P): Route[AndXor4[P1, P2, P3, P], AndXor2[QS1, QS2]]
    

    
      def ?[QS <: QueryString](next: QS): RouteP3QS3[P1, P2, P3, QS1, QS2, QS] =
        new RouteP3QS3[P1, P2, P3, QS1, QS2, QS] {}
    
  }

  sealed trait RouteP3QS3[P1, P2, P3, QS1, QS2, QS3]
  extends Route[AndXor3[P1, P2, P3], AndXor3[QS1, QS2, QS3]] {
    
      def /[P <: Path](last: P): Route[AndXor4[P1, P2, P3, P], AndXor3[QS1, QS2, QS3]]
    

    
      def ?[QS <: QueryString](last: QS): Route[AndXor3[P1, P2, P3], AndXor4[QS1, QS2, QS3, QS]]
    
  }

}
