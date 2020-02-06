package andxor.types

trait Dummy {

  sealed trait Dummy2; object Dummy2 { implicit val inst: Dummy2 = new Dummy2 {} }
  sealed trait Dummy3; object Dummy3 { implicit val inst: Dummy3 = new Dummy3 {} }
  sealed trait Dummy4; object Dummy4 { implicit val inst: Dummy4 = new Dummy4 {} }
  sealed trait Dummy5; object Dummy5 { implicit val inst: Dummy5 = new Dummy5 {} }
  sealed trait Dummy6; object Dummy6 { implicit val inst: Dummy6 = new Dummy6 {} }
}
