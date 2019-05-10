package andxor

trait Labelled[A] {
  type L <: Singleton with String
  val label: L
  val value: A
}

object Labelled {
  type Aux[A, L0 <: Singleton with String] = Labelled[A] { type L = L0 }

  def apply[A, L0 <: Singleton with String](value0: A, label0: L0): Labelled.Aux[A, L0] = new Labelled[A] {
    type L = L0
    val label: L = label0
    val value: A = value0
  }
}


import scalaz.Apply
import scalaz.Isomorphism.IsoSet
import scalaz.std.option._
import scalaz.syntax.apply._
import scalaz.syntax.std.string._

case class Foo(x1: String, x2: Int)
object Foo {
  val x1Label = "x1"
  implicit val x1LabelImpl: x1Label.type = x1Label
  val x2Label = "x2"
  implicit val x2LabelImpl: x2Label.type = x2Label

  type AXO = AndXor2[Labelled.Aux[String, x1Label.type], Labelled.Aux[Int, x2Label.type]]
  val AXO: AXO = AndXor2[Labelled.Aux[String, x1Label.type], Labelled.Aux[Int, x2Label.type]]

  val iso: IsoSet[Foo, AXO.Prod] = IsoSet(
    x => AXO.Prod((Labelled[String, x1Label.type](x.x1, x1Label), Labelled[Int, x2Label.type](x.x2, x2Label))),
    x => new Foo(x.run._1.value, x.run._2.value))

  trait Read[A] { def read(s: String): Option[A] }
  object Read {
    implicit def readLabelled[A, L <: Singleton with String](implicit l: L, r: Read[A]): Read[Labelled.Aux[A, L]] =
      _.split("\n").flatMap(_.split(s"$l := ", 2).lift(1).flatMap(r.read(_)).map(Labelled(_, l))).headOption

    implicit val readStr: Read[String] = """^"(.*)"$""".r.findFirstIn(_)
    implicit val readInt: Read[Int] = _.parseInt.toOption
    implicit val readBool: Read[Boolean] = _.parseBoolean.toOption

    implicit val readApply: Apply[Read] = new Apply[Read] {
      def map[A, B](fa: Read[A])(f: A => B): Read[B] = s => fa.read(s).map(f)
      def ap[A, B](fa: => Read[A])(f: => Read[A => B]): Read[B] = s => (f.read(s) |@| fa.read(s))(_(_))
    }
  }

  val derivedShow: Read[AXO.Prod] = AXO.combineId[Read].apply
}

