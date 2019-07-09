package andxor.compat

import scala.tools.nsc.Global

trait ParseTypeclasses {
  val global: Global

  import global._

  def parseTypeclasses(term: String, args: List[Tree]): List[Tree] =
    args.flatMap(_ match {
      case NamedArg(Ident(TermName(`term`)), q"List(..$tcs)") => tcs
      case NamedArg(Ident(TermName(`term`)), q"Set(..$tcs)") => tcs
      case NamedArg(Ident(TermName(`term`)), q"Set(..$tcs)") => tcs
      case NamedArg(Ident(TermName(`term`)), q"Vector(..$tcs)") => tcs
      case t => Nil
    })
}
