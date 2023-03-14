package andxor.compat

import scala.tools.nsc.Global

trait ParseNamedArg {
  val global: Global

  import global._

  def parseNamedArg(tree: Tree, name: String): Option[Tree] =
    Some(tree).collect { case NamedArg(Ident(TermName(`name`)), rhs) => rhs }
}
