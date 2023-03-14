package andxor

import andxor.template.*
import java.nio.file.{Files, Path, Paths}
import scalariform.formatter.ScalaFormatter
import scalariform.formatter.preferences.*

object Generate {
  val conf = FormattingPreferences()
    .setPreference(NewlineAtEndOfFile, true)
    .setPreference(SpacesAroundMultiImports, false)

  val tpeLists1To22 = mkTpeList(1, 22)
  val tpeLists1To100 = mkTpeList(1, 100)
  val tpeLists2To22 = mkTpeList(2, 22)
  val tpeLists2To100 = mkTpeList(2, 100)

  private final implicit class PathOps(private val p: Path) extends AnyVal {
    def /(s: String): Path = p.resolve(s)
  }

  def fileContents(p: Path): String = new String(Files.readAllBytes(p), "UTF-8")
  def noWs(s: String): String = s.filterNot(_.isWhitespace)

  def main(args: Array[String]): Unit = {
    val rootDir = Paths.get(BuildInfo.rootDir)
    def maybeWrite(
      name: String,
      contents: String,
      pkgs: List[String] = List("andxor"),
      getProj: Path => Path = _ / "core",
      mainOrTest: Path => Path = _ / "main",
      fileDir: Path => Path = identity _,
      format: Boolean = false
    ): Unit = {
      val f = fileDir(mainOrTest(getProj(rootDir) / "src") / "scala" / "andxor") / name
      println(s"Generating $f...")
      val code = s"${pkgs.map(p => s"package $p").mkString("\n")}\n\n$contents"
      if (Files.notExists(f) || noWs(fileContents(f)) != noWs(code)) {
        val toWrite = if (format) {
          println("    formatting...")
          ScalaFormatter.format(code, conf)
        } else code
        println("    writing...")
        Option(f.getParent).foreach(Files.createDirectories(_))
        Files.write(f, toWrite.getBytes("UTF-8"))
        ()
      } else {
        println(s"Skipping $f -- content is unchanged")
      }
    }

    maybeWrite("Dummy.scala", Dummy(tpeLists1To100), pkgs = List("andxor.types"), fileDir = _ / "types")
    maybeWrite("AndXorNConstructors.scala", AndXorNConstructors(tpeLists1To100))
    maybeWrite("AndXorNTypes.scala", AndXorNTypes(tpeLists1To100.tail))
    maybeWrite("Either.scala", Either(tpeLists1To100))
    maybeWrite("FTraverseEitherNInstances.scala", FTraverseEitherNInstances(tpeLists1To100.tail))
    maybeWrite("FTraverseTupleNInstances.scala", FTraverseTupleNInstances(tpeLists1To100))
    maybeWrite("Tuple.scala", Tuple(tpeLists1To100))

    maybeWrite("DerivationTest.scala", DerivationTest(mkTpeList(1, 3)),
      getProj = _ / "tests", mainOrTest = _ / "test")
    maybeWrite("TypesTest.scala", TypesTest(tpeLists1To22),
      getProj = _ / "tests", mainOrTest = _ / "test")
  }
}
