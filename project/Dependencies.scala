import sbt._

object Dependencies {

  val fastParse: ModuleID = "com.lihaoyi" %% "fastparse" % "2.3.0"

  val organizeImports: ModuleID =
    "com.github.liancheng" %% "organize-imports" % "0.4.0"

  object Zio {
    val core: ModuleID = "dev.zio" %% "zio" % "1.0.3"

    val test: ModuleID = "dev.zio" %% "zio-test" % "1.0.3"

    val testSbt: ModuleID = "dev.zio" %% "zio-test-sbt" % "1.0.3"
  }
}
