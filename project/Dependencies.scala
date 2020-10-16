import sbt._

object Dependencies {

  val fastParse: ModuleID = "com.lihaoyi" %% "fastparse" % "2.3.0"

  val organizeImports: ModuleID =
    "com.github.liancheng" %% "organize-imports" % "0.4.0"

  val zioCore: ModuleID = "dev.zio" %% "zio" % "1.0.3"

}
