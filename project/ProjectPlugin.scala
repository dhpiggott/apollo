import sbt.Keys._
import sbt._
import scalafix.sbt.ScalafixPlugin.autoImport._

object ProjectPlugin extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements

  override def globalSettings: Seq[Setting[_]] =
    addCommandAlias(
      "fix",
      "all compile:scalafix test:scalafix"
    ) ++ addCommandAlias(
      "fixCheck",
      "all compile:scalafix --check test:scalafix --check"
    ) ++ addCommandAlias(
      "fmt",
      "all scalafmtSbt compile:scalafmt test:scalafmt"
    ) ++ addCommandAlias(
      "fmtCheck",
      "all scalafmtSbtCheck compile:scalafmtCheck test:scalafmtCheck"
    )

  override def projectSettings: Seq[Setting[_]] =
    scalaSettings ++
      scalafixSettings

  private lazy val scalaSettings = Seq(
    scalaVersion := "2.13.3"
  )

  private lazy val scalafixSettings =
    Seq(
      addCompilerPlugin(scalafixSemanticdb),
      ThisBuild / scalafixDependencies += Dependencies.organizeImports
    )
}
