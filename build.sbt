import sbt.internal.IvyConsole.Dependencies
lazy val apollo = project
  .in(file("apollo"))
  .settings(name := "apollo")
  .settings(
    libraryDependencies ++= Seq(
      Dependencies.fastParse,
      Dependencies.Zio.core
    )
  )
  .settings(
    libraryDependencies ++= Seq(
      Dependencies.Zio.test % Test,
      Dependencies.Zio.testSbt % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
