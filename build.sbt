lazy val apollo =
  project
    .in(
      file("apollo")
    )
    .settings(
      name := "apollo"
    )
    .settings(
      libraryDependencies ++= Seq(
        Dependencies.fastParse,
        Dependencies.zioCore
      )
    )
    .settings(
      run / fork := true
    )
