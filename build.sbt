lazy val apollo =
  project
    .in(
      file("apollo")
    )
    .settings(
      name := "apollo"
    )
    .settings(
      libraryDependencies += Dependencies.zioCore
    )
    .settings(
      run / fork := true
    )
