val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    scalacOptions ++= List(
      "-Wunused:params",
      "-Wunused:privates",
      "-Wunused:locals",
      "-Wvalue-discard",
      "-Wconf:msg=binding&msg=StringContext:s",
      "-explain",
      "-deprecation",
      // "-Wnonunit-statement",
    )
  )
