val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "com.github.vagmcs" %% "optimus" % "3.4.5",
      "com.github.vagmcs" %% "optimus-solver-oj" % "3.4.5",
      "com.github.vagmcs" %% "optimus-solver-lp" % "3.4.5"
    ),

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
