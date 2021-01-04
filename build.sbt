Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "3.0.0-M3"

lazy val root = project
  .in(file("."))
  .settings(
    organization := "codes.quine.labo",
    name := "minicheck",
    version := "0.1.0-SNAPSHOT",
    console / initialCommands := """
      |import minicheck._
      """.stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.20" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
