Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "2.13.3"
ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-feature",
  "-deprecation",
  "-Wunused"
)

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
    libraryDependencies += "io.monix" %% "minitest" % "2.9.0" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework")
  )
