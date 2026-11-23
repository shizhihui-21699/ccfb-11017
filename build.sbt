ThisBuild / scalaVersion := "3.3.5"
ThisBuild / organization := "fiberguard"

lazy val root = (project in file("."))
  .settings(
    name := "fiberguard",
    version := "0.1.0",
    Compile / scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    Test / parallelExecution := false,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    Test / fork := true,
    run / fork := true,
    run / javaOptions ++= Seq("-Xmx8g", "-XX:+UseG1GC"),
    Compile / run / mainClass := Some("tapdetect.Main")
  )
