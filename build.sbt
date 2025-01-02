ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "scala3-new-features",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0"
  )
