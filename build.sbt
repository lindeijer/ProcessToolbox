ThisBuild / scalaVersion := "2.12.3"
ThisBuild / organization := "nl.dgl"

lazy val bsv = (project in file("."))
  .settings(
    name := "BijStortVoorbereiding",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
  )