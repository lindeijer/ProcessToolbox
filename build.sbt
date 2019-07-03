ThisBuild / scalaVersion := "2.12.6"
ThisBuild / organization := "nl.dgl"

lazy val bsv = (project in file("."))
  .settings(
    name := "BijStortVoorbereiding",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.25" % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
    libraryDependencies += "com.michaelpollmeier" %% "gremlin-scala" % "3.3.3.4",
    libraryDependencies += "org.apache.tinkerpop" % "tinkergraph-gremlin" % "3.3.1",
  )
  
