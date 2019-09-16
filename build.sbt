ThisBuild / scalaVersion := "2.12.6"
ThisBuild / organization := "nl.dgl"

lazy val bsv = (project in file("."))
  .settings(
    resolvers += Resolver.mavenLocal,
    resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases" //  will flag errors in your ScalaTest (and Scalactic) code at compile time, by first adding this line to ~/.sbt/0.13/global.sbt:
    updateOptions := updateOptions.value.withLatestSnapshots(false),
    name := "BijStortVoorbereiding",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.25" % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
    
    libraryDependencies += "org.apache.tinkerpop" % "gremlin-core" % "3.4.1",
    libraryDependencies += "com.michaelpollmeier" %% "gremlin-scala" % "3.4.1.6", // tinkerpop  3.4.1 6=latest scala DSL wrt this tinkerpop
    libraryDependencies += "org.apache.tinkerpop" % "tinkergraph-gremlin" % "3.4.1",  // the inmemory DB  
    // in lib: libraryDependencies += "nl.dgl.romix.akka" % "akka-kryo-serialization-shaded_2.12" % "0.5.3-SNAPSHOT"
    
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
  )
  
