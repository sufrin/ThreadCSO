//
// to build all the cso library module jar files
//      sbt20 clean test package
// to make a single jar file from the module jars (but not the examples)
//      sh scripts/onejarfile
//
// NB: The IntelliJ project structure includes two artefacts; one for the
//     substantive modules, and one for the examples
//

ThisBuild / organization := "ox"
Global / resolvers += "scala-integration" at
  "https://scala-ci.typesafe.com/artifactory/scala-integration/"
ThisBuild / scalaVersion := "2.13.11"
ThisBuild / version := "2.0.0"
ThisBuild / fork := true
ThisBuild / javaOptions ++= Seq("--enable-preview") // when running
ThisBuild / javacOptions ++= Seq("--enable-preview", "--release", "14") // when compiling,

lazy val scalaReflect = Def.setting {
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
}

lazy val root = (project in file("."))
   .aggregate(app, core, csomacros, net, msgpack, logging, app)

lazy val core = (project in file("core"))
  .dependsOn(csomacros)
  .dependsOn(app % "test")
  .settings(
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked"
      /* "-Werror" */
    ),
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.15",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"
  )



lazy val net = (project in file("net"))
  .dependsOn(csomacros)
  .dependsOn(core)
  .dependsOn(msgpack)
  .dependsOn(app)
  .dependsOn(logging) // for Logging at present: refactor this
  .settings(
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked"
      /* "-Werror" */
    )
  )

lazy val msgpack = (project in file("msgpack"))
  .dependsOn(logging) // for Logging at present: refactor this
  .settings(
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked"
      /* "-Werror" */
    )
  )

lazy val logging = (project in file("logging"))
 .dependsOn(csomacros)
 .dependsOn(core)
 .dependsOn(app)
 .settings(
   scalacOptions ++= Seq(
     "-deprecation",
     "-unchecked"
     /* "-Werror" */
   )
 )
  
lazy val csomacros = (project in file("macros"))
  .settings(
    libraryDependencies += scalaReflect.value
  )

lazy val app = (project in file("app"))
  .settings(
    libraryDependencies += scalaReflect.value
  )
  
lazy val examples = (project in file("examples"))
  .dependsOn(core)
  .dependsOn(app)
  .settings(
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked"
      /* "-Werror" */
    ),
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.15",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"
  )

lazy val manualtests = (project in file("manualtests"))
  .dependsOn(core)
  .dependsOn(app)
  .settings(
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked"
      /* "-Werror" */
    ),
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.15",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"
  )