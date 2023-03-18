//
// to build all the jar files
// ./cso -sbt clean test package
//

ThisBuild / organization := "ox"
Global / resolvers += "scala-integration" at
  "https://scala-ci.typesafe.com/artifactory/scala-integration/"
ThisBuild / scalaVersion := "2.13.11-bin-36da1b3"
ThisBuild / version := "0.1.1-SNAPSHOT"
ThisBuild / fork := true
ThisBuild / javaOptions ++= Seq("--enable-preview") // when running
ThisBuild / javacOptions ++= Seq(
    "--enable-preview", 
    "--release", 
    "14"
  ) // when compiling,

lazy val scalaReflect = Def.setting {
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
}

lazy val core = (project in file("core"))
  .dependsOn(macroSub)
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

lazy val macroSub = (project in file("macros"))
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
