ThisBuild / scalaVersion := "2.13.11"
ThisBuild / fork := true
ThisBuild / javaOptions ++= Seq("--enable-preview") // when running
ThisBuild / javacOptions ++= Seq("--enable-preview", "--release", "14") // when compiling,

Compile / PB.targets := Seq(
  scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
)

// (optional) If you need scalapb/scalapb.proto or anything from
// google/protobuf/*.proto
libraryDependencies ++= Seq(
    "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
)
