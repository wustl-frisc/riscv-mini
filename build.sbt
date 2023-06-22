ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "2.5.0"
ThisBuild / organization     := "edu.berkeley.cs"

val chiselVersion = "3.5.6"

lazy val root = (project in file("."))
  .settings(
    name := "riscv-mini",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % "0.5.6" % "test",
      "edu.wustl.sbs" %% "foam" % "1.0-SNAPSHOT",
      "edu.wustl.sbs" %% "faust" % "2.0-SNAPSHOT"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
  )

lazy val aspects = (project in file("aspects"))
  .settings(
    libraryDependencies ++= Seq(
      "edu.wustl.sbs" %% "foam" % "1.0-SNAPSHOT",
      "edu.wustl.sbs" %% "faust" % "2.0-SNAPSHOT"
    )
  ).dependsOn(root)
