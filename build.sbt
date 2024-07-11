ThisBuild / organization := "com.orgvue"
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / version := "0.1.0"

scalacOptions := Seq(
  "-deprecation",
  "-encoding",
  "utf-8",
  "-feature",
  "-explain-types",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "check-init",
  "-indent",
  "-rewrite"
)

lazy val root = (project in file("."))
  .settings(
    name := "shapeless3"
  )