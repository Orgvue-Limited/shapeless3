lazy val settings =
  Seq(
    organization := "com.orgvue",
    scalaVersion := "3.3.1",
    version := "0.1.0",
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
  )

lazy val pureconfigDependency = "com.github.pureconfig" %% "pureconfig-generic-scala3" % "0.17.7"

lazy val core =
  project
    .settings(settings)
    .settings(moduleName := "shapeless3-core")

lazy val pureconfig =
  project
    .settings(settings)
    .settings(moduleName := "shapeless3-pureconfig")
    .settings(libraryDependencies ++= Seq(pureconfigDependency))
    .dependsOn(core)