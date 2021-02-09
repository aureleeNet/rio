lazy val parserLib = ProjectRef(uri("git://github.com/leoprover/scala-tptp-parser#v1.2"), "tptpParser")

lazy val rio = (project in file("."))
  .settings(
    name := "rio",
    version := "0.1",
    description := "A reasoner for Input/Output logics",
    organization := "net.aurelee",
    scalaVersion := "2.13.4",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-target:", "11"
    ),
    licenses += "BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
  ).dependsOn(parserLib)
