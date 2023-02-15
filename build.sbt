lazy val rio = (project in file("."))
  .settings(
    name := "rio",
    version := "1.2",
    description := "A reasoner for Input/Output logics",
    organization := "net.aurelee",
    scalaVersion := "2.13.10",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-target:11",
    ),
    Compile/unmanagedResourceDirectories += baseDirectory.value / "contrib",
    Compile/mainClass  := Some("net.aurelee.rio.Main"),
    assembly/mainClass := Some("net.aurelee.rio.Main"),
    assembly/assemblyJarName := s"${name.value}-${version.value}.jar",

    licenses += "BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test",
    libraryDependencies += "net.java.dev.jna" % "jna" % "5.12.1",
    libraryDependencies += "io.github.leoprover" %% "scala-tptp-parser" % "1.6.5",
  )
