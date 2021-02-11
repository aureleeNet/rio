lazy val parserLib = ProjectRef(uri("git://github.com/leoprover/scala-tptp-parser#v1.2"), "tptpParser")

lazy val rio = (project in file("."))
  .settings(
    name := "rio",
    version := "1.0",
    description := "A reasoner for Input/Output logics",
    organization := "net.aurelee",
    scalaVersion := "2.13.4",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-target:11",
    ),
    unmanagedResourceDirectories in Compile += baseDirectory.value / "contrib",
    
    Compile/mainClass  := Some("net.aurelee.rio.Main"),
    assembly/mainClass := Some("net.aurelee.rio.Main"),
    
    test in assembly := {},
    assemblyJarName in assembly := s"${name.value}-${version.value}.jar",
    
    licenses += "BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test",
    libraryDependencies += "net.java.dev.jna" % "jna" % "5.7.0"
  ).dependsOn(parserLib)
