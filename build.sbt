lazy val rio = (project in file("."))
  .settings(
    name := "rio",
    version := "1.2",
    description := "A reasoner for Input/Output logics",
    organization := "net.aurelee",
    scalaVersion := "2.13.8",
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
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.13" % "test",
    libraryDependencies += "net.java.dev.jna" % "jna" % "5.7.0",
    libraryDependencies += "io.github.leoprover" %% "scala-tptp-parser" % "1.6.4"
  )
