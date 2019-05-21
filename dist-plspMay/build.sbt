name := "plspMay"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"

lazy val root = (project in file(".")).
  settings(
    mainClass in assembly := Some("plspSolver"),
    assemblyJarName in assembly := "199x223x_simple.jar", // 学籍番号を入れて下さい
    name := "plspSolver"
  )

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case _ => MergeStrategy.first
}