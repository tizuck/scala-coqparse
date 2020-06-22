name := "scala-coqparse"

version := "0.1"

scalaVersion := "2.12.7"

resolvers += "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven"

val ParsebackVersion = "0.3"
val kiamaVersion = "2.2.0"

libraryDependencies ++=
  Seq(
    "org.bitbucket.inkytonik.kiama" %% "kiama" % kiamaVersion,
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
    "com.codecommit" %% "parseback-cats" % ParsebackVersion,
    "com.codecommit" %% "parseback-core" % ParsebackVersion
  )