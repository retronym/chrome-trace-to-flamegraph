scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.core" % "jackson-core" % "2.9.5",
  "com.fasterxml.jackson.core" % "jackson-annotations" % "2.9.5",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.9.5",
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % "2.9.5",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.5"
)

name := "chrome-trace-to-flamegraph"

organization := "io.github.retronym"
