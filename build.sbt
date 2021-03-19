scalaVersion := "2.13.4"

val scalaTestVersion = "3.1.4"

libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"

logBuffered in Test := false
coverageEnabled := true
