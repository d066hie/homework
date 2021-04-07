scalaVersion := "2.13.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.3" % "test"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.2.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.2.0"

logBuffered in Test := false
