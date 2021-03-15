scalaVersion := "2.13.5"

val circeVersion = "0.13.0"
val catsVersion = "2.2.0"
val scalaTestVersion = "3.1.0.0-RC2"

logBuffered in Test := false

libraryDependencies ++= Seq(
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion,
  "org.typelevel" %% "cats-core" % catsVersion,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalacheck" %% "scalacheck" % "1.14.1",
  "org.scalaj" %% "scalaj-http" % "2.4.2"
)
