lazy val root = (project in file(".")).dependsOn(RootProject(file("../sbt-plugin")))

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1")
