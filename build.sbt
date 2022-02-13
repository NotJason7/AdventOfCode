val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Advent-of-Code",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.9" % "test",
      "org.scalanlp" %% "breeze" % "2.0.1-RC1",
      "org.scalanlp" %% "breeze-viz" % "2.0.1-RC1",
//      "org.creativescala" %% "doodle" % "0.10.1"
    )
  )
