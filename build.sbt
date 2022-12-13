val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "Advent-of-Code",
    version      := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalatest"     %% "scalatest"    % "3.2.9" % "test",
      "org.creativescala" %% "doodle"       % "0.10.1",
      "org.typelevel"     %% "cats-effect"  % "3.3.14",
      "co.fs2"            %% "fs2-core"     % "3.2.10",
      "co.fs2"            %% "fs2-io"       % "3.2.10",
      "io.circe"          %% "circe-core"   % "0.14.3",
      "io.circe"          %% "circe-parser" % "0.14.1"
    )
  )
