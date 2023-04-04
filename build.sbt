val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "skavin",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.typelevel" %% "cats-core" % "2.8.0"
    )
  )
