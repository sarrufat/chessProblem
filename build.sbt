

 val scalacheck = "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"
 val scalatest = "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

lazy val commonSettings = Seq(
  organization := "org.sarrufat",
  version := "0.1.0",
  scalaVersion := "2.11.6"
)


lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "ChessChallenge",
    libraryDependencies += scalacheck,
     libraryDependencies += scalatest
  )

